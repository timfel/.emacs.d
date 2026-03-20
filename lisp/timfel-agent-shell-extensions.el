;;; timfel-agent-shell-extensions.el --- Agent-shell helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Local helpers for orchestrating `agent-shell' shell buffers.

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'seq)
(require 'subr-x)
(require 'timfel)
(require 'agent-shell)
(require 'timfel-agent-shell-bwrap)
(require 'timfel-agent-shell-recovery)
(require 'timfel-agent-shell-ui)
(require 'timfel-agent-shell-worktrees)

(defcustom timfel/agent-shell-planning-request
  "Go into planning mode"
  "Initial request queued before each fan-out agent task."
  :type 'string
  :group 'timfel)

(defun timfel/agent-shell--buffer-for-directory (directory)
  "Return the live `agent-shell' buffer rooted at DIRECTORY, or nil."
  (let ((root (file-name-as-directory (expand-file-name directory))))
    (seq-find (lambda (buffer)
                (with-current-buffer buffer
                  (string=
                   (file-name-as-directory
                    (expand-file-name default-directory))
                   root)))
              (agent-shell-buffers))))

(defun timfel/agent-shell--start-shell-in-directory (title directory startup-mode)
  "Start or reuse an `agent-shell' rooted at DIRECTORY."
  (if-let* ((config (or (agent-shell--resolve-preferred-config)
                        (user-error
                         "No preferred agent-shell config is available")))
            (shell-buffer (or (timfel/agent-shell--buffer-for-directory directory)
                              (agent-shell--start :config config
                                                  :no-focus t
                                                  :session-strategy startup-mode))))
      (progn
        (when (and title (not (string-empty-p title)))
          (with-current-buffer shell-buffer
            (cl-letf (((symbol-function 'read-string)
                       (lambda (&rest _) title)))
              (call-interactively #'agent-shell-rename-buffer))))
        shell-buffer)))

(defun timfel/agent-shell--dired-marked-directories ()
  "Return normalized marked directories from the current Dired buffer."
  (unless (derived-mode-p 'dired-mode)
    (user-error "Current buffer is not a Dired buffer"))
  (let ((directories
         (seq-uniq
          (mapcar (lambda (path)
                    (file-name-as-directory (expand-file-name path)))
                  (dired-get-marked-files nil nil #'file-directory-p))
          #'string=)))
    (unless directories
      (user-error "No marked directories in %s" (buffer-name)))
    directories))

(defun timfel/agent-shell--normalize-task-specs (task-specs)
  "Normalize TASK-SPECS into plists with `:title', `:task', and `:directory'."
  (mapcar
   (lambda (spec)
     (cond
      ((stringp spec)
       (list :title spec :task spec))
      ((and (consp spec)
            (stringp (car spec))
            (stringp (cdr spec)))
       (list :title (car spec) :task (cdr spec)))
      ((and (listp spec)
            (plist-member spec :directory)
            (stringp (plist-get spec :directory)))
       (let ((directory (file-name-as-directory
                         (expand-file-name (plist-get spec :directory)))))
         (list :title (plist-get spec :title)
               :task (plist-get spec :task)
               :directory directory)))
      (t
       (user-error "Invalid task spec: %S" spec))))
   task-specs))


(defun timfel/agent-shell--queue-startup-requests (shell-buffer task)
  "Queue planning mode in SHELL-BUFFER before TASK, then start processing.

Use this only for brand new sessions so resumed sessions keep their existing
conversation state untouched."
  (when (and task (not (string-empty-p task)))
    (with-current-buffer shell-buffer
      (unless (derived-mode-p 'agent-shell-mode)
        (error "Not in an agent-shell buffer: %s" (buffer-name shell-buffer)))
      (unless (fboundp 'agent-shell--enqueue-request)
        (error "agent-shell does not expose its request queue helper"))
      (agent-shell--enqueue-request :prompt timfel/agent-shell-planning-request)
      (agent-shell--enqueue-request :prompt task)
      (unless (shell-maker-busy)
        (agent-shell-resume-pending-requests))
      t)))

;;;###autoload
(defun timfel/dired-agent-shell-marked-directories ()
  "Interactively start or resume `agent-shell' for each marked Dired directory.

For each marked directory in the current Dired buffer, reuse the shared
fan-out helper so each directory waits for its own session-selection prompt
before moving on to the next one."
  (interactive)
  (unless (require 'agent-shell nil t)
    (user-error "agent-shell is not installed"))
  (let ((directories (timfel/agent-shell--dired-marked-directories)))
    (timfel/agent-shell-fan-out-worktrees
     (mapcar (lambda (directory)
               (list :directory directory))
             directories))))

;;;###autoload
(defun timfel/agent-shell-fan-out-worktrees (task-specs &optional directory)
  "Create one worktree-backed `agent-shell' per entry in TASK-SPECS.

TASK-SPECS may be a list of strings, an alist of `(TITLE . TASK)' pairs, or
plists containing `:directory' plus optional `:title' and `:task'. Specs with
`:directory' start or resume shells in those existing directories. Other specs
create or reuse worktrees below DIRECTORY's Git repository, rename each shell
buffer to TITLE, and queue TASK. When DIRECTORY is nil, use
`default-directory'.

Interactively, prompt for the number of worktrees, then read a TITLE and TASK
for each one. Return a plist for each started shell with `:title', `:task',
`:worktree', and `:buffer' keys."
  (interactive
   (list
    (let* ((count (read-number "Number of worktrees/tasks: " 2))
           (task-specs
            (cl-loop for index from 1 to count
                     for title =
                     (string-trim
                      (read-from-minibuffer (format "Title %d: " index)))
                     for task =
                     (string-trim
                      (read-from-minibuffer (format "Task %d: " index) nil nil nil nil title))
                     unless (or (string-empty-p title)
                                (string-empty-p task))
                     collect (cons title task))))
      (unless task-specs
        (user-error "Need at least one non-empty title/task pair"))
      task-specs)
    default-directory))
  (let* ((task-specs (timfel/agent-shell--normalize-task-specs task-specs))
         (needs-repo-root (seq-some (lambda (spec)
                                      (null (plist-get spec :directory)))
                                    task-specs))
         (repo-root (when needs-repo-root
                      (timfel/agent-shell-worktrees-git-root directory))))
    (when (and needs-repo-root (not repo-root))
      (user-error "Not inside a git repository: %s"
                  (expand-file-name (or directory default-directory))))
    (cl-loop for spec in task-specs
             do
             (let* ((title (plist-get spec :title))
                    (task (plist-get spec :task))
                    (worktree-dir
                     (or (plist-get spec :directory)
                         (timfel/agent-shell-worktrees-create repo-root title)))
                    (has-transcripts
                     (file-expand-wildcards
                      (expand-file-name ".agent-shell/transcripts/*.md" worktree-dir)))
                    (startup-mode
                     (if (and has-transcripts
                              (y-or-n-p (format "Resume session in %s" worktree-dir)))
                         'latest
                       'new))
                    (shell-buffer
                     (timfel/agent-shell--start-shell-in-directory
                      title worktree-dir startup-mode)))
               (when (eq startup-mode 'new)
                 (timfel/agent-shell--queue-startup-requests shell-buffer task))))))

(provide 'timfel-agent-shell-extensions)

;;; timfel-agent-shell-extensions.el ends here
