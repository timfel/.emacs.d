;;; timfel-agent-shell-fanout.el --- Agent-shell session fan-out -*- lexical-binding: t -*-

;;; Commentary:

;; Start or resume multiple agent-shell sessions from task specs.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'timfel)
(require 'agent-shell)

(defcustom timfel/agent-shell-planning-request
  "Go into planning mode"
  "Initial request queued before each fan-out agent task."
  :type 'string
  :group 'timfel)

(defun timfel/agent-shell--worktrees-create (repo-root title)
  "Create or reuse an agent-shell worktree below REPO-ROOT for TITLE."
  (let* ((repo-root (expand-file-name repo-root))
         (repo-name (file-name-nondirectory (directory-file-name repo-root)))
         (mx-dir (expand-file-name (format "mx.%s" repo-name) repo-root))
         (repo-roots (cons repo-root
                           (when (file-directory-p mx-dir)
                             (thread-last
                               '("graal" "graal-enterprise")
                               (seq-map (lambda (p) (expand-file-name (concat "../" p) repo-root)))
                               (seq-filter #'file-directory-p)
                               (seq-filter (lambda (p) (not (file-equal-p repo-root p))))))))
         (default-directory repo-root)
         (transcript-dir (funcall agent-shell-transcript-file-path-function))
         (base-dir (file-name-concat
                    (file-name-parent-directory
                     (file-name-parent-directory transcript-dir))
                    "worktrees"))
         (slug (thread-last
                 title
                 (downcase)
                 (replace-regexp-in-string "[^[:alnum:]]+" "-")
                 (replace-regexp-in-string "\\`-+\\|-+\\'" "")))
         (slug (if (string-empty-p slug) "task" slug)))

    (if-let ((created-worktrees (timfel/agent-shell--worktrees-create-with-suffix repo-roots base-dir slug nil)))
        (cdr (car created-worktrees)))))

(defun timfel/agent-shell--worktree-create (git-root parent-folder branch)
  "Create a worktree and BRANCH from GIT-ROOT under PARENT-FOLDER with a
filename matching the GIT-ROOT final name.

Return new worktree-dir on success, nil on failure."
  (let ((worktree-dir (expand-file-name
                       (file-name-nondirectory (directory-file-name git-root))
                       parent-folder)))
    (make-directory parent-folder t)
    (let ((default-directory (file-name-as-directory git-root)))
      ;; prune old worktrees first
      (process-file "git" nil nil nil "worktree" "prune")
      ;; then make the new worktree
      (if (zerop (process-file "git" nil nil nil "worktree" "add" "-b" branch worktree-dir "HEAD"))
          worktree-dir
        ;; if creating the worktree failed, let's check why
        (if (file-exists-p worktree-dir)
            ;; worktree exists, so if no agent buffers are currently using it,
            ;; then we can re-use it
            (unless (seq-some (lambda (b)
                                (file-in-directory-p
                                 (with-current-buffer b default-directory)
                                 worktree-dir))
                              (agent-shell-buffers))
              worktree-dir))))))

(defun timfel/agent-shell--worktrees-create-with-suffix (repo-roots base-dir slug suffix)
  (let* ((parent-folder-for-worktrees
          (expand-file-name (if suffix (format "%s-%02d" slug suffix) slug) base-dir))
         (branch
          (format "agent-shell/%s"
                  (file-name-nondirectory (directory-file-name parent-folder-for-worktrees)))))

    ;; try creating and allocating. if anything fails, undo what we alreday
    ;; did and skip to the next suffix
    (let ((created-worktrees
           (seq-filter #'cdr
                       (seq-map (lambda (repo-root)
                                  (cons repo-root (timfel/agent-shell--worktree-create repo-root parent-folder-for-worktrees branch)))
                                repo-roots))))
      (if (= (seq-length created-worktrees) (seq-length repo-roots))
          ;; ok, we got all our worktrees done, return
          created-worktrees
        ;; at least one worktree was not created, so undo anything we did and move on
        (mapc (lambda (repo-root-and-new-worktree-dir)
                (let ((default-directory (car repo-root-and-new-worktree-dir)))
                  (process-file "git" nil nil nil "worktree" "remove" (cdr repo-root-and-new-worktree-dir))
                  (process-file "git" nil nil nil "branch" "-d" branch)))
              created-worktrees)
        (timfel/agent-shell--worktrees-create-with-suffix repo-roots base-dir slug (1+ (or suffix 0)))))))

;;;###autoload
(defun timfel/agent-shell-fan-out-worktrees (task-specs &optional directory)
  "Create one worktree-backed `agent-shell' per entry in TASK-SPECS.

TASK-SPECS is an alist of `(TITLE . TASK)' pairs. If a TITLE is an
absolute directory path, the final element of the path is taken as TITLE
and the working directory is the absolute path. Other specs create or
reuse worktrees below DIRECTORY's Git repository, rename each shell
buffer to TITLE, and queue TASK. When DIRECTORY is nil, use
`default-directory'."
  (let* ((titles (mapcar #'car task-specs))
         (needs-repo-root (not (seq-every-p #'file-name-absolute-p titles)))
         (directory (file-name-as-directory (expand-file-name (or directory default-directory))))
         (agent-shell-session-strategy 'latest)
         (config (copy-alist (agent-shell--resolve-preferred-config)))
         (repo-root (when needs-repo-root
                      (let ((default-directory directory))
                        (with-temp-buffer
                          (when (zerop (process-file "git" nil t nil "rev-parse" "--show-toplevel"))
                            (string-trim (buffer-string))))))))

    (when (and needs-repo-root (not repo-root))
      (user-error "Not inside a git repository: %s" directory))

    (when (seq-some #'string-blank-p titles)
      (user-error "Empty title for fanout: %s" titles))

    (unless config
      (user-error "No preferred agent-shell config is available"))

    ;; they consult it asynchronously, so we have not a chance
    (setq agent-shell-session-strategy 'latest)

    (cl-loop for (title-or-dir . task) in task-specs
             do
             (let* ((title-is-dir (file-name-absolute-p title-or-dir))
                    (title (if title-is-dir
                               (string-join (last (split-string (directory-file-name title-or-dir) "/" t) 2) "-")
                             title-or-dir))
                    (worktree-dir (if title-is-dir
                                      title-or-dir
                                    (timfel/agent-shell--worktrees-create repo-root title)))
                    (config (copy-alist config))
                    (default-directory worktree-dir)
                    (prev-transcripts (ignore-errors (directory-files
                                                      (file-name-parent-directory (funcall agent-shell-transcript-file-path-function))
                                                      nil "\\.md$"))))
               (setf (alist-get :buffer-name config) (concat title " agent"))
               (when-let ((shell-buffer (agent-shell-start :config config)))
                 (shell-maker-set-buffer-name shell-buffer (alist-get :buffer-name config))
                 (with-current-buffer shell-buffer
                   (setq-local timfel/agent-shell-worktree-parent (file-name-parent-directory worktree-dir)))
                 (when (and task (not (string-blank-p task)) (not prev-transcripts))
                   (with-current-buffer shell-buffer
                     (agent-shell-queue-request timfel/agent-shell-planning-request)
                     (agent-shell-queue-request task))))))))

(defun timfel/agent-shell-cleanup-worktree ()
  (interactive)
  (let ((worktree-parent nil))
    (when (boundp 'timfel/agent-shell-worktree-parent)
      (setq worktree-parent timfel/agent-shell-worktree-parent)
      (kill-buffer))
    (if (and worktree-parent
             (yes-or-no-p (format "Delete %s? " worktree-parent)))
        (delete-directory worktree-parent t nil))))

(provide 'timfel-agent-shell-fanout)

;;; timfel-agent-shell-fanout.el ends here
