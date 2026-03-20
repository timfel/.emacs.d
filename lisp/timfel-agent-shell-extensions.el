;;; timfel-agent-shell-extensions.el --- Agent-shell helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Local helpers for orchestrating `agent-shell' worktrees and shell buffers.

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'seq)
(require 'subr-x)
(require 'timfel)
(require 'agent-shell)

(defcustom timfel/agent-shell-worktree-subdirectory
  ".agent-shell/worktrees"
  "Relative directory under a git repository for agent-shell worktrees."
  :type 'string
  :group 'timfel)

(defcustom timfel/agent-shell-planning-request
  "Go into planning mode"
  "Initial request queued before each fan-out agent task."
  :type 'string
  :group 'timfel)

(defvar timfel/dired-agent-shell-idle-delay 2
  "Seconds of Emacs idle time before advancing to the next marked directory.")

(defvar timfel/agent-shell-recover-live-set--inhibit-save nil)

;;;###autoload
(defun timfel/agent-shell-recover-live-set (&optional omit-buffer)
  "Restore or refresh the saved live set of `agent-shell' directories.

When called interactively, reopen each directory saved in
`~/.emacs.d/.agent-shell/live-agent-shell-set.el' with `agent-shell'.

When called from `agent-shell-mode-hook', save the current live set and install
a buffer-local `kill-buffer-hook' that rewrites the snapshot while omitting the
buffer being killed."
  (interactive)
  (let ((state-file (locate-user-emacs-file ".agent-shell/live-agent-shell-set.el")))
    (if (called-interactively-p 'interactive)
        (let ((directories
               (when (file-readable-p state-file)
                 (with-temp-buffer
                   (insert-file-contents state-file)
                   (read (current-buffer)))))
              (restored 0))
          (unless directories
            (user-error "No saved live agent-shell set"))
          (let ((timfel/agent-shell-recover-live-set--inhibit-save t))
            (dolist (directory directories)
              (when (y-or-n-p (format "Restore agent-shell for %s? " directory))
                (unless (seq-some
                         (lambda (buffer)
                           (with-current-buffer buffer
                             (string=
                              (file-name-as-directory (expand-file-name default-directory))
                              (file-name-as-directory (expand-file-name directory)))))
                         (agent-shell-buffers))
                  (let ((default-directory directory)
                        (agent-shell-session-strategy 'prompt))
                    (call-interactively #'agent-shell)))
                (setq restored (1+ restored)))))
          (timfel/agent-shell-recover-live-set)
          (message "Recovered %d agent-shell director%s"
                   restored
                   (if (= restored 1) "y" "ies")))
      (when (derived-mode-p 'agent-shell-mode)
        (add-hook 'kill-buffer-hook
                  (lambda ()
                    (timfel/agent-shell-recover-live-set (current-buffer)))
                  nil t))
      (unless timfel/agent-shell-recover-live-set--inhibit-save
        (let ((directories
               (sort
                (seq-uniq
                 (delq nil
                       (mapcar
                        (lambda (buffer)
                          (unless (eq buffer omit-buffer)
                            (with-current-buffer buffer
                              (and (derived-mode-p 'agent-shell-mode)
                                   default-directory
                                   (file-name-as-directory
                                    (expand-file-name default-directory))))))
                        (agent-shell-buffers)))
                 #'string=)
                #'string-lessp)))
          (make-directory (file-name-directory state-file) t)
          (with-temp-file state-file
            (let ((print-length nil)
                  (print-level nil))
              (prin1 directories (current-buffer))
              (insert "\n"))))))))

(defun timfel/agent-shell--git-common-root (&optional directory)
  "Return the git common root for DIRECTORY, or nil when unavailable."
  (let ((default-directory
         (file-name-as-directory
          (expand-file-name (or directory default-directory)))))
    (with-temp-buffer
      (when (zerop (process-file "git" nil t nil
                                 "rev-parse" "--git-common-dir"))
        (let ((gitdir (string-trim (buffer-string))))
          (unless (string-empty-p gitdir)
            (file-name-directory
             (directory-file-name
              (expand-file-name gitdir default-directory)))))))))

(defun timfel/agent-shell--bwrap-bind-args (mode path)
  "Return bubblewrap bind arguments for PATH using MODE when PATH exists."
  (when (file-exists-p path)
    (list mode path path)))

(defun timfel/agent-shell--git-root (&optional directory)
  "Return the git root for DIRECTORY, or nil when outside Git."
  (let ((default-directory
         (file-name-as-directory
          (expand-file-name (or directory default-directory)))))
    (with-temp-buffer
      (when (zerop (process-file "git" nil t nil
                                 "rev-parse" "--show-toplevel"))
        (string-trim (buffer-string))))))

(defun timfel/agent-shell--slugify (text)
  "Convert TEXT into a short filesystem-safe slug."
  (let* ((slug (downcase text))
         (slug (replace-regexp-in-string "[^[:alnum:]]+" "-" slug))
         (slug (replace-regexp-in-string "\\`-+\\|-+\\'" "" slug)))
    (if (string-empty-p slug)
        "task"
      (substring slug 0 (min 24 (length slug))))))

(defun timfel/agent-shell--repo-name (repo-root)
  "Return the directory name for REPO-ROOT."
  (file-name-nondirectory
   (directory-file-name (expand-file-name repo-root))))

(defun timfel/agent-shell--worktree-name (title)
  "Create the base worktree directory name for TITLE."
  (timfel/agent-shell--slugify title))

(defun timfel/agent-shell--worktree-parent (repo-root title &optional suffix)
  "Return the per-task parent directory for REPO-ROOT, TITLE, and SUFFIX."
  (let* ((base-dir (expand-file-name timfel/agent-shell-worktree-subdirectory
                                     repo-root))
         (base-name (timfel/agent-shell--worktree-name title))
         (name (if suffix
                   (format "%s-%02d" base-name suffix)
                 base-name)))
    (expand-file-name name base-dir)))

(defun timfel/agent-shell--worktree-branch (worktree-parent)
  "Return the agent-shell branch name for WORKTREE-PARENT."
  (format "agent-shell/%s"
          (file-name-nondirectory
           (directory-file-name worktree-parent))))

(defun timfel/agent-shell--mx-linked-sibling-repos (repo-root)
  "Return extra repo roots that should get sibling worktrees with REPO-ROOT.

If REPO-ROOT looks like an mx suite checkout by containing `mx.<repo-name>',
include sibling checkouts named `graal' and `graal-enterprise' when present."
  (let* ((repo-root (expand-file-name repo-root))
         (repo-name (timfel/agent-shell--repo-name repo-root))
         (mx-dir (expand-file-name (format "mx.%s" repo-name) repo-root)))
    (when (file-directory-p mx-dir)
      (seq-filter
       (lambda (sibling)
         (and (file-directory-p sibling)
              (not (file-equal-p repo-root sibling))))
       (mapcar (lambda (name)
                 (expand-file-name (concat "../" name) repo-root))
               '("graal" "graal-enterprise"))))))

(defun timfel/agent-shell--git-branch-exists-p (repo-root branch)
  "Return non-nil when BRANCH already exists in REPO-ROOT."
  (let ((default-directory (file-name-as-directory (expand-file-name repo-root))))
    (zerop (process-file "git" nil nil nil
                         "show-ref" "--verify" "--quiet"
                         (format "refs/heads/%s" branch)))))

(defun timfel/agent-shell--prune-missing-worktrees (repo-root)
  "Prune registered worktrees in REPO-ROOT whose directories are missing."
  (let ((default-directory (file-name-as-directory (expand-file-name repo-root)))
        (missing-worktrees nil))
    (with-temp-buffer
      (unless (zerop (process-file "git" nil t nil "worktree" "list" "--porcelain"))
        (user-error "Failed to list worktrees for %s: %s"
                    repo-root
                    (string-trim (buffer-string))))
      (goto-char (point-min))
      (while (re-search-forward "^worktree \(.+\)$" nil t)
        (let ((worktree-dir (match-string 1)))
          (unless (file-directory-p worktree-dir)
            (push worktree-dir missing-worktrees)))))
    (when missing-worktrees
      (with-temp-buffer
        (unless (zerop (process-file "git" nil t nil "worktree" "prune"))
          (user-error "Failed to prune missing worktrees for %s: %s"
                      repo-root
                      (string-trim (buffer-string))))))
    missing-worktrees))

(defun timfel/agent-shell--registered-worktrees (repo-root)
  "Return registered Git worktrees for REPO-ROOT as plists."
  (let ((default-directory (file-name-as-directory (expand-file-name repo-root)))
        entries
        current)
    (with-temp-buffer
      (unless (zerop (process-file "git" nil t nil "worktree" "list" "--porcelain"))
        (user-error "Failed to list worktrees for %s: %s"
                    repo-root
                    (string-trim (buffer-string))))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (cond
           ((string-prefix-p "worktree " line)
            (when current
              (push current entries))
            (setq current (list :path (string-remove-prefix "worktree " line))))
           ((and current (string-prefix-p "branch " line))
            (setq current (plist-put current :branch
                                     (string-remove-prefix "branch " line))))))
        (forward-line 1)))
    (when current
      (push current entries))
    (nreverse entries)))

(defun timfel/agent-shell--normalize-branch-ref (branch)
  "Return BRANCH without a leading `refs/heads/' prefix."
  (string-remove-prefix "refs/heads/" branch))

(defun timfel/agent-shell--registered-worktree-for-branch (repo-root branch)
  "Return the registered worktree path for BRANCH in REPO-ROOT, or nil."
  (let ((branch-ref (format "refs/heads/%s" branch)))
    (seq-some (lambda (entry)
                (when (equal (plist-get entry :branch) branch-ref)
                  (plist-get entry :path)))
              (timfel/agent-shell--registered-worktrees repo-root))))

(defun timfel/agent-shell--registered-worktree-at-path (repo-root worktree-dir)
  "Return the registered worktree entry for WORKTREE-DIR in REPO-ROOT, or nil."
  (let ((target (file-name-as-directory (expand-file-name worktree-dir))))
    (seq-some (lambda (entry)
                (when (string= (file-name-as-directory
                                (expand-file-name (plist-get entry :path)))
                               target)
                  entry))
              (timfel/agent-shell--registered-worktrees repo-root))))

(defun timfel/agent-shell--directory-prefix-p (directory path)
  "Return non-nil when PATH is DIRECTORY or a descendant of DIRECTORY."
  (let ((directory (file-name-as-directory (expand-file-name directory)))
        (path (and path (file-name-as-directory (expand-file-name path)))))
    (and path (string-prefix-p directory path))))

(defun timfel/agent-shell--acp-buffer-p (buffer)
  "Return non-nil when BUFFER looks like an ACP helper buffer."
  (let ((name (buffer-name buffer)))
    (or (string-match-p "\`\*acp-.*\(log\|traffic\)\*\'" name)
        (string-match-p "\`acp-client-stderr(" name))))

(defun timfel/agent-shell--directory-associated-buffers (directory)
  "Return live agent-shell or ACP buffers associated with DIRECTORY."
  (seq-filter
   (lambda (buffer)
     (and (buffer-live-p buffer)
          (with-current-buffer buffer
            (and default-directory
                 (timfel/agent-shell--directory-prefix-p directory default-directory)
                 (or (derived-mode-p 'agent-shell-mode)
                     (timfel/agent-shell--acp-buffer-p buffer))))))
   (buffer-list)))

(defun timfel/agent-shell--process-directory (process)
  "Return a best-effort working directory for PROCESS, or nil."
  (or (when-let ((buffer (process-buffer process)))
        (and (buffer-live-p buffer)
             (buffer-local-value 'default-directory buffer)))
      (let* ((command (process-command process))
             (chdir-pos (cl-position "--chdir" command :test #'string=)))
        (when chdir-pos
          (nth (1+ chdir-pos) command)))))

(defun timfel/agent-shell--directory-live-processes (directory)
  "Return live processes associated with DIRECTORY."
  (seq-filter
   (lambda (process)
     (and (process-live-p process)
          (timfel/agent-shell--directory-prefix-p
           directory
           (timfel/agent-shell--process-directory process))))
   (process-list)))

(defun timfel/agent-shell--worktree-reusable-p (directory)
  "Return non-nil when DIRECTORY has no associated live buffers or processes."
  (and (file-directory-p directory)
       (null (timfel/agent-shell--directory-associated-buffers directory))
       (null (timfel/agent-shell--directory-live-processes directory))))

(defun timfel/agent-shell--resolve-worktree-allocation (repo-root repo-roots title)
  "Resolve a worktree allocation for REPO-ROOT, REPO-ROOTS, TITLE.

Return a plist with `:branch', `:worktree-parent', `:primary-worktree', and
`:reuse' keys.  Reuse an existing worktree when every repo in REPO-ROOTS has a
registered worktree at the candidate path, all those worktrees share the same
branch, and none has live agent-shell, ACP, or process activity."
  (let ((suffix nil)
        (next-suffix 2))
    (catch 'allocation
      (while t
        (let* ((worktree-parent (timfel/agent-shell--worktree-parent
                                 repo-root title suffix))
               (expected-worktrees
                (mapcar (lambda (root)
                          (expand-file-name (timfel/agent-shell--repo-name root)
                                            worktree-parent))
                        repo-roots))
               (registered-entries
                (cl-mapcar #'timfel/agent-shell--registered-worktree-at-path
                           repo-roots expected-worktrees))
               (branches (delq nil
                               (mapcar (lambda (entry)
                                         (when-let ((branch (plist-get entry :branch)))
                                           (timfel/agent-shell--normalize-branch-ref branch)))
                                       registered-entries)))
               (branch (timfel/agent-shell--worktree-branch worktree-parent)))
          (cond
           ((and (seq-every-p #'identity registered-entries)
                 ;; (= (length (delete-dups (copy-sequence branches))) 1)
                 (seq-every-p #'timfel/agent-shell--worktree-reusable-p
                              expected-worktrees))
            (throw 'allocation
                   (list :branch (car branches)
                         :worktree-parent worktree-parent
                         :primary-worktree (car expected-worktrees)
                         :reuse t)))
           ((and (not (seq-some #'identity registered-entries))
                 (not (seq-some (lambda (root)
                                  (timfel/agent-shell--git-branch-exists-p root branch))
                                repo-roots))
                 (not (file-exists-p worktree-parent)))
            (throw 'allocation
                   (list :branch branch
                         :worktree-parent worktree-parent
                         :primary-worktree nil
                         :reuse nil))))
          (setq suffix next-suffix
                next-suffix (1+ next-suffix)))))))

(defun timfel/agent-shell--create-single-worktree (repo-root worktree-parent branch)
  "Create one Git worktree for REPO-ROOT below WORKTREE-PARENT on BRANCH."
  (let* ((repo-name (timfel/agent-shell--repo-name repo-root))
         (worktree-dir (expand-file-name repo-name worktree-parent)))
    (timfel/agent-shell--prune-missing-worktrees repo-root)
    (when (file-exists-p worktree-dir)
      (user-error "Worktree directory already exists: %s" worktree-dir))
    (make-directory worktree-parent t)
    (with-temp-buffer
      (let ((default-directory (file-name-as-directory repo-root)))
        (unless (zerop (process-file "git" nil t nil
                                     "worktree" "add"
                                     "-b" branch
                                     worktree-dir
                                     "HEAD"))
          (user-error "Failed to create worktree %s: %s"
                      worktree-dir
                      (string-trim (buffer-string))))))
    worktree-dir))

(defun timfel/agent-shell--create-worktree (repo-root title)
  "Create or reuse a Git worktree below REPO-ROOT for TITLE.

The primary worktree lives at:
  <repo-root>/<subdir>/<title-slug>/<repo-name>

If REPO-ROOT is an mx suite checkout with an `mx.<repo-name>' directory and a
sibling `graal' or `graal-enterprise' checkout exists, create sibling worktrees
under the same task parent directory as well.  When a matching branch already
has registered worktrees and none has live agent-shell, ACP, or process
activity, reuse those worktrees instead of allocating a suffixed branch name.
Return the primary worktree directory."
  (let* ((repo-roots (cons repo-root
                           (timfel/agent-shell--mx-linked-sibling-repos repo-root))))
    (dolist (root repo-roots)
      (timfel/agent-shell--prune-missing-worktrees root))
    (let* ((allocation (timfel/agent-shell--resolve-worktree-allocation
                        repo-root repo-roots title))
           (worktree-parent (plist-get allocation :worktree-parent))
           (branch (plist-get allocation :branch))
           (primary-worktree (plist-get allocation :primary-worktree)))
      (if (plist-get allocation :reuse)
          (progn
            (message "Reusing existing worktree %s on branch %s"
                     primary-worktree
                     branch)
            primary-worktree)
        (setq primary-worktree
              (timfel/agent-shell--create-single-worktree
               repo-root worktree-parent branch))
        (dolist (sibling-repo (cdr repo-roots))
          (timfel/agent-shell--create-single-worktree
           sibling-repo worktree-parent branch))
        primary-worktree))))

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
      (when (and title (not (string-empty-p title)))
        (with-current-buffer shell-buffer
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _) title)))
            (call-interactively #'agent-shell-rename-buffer))))))

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

(defun timfel/agent-shell--pause-until-idle ()
  "Pause the current command until Emacs has been idle for long enough."
  (let ((timer nil))
    (unwind-protect
        (progn
          (setq timer
                (run-with-idle-timer
                 timfel/dired-agent-shell-idle-delay nil
                 (lambda ()
                   (when (> (recursion-depth) 0)
                     (exit-recursive-edit)))))
          (recursive-edit))
      (when timer
        (cancel-timer timer)))))

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

(defun timfel/agent-shell--rename-shell-buffer (shell-buffer title)
  "Rename SHELL-BUFFER to TITLE using `agent-shell-rename-buffer' when available."
  )

(defun timfel/agent-shell--live-buffers ()
  "Return live `agent-shell' buffers sorted by buffer name."
  (sort (seq-filter #'buffer-live-p (agent-shell-buffers))
        (lambda (left right)
          (string-lessp (buffer-name left)
                        (buffer-name right)))))

(defun timfel/agent-shell--buffer-busy-p (buffer)
  "Return non-nil when agent shell BUFFER is currently busy."
  (with-current-buffer buffer
    (and (derived-mode-p 'agent-shell-mode)
         (fboundp 'shell-maker-busy)
         (shell-maker-busy))))

(defun timfel/agent-shell--grid-dimensions (count)
  "Return `(COLUMNS . ROWS)' for arranging COUNT buffers in a grid."
  (let* ((columns (max 1 (ceiling (sqrt count))))
         (rows (max 1 (ceiling (/ (float count) columns)))))
    (cons columns rows)))

;;;###autoload
(defun timfel/agent-shell-command-prefix-bwrap (_buffer)
  "Return a `bwrap' command prefix for `agent-shell', or nil when unavailable."
  (when (executable-find "bwrap")
    (let* ((tmpdir (make-temp-file "/tmp/bcodex-session/" t))
           (common-root (or (timfel/agent-shell--git-common-root)
                            default-directory))
           (graal-dir (expand-file-name "../graal"))
           (extra-dir-to-bind (if (file-directory-p graal-dir)
                                  graal-dir
                                default-directory))
           (graal-common-root (if (file-directory-p graal-dir)
                                  (or (timfel/agent-shell--git-common-root graal-dir)
                                      graal-dir)
                                extra-dir-to-bind))
           (base-args
            `("bwrap" "--die-with-parent" "--new-session"
              "--ro-bind" "/" "/"
              "--bind" ,default-directory ,default-directory
              "--bind" ,common-root ,common-root
              "--bind" ,extra-dir-to-bind ,extra-dir-to-bind
              "--bind" ,graal-common-root ,graal-common-root))
           (optional-bind-args
            (delq nil
                  (mapcar (lambda (path)
                            (timfel/agent-shell--bwrap-bind-args "--bind" path))
                          (list (expand-file-name "~/dev/mx")
                                (expand-file-name "~/dev/graal")
                                (expand-file-name "~/dev/graalpython")
                                (expand-file-name "~/dev/graal-enterprise")
                                (expand-file-name "~/.cache")
                                (expand-file-name "~/.mx")
                                (expand-file-name "~/dev/.metadata")
                                (expand-file-name "~/.eclipse")
                                (expand-file-name "~/.codex")
                                (expand-file-name "~/.opencode")
                                (expand-file-name "~/.config/opencode"))))))
       (append
       base-args
       (apply #'append optional-bind-args)
       `("--proc" "/proc"
         "--dev" "/dev"
         "--tmpfs" ,timfel/cloud-storage
         "--bind" ,tmpdir "/tmp"
         "--chdir" ,default-directory
         "--setenv" "HTTP_PROXY" ,(or (getenv "HTTP_PROXY") "")
         "--setenv" "HTTPS_PROXY" ,(or (getenv "HTTPS_PROXY") "")
         "--setenv" "NO_PROXY" ,(or (getenv "NO_PROXY") "")
         "--setenv" "HOME" ,(getenv "HOME")
         "--setenv" "TMPDIR" "/tmp"
         "--setenv" "XDG_CACHE_INNER" ,(expand-file-name ".agent-shell/xdgcache")
         "--setenv" "XDG_STATE_INNER" ,(expand-file-name ".agent-shell/xdgstate")
         "--setenv" "XDG_RUNTIME_INNER" ,(expand-file-name ".agent-shell/xdgruntime")
         "--")))))

(defun timfel/agent-shell-tile-buffers-grid (&optional prefix)
  "Tile live `agent-shell' buffers in the selected frame as a grid.

With no PREFIX, tile all live agent-shell buffers.
With a PREFIX other than numeric 1, tile only busy agent-shell buffers.
With numeric PREFIX 1, tile only idle agent-shell buffers."
  (interactive "P")
  (let* ((selector (and prefix
                        (if (= (prefix-numeric-value prefix) 1)
                            #'not
                          #'identity)))
         (buffers (if selector
                      (seq-filter
                       (lambda (buffer)
                         (funcall selector
                                  (timfel/agent-shell--buffer-busy-p buffer)))
                       (timfel/agent-shell--live-buffers))
                    (timfel/agent-shell--live-buffers)))
         (count (length buffers)))
    (if (zerop count)
        (message
         (cond
          ((not prefix) "No live agent-shell buffers")
          ((= (prefix-numeric-value prefix) 1)
           "No idle agent-shell buffers")
          (t
           "No busy agent-shell buffers")))
      (pcase-let* ((`(,columns . ,rows)
                    (timfel/agent-shell--grid-dimensions count)))
        (let* ((root (selected-window))
               (column-windows (list root))
               all-windows)
          (delete-other-windows root)
          (dotimes (_ (1- columns))
            (setq column-windows
                  (append column-windows
                          (list (split-window (car (last column-windows))
                                              nil 'right)))))
          (dolist (column-window column-windows)
            (let ((windows-in-column (list column-window)))
              (dotimes (_ (1- rows))
                (setq windows-in-column
                      (append windows-in-column
                              (list (split-window
                                     (car (last windows-in-column))
                                     nil 'below)))))
              (setq all-windows (append all-windows windows-in-column))))
          (cl-mapc #'set-window-buffer all-windows buffers)
          (dolist (window (nthcdr count all-windows))
            (when (window-live-p window)
              (delete-window window)))
          (balance-windows-area)
          (select-window (car (window-list (selected-frame) 'nomini))))))))

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
  (let* ((directories (timfel/agent-shell--dired-marked-directories)))
    (timfel/agent-shell-fan-out-worktrees
     (mapcar (lambda (directory)
               (list :directory directory))
             directories))))

;;;###autoload
(defun timfel/agent-shell-fan-out-worktrees (task-specs &optional directory)
  "Create one worktree-backed `agent-shell' per entry in TASK-SPECS.

TASK-SPECS may be a list of strings, an alist of `(TITLE . TASK)' pairs, or
plists containing `:directory' plus optional `:title' and `:task'.  Specs with
`:directory' start or resume shells in those existing directories.  Other specs
create or reuse worktrees below DIRECTORY's Git repository, rename each shell
buffer to TITLE, and queue TASK.  When DIRECTORY is nil, use
`default-directory'.

Interactively, prompt for the number of worktrees, then read a TITLE and TASK
for each one.  Return a plist for each started shell with `:title', `:task',
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
                      (timfel/agent-shell--git-root directory))))
    (when (and needs-repo-root (not repo-root))
      (user-error "Not inside a git repository: %s"
                  (expand-file-name (or directory default-directory))))
    (cl-loop for spec in task-specs
             do
             (let* ((title (plist-get spec :title))
                    (task (plist-get spec :task))
                    (old-dir (plist-get spec :directory))
                    (worktree-dir
                     (or (plist-get spec :directory)
                         (timfel/agent-shell--create-worktree
                          repo-root title)))
                    (has-transcripts
                     (file-expand-wildcards (expand-file-name ".agent-shell/transcripts/*.md" worktree-dir)))
                    (startup-mode (if (and has-transcripts (y-or-n-p (format "Resume session in %s" worktree-dir)))
                                      'latest
                                    'new)))
               (timfel/agent-shell--start-shell-in-directory title worktree-dir startup-mode)
               (if (eq startup-mode 'new)
                   (timfel/agent-shell--queue-startup-requests shell-buffer task))))))

(provide 'timfel-agent-shell-extensions)

;;; timfel-agent-shell-extensions.el ends here
