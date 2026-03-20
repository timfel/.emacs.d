;;; timfel-agent-shell-worktrees.el --- Agent-shell worktree helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Focused helpers for allocating agent-shell Git worktrees.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'agent-shell)

(defun timfel/agent-shell-worktrees-git-root (&optional directory)
  "Return the git root for DIRECTORY, or nil when outside Git."
  (let ((default-directory
         (file-name-as-directory
          (expand-file-name (or directory default-directory)))))
    (with-temp-buffer
      (when (zerop (process-file "git" nil t nil
                                 "rev-parse" "--show-toplevel"))
        (string-trim (buffer-string))))))

;;;###autoload
(defun timfel/agent-shell-worktrees-create (repo-root title)
  "Create or reuse an agent-shell worktree below REPO-ROOT for TITLE."
  (let* ((repo-root (expand-file-name repo-root))
         (repo-name (file-name-nondirectory
                     (directory-file-name repo-root)))
         (mx-dir (expand-file-name (format "mx.%s" repo-name) repo-root))
         (repo-roots
          (cons repo-root
                (when (file-directory-p mx-dir)
                  (seq-filter
                   (lambda (sibling)
                     (and (file-directory-p sibling)
                          (not (file-equal-p repo-root sibling))))
                   (mapcar (lambda (name)
                             (expand-file-name (concat "../" name) repo-root))
                           '("graal" "graal-enterprise")))))))
    (cl-labels
        ((prune-missing-worktrees (root)
           (let ((default-directory (file-name-as-directory root))
                 missing-worktrees)
             (with-temp-buffer
               (unless (zerop (process-file "git" nil t nil "worktree" "list" "--porcelain"))
                 (user-error "Failed to list worktrees for %s: %s"
                             root
                             (string-trim (buffer-string))))
               (goto-char (point-min))
               (while (re-search-forward "^worktree \\(.+\\)$" nil t)
                 (let ((worktree-dir (match-string 1)))
                   (unless (file-directory-p worktree-dir)
                     (push worktree-dir missing-worktrees)))))
             (when missing-worktrees
               (with-temp-buffer
                 (unless (zerop (process-file "git" nil t nil "worktree" "prune"))
                   (user-error "Failed to prune missing worktrees for %s: %s"
                               root
                               (string-trim (buffer-string))))))))
         (create-single-worktree (root parent branch)
           (let ((worktree-dir
                  (expand-file-name
                   (file-name-nondirectory (directory-file-name root))
                   parent)))
             (prune-missing-worktrees root)
             (when (file-exists-p worktree-dir)
               (user-error "Worktree directory already exists: %s" worktree-dir))
             (make-directory parent t)
             (with-temp-buffer
               (let ((default-directory (file-name-as-directory root)))
                 (unless (zerop (process-file "git" nil t nil
                                              "worktree" "add"
                                              "-b" branch
                                              worktree-dir
                                              "HEAD"))
                   (user-error "Failed to create worktree %s: %s"
                               worktree-dir
                               (string-trim (buffer-string))))))
             worktree-dir)))
      (dolist (root repo-roots)
        (prune-missing-worktrees root))
      (let ((suffix nil)
            (next-suffix 2))
        (catch 'allocation
          (while t
            (let* ((parent
                    (let* ((default-directory repo-root)
                           (transcript-dir (funcall agent-shell-transcript-file-path-function))
                           (base-dir (file-name-concat
                                      (file-name-parent-directory
                                       (file-name-parent-directory transcript-dir))
                                      "worktrees"))
                           (slug (downcase title))
                           (slug (replace-regexp-in-string "[^[:alnum:]]+" "-" slug))
                           (slug (replace-regexp-in-string "\\`-+\\|-+\\'" "" slug))
                           (slug (if (string-empty-p slug)
                                     "task"
                                   (substring slug 0 (min 24 (length slug)))))
                           (name (if suffix
                                     (format "%s-%02d" slug suffix)
                                   slug)))
                      (expand-file-name name base-dir)))
                   (branch (format "agent-shell/%s"
                                   (file-name-nondirectory
                                    (directory-file-name parent))))
                   (expected-worktrees
                    (mapcar (lambda (root)
                              (expand-file-name
                               (file-name-nondirectory (directory-file-name root))
                               parent))
                            repo-roots))
                   (registered-entries
                    (cl-mapcar
                     (lambda (root worktree-dir)
                       (let ((target (file-name-as-directory
                                      (expand-file-name worktree-dir))))
                         (seq-some
                          (lambda (entry)
                            (when (string=
                                   (file-name-as-directory
                                    (expand-file-name (plist-get entry :path)))
                                   target)
                              entry))
                          (let ((default-directory (file-name-as-directory root))
                                entries
                                current)
                            (with-temp-buffer
                              (unless (zerop (process-file "git" nil t nil "worktree" "list" "--porcelain"))
                                (user-error "Failed to list worktrees for %s: %s"
                                            root
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
                            (nreverse entries)))))
                     repo-roots
                     expected-worktrees))
                   (branches
                    (delq nil
                          (mapcar (lambda (entry)
                                    (when-let ((branch-ref (plist-get entry :branch)))
                                      (string-remove-prefix "refs/heads/" branch-ref)))
                                  registered-entries))))
              (cond
               ((and
                 (seq-every-p #'identity registered-entries)
                 (seq-every-p
                  (lambda (directory)
                    (and
                     (file-directory-p directory)
                     (cl-flet ((directory-prefix-p (path)
                                 (let ((directory (file-name-as-directory
                                                   (expand-file-name directory)))
                                       (path (and path
                                                  (file-name-as-directory
                                                   (expand-file-name path)))))
                                   (and path (string-prefix-p directory path)))))
                       (and
                        (null
                         (seq-some
                          (lambda (buffer)
                            (and (buffer-live-p buffer)
                                 (with-current-buffer buffer
                                   (and default-directory
                                        (directory-prefix-p default-directory)
                                        (or (derived-mode-p 'agent-shell-mode)
                                            (let ((name (buffer-name buffer)))
                                              (or (string-match-p "\\`\\*acp-.*\\(log\\|traffic\\)\\*\\'" name)
                                                  (string-match-p "\\`acp-client-stderr(" name))))))))
                          (buffer-list)))
                        (null
                         (seq-some
                          (lambda (process)
                            (and
                             (process-live-p process)
                             (directory-prefix-p
                              (or (when-let ((buffer (process-buffer process)))
                                    (and (buffer-live-p buffer)
                                         (buffer-local-value 'default-directory buffer)))
                                  (let* ((command (process-command process))
                                         (chdir-pos (cl-position "--chdir" command :test #'string=)))
                                    (when chdir-pos
                                      (nth (1+ chdir-pos) command)))))))
                          (process-list))))))
                  expected-worktrees))
                (message "Reusing existing worktree %s on branch %s"
                         (car expected-worktrees)
                         (car branches))
                (throw 'allocation (car expected-worktrees)))
               ((and (not (seq-some #'identity registered-entries))
                     (not
                      (seq-some
                       (lambda (root)
                         (let ((default-directory (file-name-as-directory root)))
                           (zerop (process-file "git" nil nil nil
                                                "show-ref" "--verify" "--quiet"
                                                (format "refs/heads/%s" branch)))))
                       repo-roots))
                     (not (file-exists-p parent)))
                (let ((primary-worktree
                       (create-single-worktree repo-root parent branch)))
                  (dolist (sibling-repo (cdr repo-roots))
                    (create-single-worktree sibling-repo parent branch))
                  (throw 'allocation primary-worktree))))
              (setq suffix next-suffix
                    next-suffix (1+ next-suffix))))))))))

(provide 'timfel-agent-shell-worktrees)

;;; timfel-agent-shell-worktrees.el ends here
