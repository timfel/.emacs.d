;;; timfel-agent-shell-recovery.el --- Agent-shell live set recovery -*- lexical-binding: t -*-

;;; Commentary:

;; Helper to recover agent shells after emacs restart.

;;; Code:

(require 'agent-shell)
(require 'seq)

(defvar timfel/agent-shell-recovery--live-set-inhibit-save nil)
(defconst timfel/agent-shell-recovery--state-file
  (locate-user-emacs-file ".agent-shell/live-agent-shell-set.el"))

;;;###autoload
(defun timfel/agent-shell-recovery-recover-live-set ()
  "Restore or refresh the saved live set of `agent-shell' directories.

When called interactively, reopens each directory saved in
`~/.emacs.d/.agent-shell/live-agent-shell-set.el' with `agent-shell'."
  (interactive)
  (let ((directories
         (when (file-readable-p timfel/agent-shell-recovery--state-file)
           (with-temp-buffer
             (insert-file-contents timfel/agent-shell-recovery--state-file)
             (read (current-buffer)))))
        (restored 0))
    (unless directories
      (user-error "No saved live agent-shell set"))
    (let ((timfel/agent-shell-recovery--live-set-inhibit-save t))
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
                  (agent-shell-session-strategy 'latest))
              (call-interactively #'agent-shell)))
          (setq restored (1+ restored)))))
    (timfel/agent-shell-recovery--update-live-set)
    (message "Recovered %d agent-shell director%s"
             restored
             (if (= restored 1) "y" "ies"))))

(defun timfel/agent-shell-recovery--update-live-set (&optional omit-buffer)
  "Save the current live set of agent shells.

When called inside an agent-shell, install a buffer-local
`kill-buffer-hook' that rewrites the snapshot while omitting the buffer
being killed."
  (when (derived-mode-p 'agent-shell-mode)
    (add-hook 'kill-buffer-hook
              (lambda () (timfel/agent-shell-recovery--update-live-set (current-buffer)))
              nil t))
  (unless timfel/agent-shell-recovery--live-set-inhibit-save
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
      (make-directory (file-name-directory timfel/agent-shell-recovery--state-file) t)
      (with-temp-file timfel/agent-shell-recovery--state-file
        (let ((print-length nil)
              (print-level nil))
          (prin1 directories (current-buffer))
          (insert "\n"))))))

(provide 'timfel-agent-shell-recovery)

;;; timfel-agent-shell-recovery.el ends here
