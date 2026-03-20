;;; timfel-agent-shell-extensions.el --- Agent-shell Dired helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Dired entrypoints for agent-shell helpers.

;;; Code:

(require 'dired)
(require 'subr-x)
(require 'timfel-agent-shell-bwrap)
(require 'timfel-agent-shell-fanout)
(require 'timfel-agent-shell-recovery)
(require 'timfel-agent-shell-ui)

;;;###autoload
(defun timfel/dired-agent-shell-marked-directories ()
  "Start or resume `agent-shell' for each marked Dired directory."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "Current buffer is not a Dired buffer"))
  (let ((directories
         (thread-last
           (dired-get-marked-files nil nil #'file-directory-p)
           (seq-map #'expand-file-name)
           (seq-map #'file-name-as-directory)
           (delete-dups))))
    (unless directories
      (user-error "No marked directories in %s" (buffer-name)))
    (timfel/agent-shell-fan-out-worktrees directories)))

(provide 'timfel-agent-shell-extensions)

;;; timfel-agent-shell-extensions.el ends here
