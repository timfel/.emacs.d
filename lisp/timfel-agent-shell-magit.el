;;; timfel-agent-shell-magit.el --- Agent-shell helpers for Magit -*- lexical-binding: t -*-

;;; Commentary:

;; Review selected Magit diff regions with a matching live `agent-shell'.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'agent-shell)
(require 'magit-diff)
(require 'magit-section)
(require 'magit-mode)
(require 'timfel-agent-shell-fanout)

(defvar timfel/agent-shell-magit-comment-history nil
  "Minibuffer history for Magit review comments sent to `agent-shell'.")

(defun timfel/agent-shell-magit--read-shell-buffer (repo-root)
  "Return the live `agent-shell' buffer matching REPO-ROOT."
  (if-let* ((buffers (thread-last
                       (agent-shell-buffers)
                       (seq-filter #'buffer-live-p)
                       (seq-filter (lambda (b)
                                     (file-in-directory-p
                                      (buffer-local-value default-directory b)
                                      repo-root)))))
            (choices (seq-map (lambda (buffer)
                                (cons (format "%s  [%s]"
                                              (buffer-name buffer)
                                              (buffer-local-value 'default-directory buffer))
                                      buffer))
                              buffers)))
      (completing-read "Agent shell: " choices nil t nil nil (caar choices))
    (user-error "No agent-shell to send to")))

(defun timfel/agent-shell-magit--selected-hunk ()
  "Return the current hunk section.

If the region is active, it has to stay inside a single hunk body."
  (unless (derived-mode-p 'magit-diff-mode)
    (user-error "Current buffer is not a Magit diff"))
  (let ((section (magit-current-section)))
    (unless (magit-hunk-section-p section)
      (user-error "Point must be inside a diff hunk"))
    (when (region-active-p)
      (unless (magit-section-internal-region-p section)
        (user-error "Region must stay inside a single diff hunk body")))
    section))

(defun timfel/agent-shell-magit--region-context (section)
  "Return diff context for SECTION.

Use the active internal region when present, otherwise include the full hunk."
  (concat
   (magit-diff-file-header section)
   (if (and (region-active-p)
            (magit-section-internal-region-p section))
       (magit-diff-hunk-region-patch section)
     (buffer-substring-no-properties (oref section start) (oref section end)))))

(defun timfel/agent-shell-magit--request (repo-root commit-sha file comment patch)
  (concat
   "You are reviewing code.\n"
   "Repository: " repo-root "\n"
   "Commit-ish: " commit-sha "\n"
   "File: " file "\n\n"
   comment
   "\n\n"
   "```diff\n" patch "```\n\n"
   "Please address respond to this review comment. "
   "Do NOT edit any code, unless the requested change "
   "is specifically ONLY to make a small code change."))

;;;###autoload
(defun timfel/magit-diff-comment-region-with-agent-shell (comment)
  "Send COMMENT and the selected Magit diff region to a matching `agent-shell'."
  (interactive
   (list
    (read-from-minibuffer "Agent review comment: " nil nil nil
                          'timfel/agent-shell-magit-comment-history)))
  (if-let* ((section (magit-current-section))
            (repo-root (magit-toplevel))
            (commit-sha (magit-buffer-revision))
            (file (magit-section-parent-value section))
            (shell-buffer (timfel/agent-shell-magit--read-shell-buffer repo-root))
            (patch (timfel/agent-shell-magit--region-context section))
            (request (timfel/agent-shell-magit--request repo-root commit-sha file comment patch)))
    (with-current-buffer shell-buffer
      (agent-shell-queue-request request))
    (message "Queued review comment for %s in %s"
             (abbreviate-file-name file)
             (buffer-name shell-buffer))))

(provide 'timfel-agent-shell-magit)

;;; timfel-agent-shell-magit.el ends here
