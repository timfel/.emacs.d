;;; timfel-agent-shell-magit.el --- Agent-shell helpers for Magit -*- lexical-binding: t -*-

;;; Commentary:

;; Review selected Magit diff regions with a matching live `agent-shell'.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'agent-shell)
(require 'magit-diff)
(require 'magit-section)
(require 'timfel-agent-shell-fanout)

(defvar magit-buffer-revision nil)
(defvar magit-buffer-revision-hash nil)

(defvar timfel/agent-shell-magit-comment-history nil
  "Minibuffer history for Magit review comments sent to `agent-shell'.")

(defun timfel/agent-shell-magit--matching-shell-buffers (repo-root)
  "Return live `agent-shell' buffers whose worktree parent contains REPO-ROOT."
  (let ((repo-root (file-name-as-directory (expand-file-name repo-root))))
    (seq-filter
     (lambda (buffer)
       (if (buffer-live-p buffer)
            (with-current-buffer buffer
              (let ((worktree-parent
                     (or (buffer-local-value 'timfel/agent-shell-worktree-parent buffer)
                         default-directory)))
                (and worktree-parent
                     (file-in-directory-p
                      repo-root
                      (file-name-as-directory
                       (expand-file-name worktree-parent))))))))
     (agent-shell-buffers))))

(defun timfel/agent-shell-magit--read-shell-buffer (repo-root)
  "Return the live `agent-shell' buffer matching REPO-ROOT."
  (let ((buffers (timfel/agent-shell-magit--matching-shell-buffers repo-root)))
    (cond
     ((null buffers)
      (user-error "No live agent-shell worktree parent contains %s" repo-root))
     ((null (cdr buffers))
      (car buffers))
     (t
      (let* ((choices
              (mapcar
               (lambda (buffer)
                 (cons
                  (format "%s  [%s]"
                          (buffer-name buffer)
                          (buffer-local-value 'default-directory buffer))
                  buffer))
               buffers))
             (selected
              (completing-read "Agent shell: " choices nil t nil nil
                               (caar choices))))
        (or (cdr (assoc selected choices))
            (user-error "No agent-shell selected")))))))

(defun timfel/agent-shell-magit--commit-sha ()
  "Return the commit SHA for the current Magit revision diff."
  (or magit-buffer-revision-hash
      (and magit-buffer-revision
           (magit-rev-parse magit-buffer-revision))
      (user-error "This Magit diff is not locked to a single commit")))

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
  "Format an `agent-shell' request for review feedback."
  (format
   (concat
    "You are reviewing code from Magit.\n\n"
    "Repository: %s\n"
    "Commit SHA: %s\n"
    "File: %s\n\n"
    "Comment:\n%s\n\n"
    "Diff context (includes the file and hunk header):\n"
    "```diff\n%s```\n\n"
    "Please address respond to this review comment. "
    "Do NOT edit any code, unless the requested change "
    "is specifically ONLY to make a small code change.")
   repo-root commit-sha file comment patch))

;;;###autoload
(defun timfel/magit-diff-comment-region-with-agent-shell (comment)
  "Send COMMENT and the selected Magit diff region to a matching `agent-shell'."
  (interactive
   (list
    (read-from-minibuffer "Agent review comment: " nil nil nil
                          'timfel/agent-shell-magit-comment-history)))
  (let* ((section (timfel/agent-shell-magit--selected-hunk))
         (repo-root (magit-toplevel))
         (shell-buffer (timfel/agent-shell-magit--read-shell-buffer repo-root))
         (commit-sha (timfel/agent-shell-magit--commit-sha))
         (file (magit-section-parent-value section))
         (patch (timfel/agent-shell-magit--region-context section))
         (request (timfel/agent-shell-magit--request
                   repo-root commit-sha file comment patch)))
    (with-current-buffer shell-buffer
      (agent-shell-queue-request request))
    (message "Queued review comment for %s in %s"
             (abbreviate-file-name file)
             (buffer-name shell-buffer))))

(provide 'timfel-agent-shell-magit)

;;; timfel-agent-shell-magit.el ends here
