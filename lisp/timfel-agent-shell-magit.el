;;; timfel-agent-shell-magit.el --- Agent-shell context helpers for Magit -*- lexical-binding: t -*-

;;; Commentary:

;; Turn the current Magit diff hunk into `agent-shell' context.

;;; Code:

(require 'subr-x)
(require 'magit-diff)
(require 'magit-section)
(require 'magit-mode)

(defun timfel/agent-shell-magit--region-context (section)
  "Return diff context for SECTION.

Use the active internal region when present, otherwise include the full hunk."
  (concat
   (magit-diff-file-header section)
   (if (and (region-active-p)
            (magit-section-internal-region-p section))
       (magit-diff-hunk-region-patch section)
     (buffer-substring-no-properties (oref section start) (oref section end)))))

(defun timfel/agent-shell-magit--selected-hunk ()
  "Return the current hunk section, or nil when none is selected.

If the region is active, it has to stay inside a single hunk body."
  (when-let ((section (magit-current-section)))
    (when (and (magit-hunk-section-p section)
               (or (not (region-active-p))
                   (magit-section-internal-region-p section)))
      section)))

(defun timfel/agent-shell-magit--format-context (commit-sha patch)
  "Format Magit diff context for `agent-shell'."
  (string-join
   (delq nil
         (list "[MAGIT DIFF CONTEXT]"
               (when commit-sha
                 (format "Commit-ish: %s" commit-sha))
               ""
               "```diff"
               patch
               "```"
               "[END CONTEXT]"))
   "\n"))

;;;###autoload
(defun timfel/agent-shell-magit-context-source ()
  "Return the current Magit hunk as `agent-shell' context, or nil."
  (when-let* ((section (timfel/agent-shell-magit--selected-hunk))
              (patch (string-trim-right
                      (timfel/agent-shell-magit--region-context section))))
    (unless (string-empty-p patch)
      (timfel/agent-shell-magit--format-context (magit-buffer-revision) patch))))

(provide 'timfel-agent-shell-magit)

;;; timfel-agent-shell-magit.el ends here
