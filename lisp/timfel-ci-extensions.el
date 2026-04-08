;;; timfel-ci-extensions.el --- CI dashboard helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Local helpers around `emacs-ci' for launching agent-shell reviews from
;; `*ci-dashboard*'.

;;; Code:

(require 'eieio)
(require 'subr-x)
(require 'timfel)

(declare-function ci--section-find-type "emacs-ci" (typ section))
(declare-function magit-current-section "magit-section")
(declare-function timfel/agent-shell-fan-out-worktrees
                  "timfel-agent-shell-extensions"
                  (task-specs &optional directory))

(defvar ci-dashboard-base-url nil)
(defvar ci-dashboard-mode-map nil)

(defconst timfel/ci-dashboard--jira-key-regexp
  "\\b[A-Z][A-Z0-9]+-[0-9]+\\b"
  "Regexp matching a Jira-style issue key inside a pull request title.")

(defun timfel/ci-dashboard--read-project-root ()
  "Prompt for the project root to use for agent worktree creation."
  (let* ((recent-root (timfel/determine-recent-project-root))
         (initial-directory (or recent-root default-directory)))
    (expand-file-name
     (read-directory-name "Git directory for agent worktrees: "
                          initial-directory nil t))))

(defun timfel/ci-dashboard--section-heading (section)
  "Return the visible heading text for SECTION."
  (let ((start (funcall #'slot-value section 'start))
        (content (funcall #'slot-value section 'content))
        (end (funcall #'slot-value section 'end)))
    (string-trim
     (buffer-substring-no-properties
      start
      (1- (or content end))))))

(defun timfel/ci-dashboard--normalize-pr-title (title)
  "Normalize dashboard heading TITLE into a pull request title."
  (thread-last
    title
    (string-trim)
    (string-remove-prefix ">")
    (replace-regexp-in-string " ([0-9]+/[0-9]+/[0-9]+)\\'" "")
    (replace-regexp-in-string " " "_")
    (replace-regexp-in-string "_+" "_")
    (string-trim)))

(defun timfel/ci-dashboard--pr-title (pr &optional section)
  "Return the pull request title from PR or SECTION."
  (let* ((pull-request (alist-get 'pullRequest pr))
         (title (alist-get 'title pull-request)))
    (or title
        (when section
          (timfel/ci-dashboard--normalize-pr-title
           (timfel/ci-dashboard--section-heading section)))
        (user-error "Pull request at point is missing a title"))))

(defun timfel/ci-dashboard--task-title (pr-title pr-url)
  "Return a compact task title derived from PR-TITLE."
  (let ((pr-id (replace-regexp-in-string ".*/" "" pr-url)))
    (concat
     (or (when (string-match timfel/ci-dashboard--jira-key-regexp pr-title)
           (match-string 0 pr-title))
         (let ((title (string-trim (replace-regexp-in-string "\\s-+" " " pr-title))))
         (if (> (length title) 48)
             (concat (substring title 0 48) "...")
           title)))
     "-PR-" pr-id)))

(defun timfel/ci-dashboard--pr-url (pr)
  "Return the browser URL for pull request PR."
  (let* ((merge-job (alist-get 'mergeJob pr))
         (pull-request (or (alist-get 'pullRequest pr)
                           (alist-get 'pullRequest merge-job)))
         (to-ref (alist-get 'toRef pull-request))
         (to-repo (alist-get 'repository to-ref))
         (to-slug (alist-get 'slug to-repo))
         (pr-id (alist-get 'id pull-request)))
    (unless (and to-slug pr-id)
      (user-error "Could not determine pull request URL at point"))
    (format "%s/projects/G/repos/%s/pull-requests/%s"
            ci-dashboard-base-url to-slug pr-id)))

(defun timfel/ci-dashboard--pr-section (section)
  "Return the enclosing PR section for SECTION, or nil."
  (ci--section-find-type 'ci-pr-entry section))

(defun timfel/ci-dashboard--pr-task (pr section prompt)
  "Return a `(TITLE . PROMPT)' pair for reviewing pull request PR."
  (let* ((pr-title (timfel/ci-dashboard--pr-title pr section))
         (pr-url (timfel/ci-dashboard--pr-url pr)))
    (cons
     (timfel/ci-dashboard--task-title pr-title pr-url)
     (format
      (concat
       "PR title: %s\n"
       "PR URL: %s\n\n"
       (or prompt (concat "Review the CI gates on this pull request.\n\n"
                          "Inspect the current gate state for this PR, identify anything failing "
                          "or blocking, summarize what matters, and propose next actions.")))
      pr-title pr-url))))

(defun timfel/ci-dashboard--job-task (pr job pr-section prompt)
  "Return a `(TITLE . PROMPT)' pair for reviewing JOB from pull request PR."
  (let* ((pr-title (timfel/ci-dashboard--pr-title pr pr-section))
         (pr-url (timfel/ci-dashboard--pr-url pr))
         (job-key (or (alist-get 'key job)
                      (user-error "Job at point is missing a key")))
         (job-url (or (alist-get 'url job)
                      (user-error "Job at point is missing a URL"))))
    (cons
     (format "%s-%s"
             (timfel/ci-dashboard--task-title pr-title pr-url)
             job-key)
     (format
      (concat
       "PR title: %s\n"
       "PR URL: %s\n"
       "Job key: %s\n"
       "Job URL: %s\n\n"
       (or prompt (concat
                   "Review this CI job on the pull request under point.\n\n"
                   "Focus on this job first. Explain its current state, investigate why "
                   "it is failing or noteworthy, and propose the next actions.")))
      pr-title pr-url job-key job-url))))

(defun timfel/ci-dashboard--task-at-point (prompt)
  "Return the `(TITLE . PROMPT)' pair for the CI dashboard thing at point.

Return nil when point is neither on a pull request nor on a job."
  (let* ((section (magit-current-section))
         (type (and section (oref section type)))
         (value (and section (oref section value))))
    (pcase type
      ('ci-pr-entry
       (timfel/ci-dashboard--pr-task value section prompt))
      ('ci-dashboard-job
       (when-let ((pr-section (timfel/ci-dashboard--pr-section section)))
         (timfel/ci-dashboard--job-task (oref pr-section value) value pr-section prompt)))
      (_ nil))))

;;;###autoload
(defun timfel/ci-dashboard-investigate-with-agent (&optional arg)
  "Fan out an agent-shell worktree review for the PR or job at point."
  (interactive "P")
  (let ((prompt (if arg (read-from-minibuffer "Prompt: ") nil)))
    (if-let ((task (timfel/ci-dashboard--task-at-point prompt)))
      (let ((project-root (timfel/ci-dashboard--read-project-root)))
        (unless (require 'timfel-agent-shell-extensions nil t)
          (user-error "timfel-agent-shell-extensions is not available"))
        (timfel/agent-shell-fan-out-worktrees (list task) project-root))
    (message "Point must be on a pull request or job"))))

(with-eval-after-load 'emacs-ci
  (bind-key (kbd "C-x a i")
            #'timfel/ci-dashboard-investigate-with-agent
            'ci-dashboard-mode-map))

(provide 'timfel-ci-extensions)

;;; timfel-ci-extensions.el ends here
