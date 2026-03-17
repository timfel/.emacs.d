;;; timfel-jira-extensions.el --- Jira helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Local helpers around `jira.el' for querying and opening Jira issues.

;;; Code:

(require 'tabulated-list)
(require 'tablist)

(require 'jira)

(declare-function timfel/agent-shell-fan-out-worktrees
                  "timfel-agent-shell-extensions"
                  (task-specs &optional directory))
(declare-function jira-utils-marked-items "jira-utils")
(declare-function timfel/determine-recent-project-root "timfel")

(defvar jira-issues-key-summary-map)

(defvar timfel/jira-periodic-issues-buffer-name "*Jira Periodic Issues*"
  "Buffer name for periodic Jira issues.")

(defvar-local timfel/jira-periodic-issues--days 90
  "Number of days used to populate the current periodic issues buffer.")

(defvar-local timfel/jira-periodic-issues--summary-map nil
  "Hash table mapping visible issue keys to summaries in the current buffer.")

(defun timfel/jira-periodic-issues-alist (&optional days)
  "Return an alist of recent periodic Jira issues within DAYS.

This currently delegates to `timfel/jira-periodic-python-issues-alist'."
  (timfel/jira-periodic-python-issues-alist days))

;;;###autoload
(defun timfel/jira-periodic-python-issues-alist (&optional days)
  "Return an alist of recent periodic Python Jira issues.

The return value is a list of `(KEY . SUMMARY)' pairs for issues with label
`periodic-job-failures', component `Python', status other than `Closed' or
`In Progress', and created within DAYS days.  DAYS defaults to 90."
  (let* ((days (or days 90))
         (jql (concat
               "labels = periodic-job-failures "
               "AND component = \"Python\" "
               "AND status NOT IN (\"Closed\", \"In Progress\") "
               (format "AND created >= -%dd " days)
               "ORDER BY created DESC")))
    (unless (require 'jira nil t)
      (user-error "jira.el is not installed"))
    (unless (require 'jira-api nil t)
      (user-error "jira-api.el is not available"))
    (let* ((response
            (jira-api-search
             :sync t
             :params `(("jql" . ,jql)
                       ("maxResults" . "100")
                       ("fields" . "key,summary"))))
           (data (request-response-data response))
           (issues (append (alist-get 'issues data) nil)))
      (mapcar (lambda (issue)
                (cons (alist-get 'key issue)
                      (alist-get 'summary (alist-get 'fields issue))))
              issues))))

(defun timfel/jira-periodic-issues--issue-at-point ()
  "Return the Jira issue key for the current tabulated row."
  (or (tabulated-list-get-id)
      (user-error "No Jira issue on this line")))

(defun timfel/jira-periodic-issues-open-issue ()
  "Open the Jira issue at point inside Emacs."
  (interactive)
  (jira-detail-show-issue (timfel/jira-periodic-issues--issue-at-point)))

(defun timfel/jira-periodic-issues-open-issue-in-browser ()
  "Open the Jira issue at point in an external browser."
  (interactive)
  (jira-actions-open-issue (timfel/jira-periodic-issues--issue-at-point)))

(defun timfel/jira-periodic-issues--issue-title-at-point ()
  "Return the Jira issue summary for the current tabulated row."
  (let ((entry (tabulated-list-get-entry)))
    (or (and entry (> (length entry) 1)
             (aref entry 1))
        (user-error "No Jira issue summary on this line"))))

(defun timfel/jira--agent-task (issue-id issue-title)
  "Build the agent investigation task for ISSUE-ID and ISSUE-TITLE."
  (format
   (concat
    "Investigate recent GraalPy periodic issue %s: %s\n\n"
    "Please inspect the Jira issue, failing job context, identify "
    "if any work on it was already done in this git repository. "
    "If work was done, double check it against the issue context "
    "and report your conclusions. Othewise, try to find the root "
    "cause in this repository, and propose a focused fix "
    "with validation if feasible.")
   issue-id issue-title))

(defun timfel/jira--read-project-root ()
  "Prompt for the project root to use for agent worktree creation."
  (let* ((recent-root (and (fboundp #'timfel/determine-recent-project-root) (timfel/determine-recent-project-root)))
         (initial-directory (or recent-root default-directory)))
    (expand-file-name
     (read-directory-name "Project root for agent worktrees: "
                          initial-directory nil t))))

(defun timfel/jira-periodic-issues--marked-issues ()
  "Return explicitly marked periodic Jira issues as `(KEY . SUMMARY)' pairs."
  (let (issues)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((issue-id (tabulated-list-get-id))
              (mark-state (and (fboundp 'tablist-get-mark-state)
                               (tablist-get-mark-state))))
          (when (and issue-id mark-state
                     (not (eq (car mark-state) ?\s)))
            (push (cons issue-id
                        (or (and timfel/jira-periodic-issues--summary-map
                                 (gethash issue-id timfel/jira-periodic-issues--summary-map))
                            ""))
                  issues)))
        (forward-line 1)))
    (nreverse issues)))

(defun timfel/jira--explicitly-marked-issue-ids ()
  "Return explicitly marked Jira issue ids in the current tabulated list."
  (let (issue-ids)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((issue-id (tabulated-list-get-id))
              (mark-state (and (fboundp 'tablist-get-mark-state)
                               (tablist-get-mark-state))))
          (when (and issue-id mark-state
                     (not (eq (car mark-state) ?\s)))
            (push issue-id issue-ids)))
        (forward-line 1)))
    (nreverse issue-ids)))

(defun timfel/jira-periodic-issues-investigate-with-agent ()
  "Start worktree-backed agent investigations for marked periodic Jira issues.

If no issues are marked in `*Jira Periodic Issues*', emit a message and do
nothing."
  (interactive)
  (unless (require 'timfel-agent-shell-extensions nil t)
    (user-error "timfel-agent-shell-extensions is not available"))
  (let ((issues (timfel/jira-periodic-issues--marked-issues)))
    (if (null issues)
        (message "No Jira issues are marked")
      (let ((project-root (timfel/jira--read-project-root)))
        (timfel/agent-shell-fan-out-worktrees
         (mapcar (lambda (issue)
                   (cons (car issue)
                         (timfel/jira--agent-task (car issue) (cdr issue))))
                 issues)
         project-root)))))

;;;###autoload
(defun timfel/jira-issues-investigate-marked-with-agent ()
  "Start worktree-backed agent investigations for marked Jira issues.

If no issues are marked in `*Jira Issues*', emit a message and do nothing."
  (interactive)
  (unless (require 'timfel-agent-shell-extensions nil t)
    (user-error "timfel-agent-shell-extensions is not available"))
  (let ((issue-ids (timfel/jira--explicitly-marked-issue-ids)))
    (if (null issue-ids)
        (message "No Jira issues are marked")
      (let ((project-root (timfel/jira--read-project-root)))
        (timfel/agent-shell-fan-out-worktrees
         (mapcar (lambda (issue-id)
                   (let ((issue-title (or (gethash issue-id jira-issues-key-summary-map)
                                          "")))
                     (cons issue-id (timfel/jira--agent-task issue-id issue-title))))
                 issue-ids)
         project-root)))))

(defun timfel/jira-periodic-issues--refresh (&optional days)
  "Populate the current buffer with periodic Jira issues for DAYS."
  (let* ((days (or days timfel/jira-periodic-issues--days 90))
         (issues (timfel/jira-periodic-issues-alist days))
         (summary-map (make-hash-table :test #'equal)))
    (dolist (issue issues)
      (puthash (car issue) (or (cdr issue) "") summary-map))
    (setq-local timfel/jira-periodic-issues--days days)
    (setq-local timfel/jira-periodic-issues--summary-map summary-map)
    (setq tabulated-list-entries
          (mapcar (lambda (issue)
                    (let ((key (car issue))
                          (summary (cdr issue)))
                      (list key (vector key (or summary "")))))
                  issues))))

(defun timfel/jira-periodic-issues--revert (_ignore-auto _noconfirm)
  "Refresh the periodic Jira issues buffer."
  (timfel/jira-periodic-issues--refresh)
  (tabulated-list-print t))

(defvar timfel/jira-periodic-issues-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "C-x a i") #'timfel/jira-periodic-issues-investigate-with-agent)
    (define-key map (kbd "I") #'timfel/jira-periodic-issues-open-issue)
    (define-key map (kbd "O") #'timfel/jira-periodic-issues-open-issue-in-browser)
    map)
  "Keymap for `timfel/jira-periodic-issues-mode'.")

(define-derived-mode timfel/jira-periodic-issues-mode tabulated-list-mode "Jira-Periodic"
  "Major mode for viewing periodic Jira issues."
  (setq tabulated-list-format [("Issue" 18 t)
                               ("Summary" 0 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Issue" nil))
  (setq-local header-line-format
              "m/u: mark or unmark    I: open issue in Jira    O: open issue in browser    C-x a i: investigate marked issues")
  (tablist-minor-mode)
  (setq-local revert-buffer-function #'timfel/jira-periodic-issues--revert)
  (tabulated-list-init-header))

;;;###autoload
(defun timfel/jira-periodic-issues (&optional days)
  "Show recent periodic Jira issues from the last DAYS days.

With a prefix argument, prompt for DAYS.  DAYS defaults to 90."
  (interactive
   (list (when current-prefix-arg
           (read-number "Show periodic Jira issues from last N days: " 90))))
  (let ((buffer (get-buffer-create timfel/jira-periodic-issues-buffer-name)))
    (with-current-buffer buffer
      (timfel/jira-periodic-issues-mode)
      (timfel/jira-periodic-issues--refresh days)
      (tabulated-list-print t))
    (pop-to-buffer buffer)))

;;;###autoload
(defun timfel/jira ()
  "Open the Jira issues UI after priming cached Jira metadata."
  (interactive)
  (unless (require 'jira nil t)
    (user-error "jira.el is not installed"))
  (condition-case nil
      (jira-api-get-basic-data)
    (error nil))
  (jira-api-get-users)
  (jira-api-get-fields)
  (jira-api-get-statuses)
  (jira-api-get-resolutions)
  (jira-api-get-projects)
  (funcall-interactively #'jira-issues))

(provide 'timfel-jira-extensions)

;;; timfel-jira-extensions.el ends here
