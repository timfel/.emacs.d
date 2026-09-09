;;; timfel-gptel-orchestration.el --- GPTel orchestration helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs-native gptel tools for the AGENTS.md workflows around current work,
;; upcoming work, and starting worktree-backed tasks.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'timfel)

(declare-function gptel--apply-preset "gptel")
(declare-function gptel-abort "gptel")
(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-new-temp-shell "agent-shell")
(declare-function agent-shell-insert "agent-shell")
(declare-function agent-shell-queue-request "agent-shell")
(declare-function agent-shell-rename-buffer "agent-shell")
(declare-function agent-shell-status "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")
(declare-function shell-maker-point-at-last-prompt-p "shell-maker")
(declare-function ci-dashboard "emacs-ci")
(declare-function ci--auth-headers "emacs-ci")
(declare-function ci--request-with-proxy-aware-settings "emacs-ci")
(declare-function org-agenda "org-agenda")
(declare-function org-agenda-files "org")
(declare-function agent-shell-fanout-worktrees "agent-shell-fanout")
(declare-function jira-api--get-current-url "jira-api")
(declare-function jira-api-search "jira-api")
(declare-function request-abort "request")
(declare-function request-response-p "request")
(declare-function timfel/jira "timfel-jira-extensions")

(defvar ci-dashboard-base-url)

(defconst timfel/gptel-orchestration-live-set-file
  (locate-user-emacs-file ".agent-shell/live-agent-shell-set.el")
  "Location of the persisted live agent-shell directory set.")

(defconst timfel/gptel-orchestration-buffer-name "*gptel-agents*"
  "Default buffer name for the AGENTS.md orchestration chat.")

(defvar timfel/gptel-orchestration-tools nil
  "GPTel tools registered for agent orchestration.")

(defgroup timfel/weekly-confluence-report nil
  "Build Tim's weekly Confluence update from local work signals."
  :group 'timfel)

(defcustom timfel/weekly-confluence-report-slack-channel "#graal-python-weekly"
  "Slack channel that mentions team issues for the weekly update."
  :type 'string
  :group 'timfel/weekly-confluence-report)

(defcustom timfel/weekly-confluence-report-pr-repositories '("graalpython")
  "Bitbucket repository slugs whose recent pull requests should be collected."
  :type '(repeat string)
  :group 'timfel/weekly-confluence-report)

(defcustom timfel/weekly-confluence-report-jira-server
  "Oracle Cloud Infrastructure JIRA"
  "Confluence Jira macro server name."
  :type 'string
  :group 'timfel/weekly-confluence-report)

(defcustom timfel/weekly-confluence-report-jira-server-id
  "7dc25513-b840-3650-9d17-2a9f1a9822f6"
  "Confluence Jira macro server id."
  :type 'string
  :group 'timfel/weekly-confluence-report)

(defcustom timfel/weekly-confluence-report-slack-timeout 900
  "Seconds to wait for the Slack agent-shell context file."
  :type 'integer
  :group 'timfel/weekly-confluence-report)

(defcustom timfel/weekly-confluence-report-request-timeout 30
  "Seconds to wait for individual Jira or Bitbucket requests."
  :type 'integer
  :group 'timfel/weekly-confluence-report)

(defun timfel/gptel-orchestration--buffer-summary (buffer)
  "Return a compact plist describing BUFFER."
  (with-current-buffer buffer
    (list :name (buffer-name buffer)
          :mode (symbol-name major-mode)
          :file buffer-file-name
          :directory (when default-directory
                       (expand-file-name default-directory)))))

(defun timfel/gptel-orchestration--interesting-buffer-p (buffer)
  "Return non-nil when BUFFER likely reflects the user's active work."
  (with-current-buffer buffer
    (let ((name (buffer-name buffer)))
      (or (derived-mode-p 'agent-shell-mode)
          (string-prefix-p "*Jira Issues*" name)
          (string-prefix-p "*Jira Issue Detail:" name)
          (string-prefix-p "*vc-dir*" name)
          (string-prefix-p "magit:" name)
          (string-prefix-p "*ci dashboard*" name)
          (string-prefix-p "*ci-dashboard*" name)
          (string-prefix-p "*ci build log" name)
          (bound-and-true-p lsp-mode)
          (bound-and-true-p eglot--managed-mode)))))

(defun timfel/gptel-orchestration--read-live-agent-shell-set ()
  "Return saved live agent-shell directories from disk."
  (when (file-readable-p timfel/gptel-orchestration-live-set-file)
    (with-temp-buffer
      (insert-file-contents timfel/gptel-orchestration-live-set-file)
      (let ((value (read (current-buffer))))
        (when (listp value)
          (seq-map #'expand-file-name value))))))

(defun timfel/gptel-orchestration--read-forms-from-string (string)
  "Read all Lisp forms from STRING and return them as a list."
  (let ((position 0)
        forms)
    (condition-case err
        (while t
          (let ((parsed (read-from-string string position)))
            (setq forms (cons (car parsed) forms)
                  position (cdr parsed))))
      (end-of-file
       (unless (string-match-p "\\`[[:space:]\n\r\t]*\\'"
                               (substring string position))
         (signal (car err) (cdr err)))))
    (nreverse forms)))

(defun timfel/gptel-tool-inspect-current-work-context (&optional limit)
  "Return the most relevant current-work context from the Emacs session."
  (let* ((limit (or limit 12))
         (interesting-buffers
          (seq-take
           (thread-last
             (buffer-list)
             (seq-filter #'timfel/gptel-orchestration--interesting-buffer-p)
             (seq-map #'timfel/gptel-orchestration--buffer-summary))
           limit))
         (agent-shell-summaries
          (when (fboundp 'agent-shell-buffers)
            (seq-take
             (thread-last
               (agent-shell-buffers)
               (seq-filter #'buffer-live-p)
               (seq-map #'timfel/gptel-orchestration--buffer-summary))
             limit))))
    (list :interesting_buffers interesting-buffers
          :agent_shell_buffers agent-shell-summaries
          :recent_agent_shell_directories
          (timfel/gptel-orchestration--read-live-agent-shell-set))))

(defun timfel/gptel-tool-evaluate-workspace-elisp (code)
  "Evaluate one or more Elisp forms from CODE in the live Emacs workspace."
  (let* ((forms (timfel/gptel-orchestration--read-forms-from-string code))
         (last-value nil))
    (unless forms
      (user-error "No Elisp forms provided"))
    (dolist (form forms)
      (setq last-value (eval form t)))
    (list :ok t
          :forms_evaluated (length forms)
          :result (prin1-to-string last-value))))

(defun timfel/gptel-orchestration--open-jira ()
  "Open Tim's Jira work queue and return a summary plist."
  (unless (fboundp 'timfel/jira)
    (unless (require 'timfel-jira-extensions nil t)
      (user-error "timfel-jira-extensions is not available")))
  (call-interactively #'timfel/jira)
  (list :source "jira"
        :status "opened"
        :buffer (buffer-name (current-buffer))
        :note "This view populates asynchronously."))

(defun timfel/gptel-orchestration--open-ci ()
  "Open Tim's CI dashboard and return a summary plist."
  (unless (fboundp 'ci-dashboard)
    (unless (require 'emacs-ci nil t)
      (user-error "emacs-ci is not available")))
  (call-interactively #'ci-dashboard)
  (list :source "ci"
        :status "opened"
        :buffer (buffer-name (current-buffer))
        :note "This view populates asynchronously."))

(defun timfel/gptel-orchestration--open-agenda ()
  "Open Tim's TODO agenda and return a summary plist."
  (require 'org-agenda)
  (org-agenda nil "t")
  (list :source "agenda"
        :status "opened"
        :buffer (buffer-name (current-buffer))))

(defun timfel/gptel-tool-open-work-queues (&optional sources)
  "Open Jira, CI, and/or agenda work queues listed in SOURCES."
  (let* ((requested (or (and (sequencep sources) (append sources nil))
                        '("jira" "ci" "agenda")))
         (results nil))
    (dolist (source requested)
      (push
       (pcase source
         ("jira" (timfel/gptel-orchestration--open-jira))
         ("ci" (timfel/gptel-orchestration--open-ci))
         ("agenda" (timfel/gptel-orchestration--open-agenda))
         (_ (user-error "Unknown work queue source: %s" source)))
       results))
    (nreverse results)))

(defun timfel/gptel-orchestration--task-title (task)
  "Return TASK's title regardless of its container shape."
  (or (plist-get task :title)
      (alist-get 'title task)
      (and (hash-table-p task) (gethash "title" task))
      (user-error "Each task must include a title")))

(defun timfel/gptel-orchestration--task-prompt (task)
  "Return TASK's prompt regardless of its container shape."
  (or (plist-get task :prompt)
      (alist-get 'prompt task)
      (and (hash-table-p task) (gethash "prompt" task))
      (user-error "Each task must include a prompt")))

(defun timfel/gptel-orchestration--normalize-task-specs (tasks)
  "Convert TASKS into an alist of `(TITLE . PROMPT)' pairs."
  (let ((items (if (vectorp tasks) (append tasks nil) tasks)))
    (unless (and (sequencep items) items)
      (user-error "At least one task is required"))
    (seq-map
     (lambda (task)
       (cons (timfel/gptel-orchestration--task-title task)
             (timfel/gptel-orchestration--task-prompt task)))
     items)))

(defun timfel/gptel-orchestration--use-explicit-directory-task-specs-p (task-specs directory explicit-directory-p)
  "Return non-nil when TASK-SPECS should target DIRECTORY directly.

When EXPLICIT-DIRECTORY-P is non-nil, this tool should treat the
directory as the task-spec title so `agent-shell-fanout-worktrees'
starts the task in that exact directory."
  (when explicit-directory-p
    (when (> (length task-specs) 1)
      (user-error "Cannot start multiple tasks directly in %s" directory))
    t))

(defun timfel/gptel-orchestration--default-work-root ()
  "Return the most sensible default root for starting work."
  (seq-find #'file-directory-p
            (mapcar #'expand-file-name '("~/dev" "d:/"))
            default-directory))

(defun timfel/gptel-orchestration--read-work-directory ()
  "Prompt for the repository or project directory to work in."
  (expand-file-name
   (read-directory-name "work where: "
                        (timfel/gptel-orchestration--default-work-root)
                        nil nil)))

(defun timfel/gptel-orchestration--ensure-git-repo (directory)
  "Create DIRECTORY and initialize Git when needed.

Return a plist describing whether anything was created."
  (let ((created-directory nil)
        (initialized-git nil))
    (unless (file-directory-p directory)
      (make-directory directory t)
      (setq created-directory t))
    (unless (file-directory-p (expand-file-name ".git" directory))
      (let ((default-directory (file-name-as-directory directory)))
        (unless (zerop (process-file "git" nil nil nil "init"))
          (user-error "Failed to initialize git repository in %s" directory))
        (setq initialized-git t)))
    (list :created_directory created-directory
          :initialized_git initialized-git)))

(defun timfel/gptel-orchestration--worktree-capable-p (directory)
  "Return non-nil when DIRECTORY can serve as a worktree source repo."
  (let ((default-directory (file-name-as-directory directory)))
    (or (zerop (process-file "git" nil nil nil "show-ref" "--verify" "--quiet"
                             "refs/remotes/origin/master"))
        (zerop (process-file "git" nil nil nil "show-ref" "--verify" "--quiet"
                             "refs/remotes/origin/main"))
        (zerop (process-file "git" nil nil nil "show-ref" "--verify" "--quiet"
                             "refs/heads/master"))
        (zerop (process-file "git" nil nil nil "show-ref" "--verify" "--quiet"
                             "refs/heads/main"))
        (zerop (process-file "git" nil nil nil "rev-parse" "--verify" "--quiet"
                             "HEAD")))))

(defun timfel/gptel-tool-start-worktree-tasks (tasks &optional directory resume-p)
  "Prompt for a work directory when needed and fan out TASKS there."
  (unless (require 'agent-shell-fanout nil t)
    (user-error "agent-shell-fanout is not available"))
  (let* ((explicit-directory-p (and directory (not (string-blank-p directory))))
         (task-specs (timfel/gptel-orchestration--normalize-task-specs tasks))
         (directory (expand-file-name
                     (or (and explicit-directory-p directory)
                         (timfel/gptel-orchestration--read-work-directory))))
         (repo-state (timfel/gptel-orchestration--ensure-git-repo directory))
         (used-worktrees (timfel/gptel-orchestration--worktree-capable-p directory))
         (effective-task-specs
          (if (timfel/gptel-orchestration--use-explicit-directory-task-specs-p
               task-specs directory explicit-directory-p)
              (list (cons directory (cdar task-specs)))
            (if used-worktrees
                task-specs
              (progn
                (when (> (length task-specs) 1)
                  (user-error
                   (concat "Cannot fan out multiple tasks from %s yet; "
                           "the repository needs a base branch or commit first")
                   directory))
                (list (cons directory (cdar task-specs))))))))
    (agent-shell-fanout-worktrees effective-task-specs directory (if resume-p 'latest 'new))
    (list :ok t
          :directory directory
          :used_worktrees used-worktrees
          :created_directory (plist-get repo-state :created_directory)
          :initialized_git (plist-get repo-state :initialized_git)
          :tasks_started (length effective-task-specs))))

(defun timfel/gptel-tool-bitbucket (argv)
  (condition-case nil
      (let* ((cmd (string-join (append '("gdev-cli" "bitbucket") (mapcar #'shell-quote-argument argv)) " ")))
        (list :ok t
              :command cmd
              :output (shell-command-to-string cmd)))
    (error "bitbucket arguments must be a list of strings")))

(defun timfel/gptel-tool-jira (argv)
  (condition-case nil
      (let* ((cmd (string-join (append '("gdev-cli" "jira") (mapcar #'shell-quote-argument argv)) " ")))
        (list :ok t
              :command cmd
              :output (shell-command-to-string cmd)))
    (error "jira arguments must be a list of strings")))

(defun timfel/weekly-confluence-report--date-string (time)
  "Return TIME as an ISO date in local time."
  (format-time-string "%Y-%m-%d" time))

(defun timfel/weekly-confluence-report--parse-date (date-string &optional end-of-day)
  "Parse DATE-STRING as local date.
When END-OF-DAY is non-nil, return the start of the following day."
  (let ((time (encode-time
               (parse-time-string (concat (string-trim date-string) " 00:00:00")))))
    (if end-of-day
        (time-add time (days-to-time 1))
      time)))

(defun timfel/weekly-confluence-report--time-in-range-p (time since until)
  "Return non-nil when TIME is at or after SINCE and before UNTIL."
  (and time
       (not (time-less-p time since))
       (time-less-p time until)))

(defun timfel/weekly-confluence-report--alist-path (value path)
  "Return nested VALUE at PATH through alists and vectors."
  (seq-reduce
   (lambda (current key)
     (cond
      ((null current) nil)
      ((and (vectorp current) (integerp key) (< key (length current)))
       (aref current key))
      ((and (listp current) (symbolp key))
       (alist-get key current))
      ((and (listp current) (stringp key))
       (or (alist-get (intern key) current)
           (alist-get key current nil nil #'string=)))
      (t nil)))
   path value))

(defun timfel/weekly-confluence-report--jira-issue-url (key)
  "Return browser URL for Jira issue KEY."
  (require 'jira)
  (concat (string-remove-suffix "/" (jira-api--get-current-url)) "/browse/" key))

(defun timfel/weekly-confluence-report--jira-linked-issues (issue)
  "Return compact linked issue data for ISSUE."
  (let ((links (timfel/weekly-confluence-report--alist-path issue '(fields issuelinks))))
    (seq-keep
     (lambda (link)
       (let* ((linked (or (alist-get 'outwardIssue link)
                          (alist-get 'inwardIssue link)))
              (key (alist-get 'key linked))
              (summary (timfel/weekly-confluence-report--alist-path
                        linked '(fields summary)))
              (status (timfel/weekly-confluence-report--alist-path
                       linked '(fields status name)))
              (type (timfel/weekly-confluence-report--alist-path
                     link '(type name))))
         (when key
           (list (cons "key" key)
                 (cons "summary" summary)
                 (cons "status" status)
                 (cons "linkType" type)))))
     (if (vectorp links) (append links nil) links))))

(defun timfel/weekly-confluence-report--normalize-jira-issue (issue source)
  "Return compact report data for Jira ISSUE from SOURCE."
  (let* ((key (alist-get 'key issue))
         (fields (alist-get 'fields issue))
         (summary (alist-get 'summary fields))
         (status (timfel/weekly-confluence-report--alist-path fields '(status name)))
         (assignee (timfel/weekly-confluence-report--alist-path fields '(assignee displayName)))
         (reporter (timfel/weekly-confluence-report--alist-path fields '(reporter displayName)))
         (updated (alist-get 'updated fields))
         (resolutiondate (alist-get 'resolutiondate fields))
         (components (alist-get 'components fields))
         (labels (alist-get 'labels fields))
         (parent (alist-get 'parent fields)))
    (list (cons "source" source)
          (cons "key" key)
          (cons "url" (and key (timfel/weekly-confluence-report--jira-issue-url key)))
          (cons "summary" summary)
          (cons "status" status)
          (cons "assignee" assignee)
          (cons "reporter" reporter)
          (cons "updated" updated)
          (cons "resolutionDate" resolutiondate)
          (cons "components"
                (seq-map (lambda (component) (alist-get 'name component))
                         (if (vectorp components) (append components nil) components)))
          (cons "labels" (if (vectorp labels) (append labels nil) labels))
          (cons "parent"
                (when parent
                  (list (cons "key" (alist-get 'key parent))
                        (cons "summary" (timfel/weekly-confluence-report--alist-path
                                         parent '(fields summary))))))
          (cons "linkedIssues"
                (timfel/weekly-confluence-report--jira-linked-issues issue)))))

(defun timfel/weekly-confluence-report--jira-issues-from-data (data)
  "Return issue list from Jira search DATA."
  (let ((issues (alist-get 'issues data)))
    (cond
     ((vectorp issues) (append issues nil))
     ((listp issues) issues)
     (t nil))))

(defun timfel/weekly-confluence-report--collect-jira-async
    (state since-date until-date callback errback)
  "Collect Jira issues asynchronously, then call CALLBACK or ERRBACK."
  (require 'jira)
  (let* ((fields (string-join
                  '("summary" "status" "assignee" "reporter" "creator"
                    "updated" "created" "resolutiondate" "issuelinks"
                    "components" "labels" "fixVersions" "parent")
                  ","))
         (primary-jql
          (format
           (concat "(((assignee = currentUser() OR reporter = currentUser() "
                   "OR creator = currentUser() OR watcher = currentUser()) "
                   "AND updated >= \"%s\" AND updated < \"%s\") "
                   "OR status changed BY currentUser() AFTER \"%s\") "
                   "ORDER BY updated DESC")
           since-date until-date since-date))
         (fallback-jql
          (format
           (concat "((assignee = currentUser() OR reporter = currentUser() "
                   "OR creator = currentUser()) "
                   "AND updated >= \"%s\" AND updated < \"%s\") "
                   "ORDER BY updated DESC")
           since-date until-date))
         (normalize
          (lambda (source data)
            (seq-map
             (lambda (issue)
               (timfel/weekly-confluence-report--normalize-jira-issue
                issue source))
             (timfel/weekly-confluence-report--jira-issues-from-data data)))))
    (cl-labels
        ((fallback
          ()
          (message "Weekly report: Jira primary query failed; trying fallback query")
          (timfel/weekly-confluence-report--add-request
           state
           (jira-api-search
            :params (list (cons "jql" fallback-jql)
                          (cons "maxResults" 150)
                          (cons "fields" fields))
            :callback (lambda (data _response)
                        (when (timfel/weekly-confluence-report--live-p state)
                          (funcall callback
                                   (funcall normalize
                                            "jira-updated-by-me-fallback" data))))
            :errback (lambda (&rest args)
                       (when (timfel/weekly-confluence-report--live-p state)
                         (funcall errback
                                  (format "Jira fallback query failed: %S" args))))))))
      (timfel/weekly-confluence-report--add-request
       state
       (jira-api-search
        :params (list (cons "jql" primary-jql)
                      (cons "maxResults" 150)
                      (cons "fields" fields))
        :callback (lambda (data _response)
                    (when (timfel/weekly-confluence-report--live-p state)
                      (funcall callback
                               (funcall normalize
                                        "jira-worked-or-closed-by-me" data))))
        :errback (lambda (&rest _args)
                   (when (timfel/weekly-confluence-report--live-p state)
                     (fallback))))))))

(defun timfel/weekly-confluence-report--extract-jira-keys (string)
  "Extract Jira issue keys from STRING."
  (let ((pos 0)
        keys
        (seen (make-hash-table :test #'equal))
        (case-fold-search nil))
    (while (and string
                (string-match "\\b[A-Z][A-Z0-9]+-[0-9]+\\b" string pos))
      (let ((key (match-string 0 string)))
        (unless (gethash key seen)
          (puthash key t seen)
          (push key keys)))
      (setq pos (match-end 0)))
    (nreverse keys)))

(defun timfel/weekly-confluence-report--millis-time (millis)
  "Convert MILLIS since epoch to Emacs time."
  (when (numberp millis)
    (seconds-to-time (/ millis 1000.0))))

(defun timfel/weekly-confluence-report--millis-date (millis)
  "Convert MILLIS since epoch to an ISO date string."
  (when-let* ((time (timfel/weekly-confluence-report--millis-time millis)))
    (timfel/weekly-confluence-report--date-string time)))

(defun timfel/weekly-confluence-report--pr-object (entry)
  "Return pull-request object from dashboard or raw Bitbucket ENTRY."
  (or (alist-get 'pullRequest entry)
      (timfel/weekly-confluence-report--alist-path entry '(mergeJob pullRequest))
      entry))

(defun timfel/weekly-confluence-report--pr-repo (pr)
  "Return target repository slug for PR."
  (or (timfel/weekly-confluence-report--alist-path pr '(toRef repository slug))
      (timfel/weekly-confluence-report--alist-path pr '(fromRef repository slug))))

(defun timfel/weekly-confluence-report--pr-url (pr)
  "Return browser URL for PR."
  (or (timfel/weekly-confluence-report--alist-path pr '(links self 0 href))
      (when-let* ((repo (timfel/weekly-confluence-report--pr-repo pr))
                  (id (alist-get 'id pr))
                  (_ (boundp 'ci-dashboard-base-url)))
        (format "%s/projects/G/repos/%s/pull-requests/%s"
                (string-remove-suffix "/" ci-dashboard-base-url) repo id))))

(defun timfel/weekly-confluence-report--normalize-pr (entry source)
  "Return compact report data for Bitbucket pull request ENTRY from SOURCE."
  (let* ((pr (timfel/weekly-confluence-report--pr-object entry))
         (title (alist-get 'title pr))
         (id (alist-get 'id pr))
         (repo (timfel/weekly-confluence-report--pr-repo pr))
         (state (alist-get 'state pr))
         (author (or (timfel/weekly-confluence-report--alist-path
                      pr '(author user displayName))
                     (timfel/weekly-confluence-report--alist-path
                      pr '(author user name))))
         (created (alist-get 'createdDate pr))
         (updated (alist-get 'updatedDate pr))
         (closed (alist-get 'closedDate pr))
         (from-branch (timfel/weekly-confluence-report--alist-path
                       pr '(fromRef displayId)))
         (to-branch (timfel/weekly-confluence-report--alist-path
                     pr '(toRef displayId)))
         (url (timfel/weekly-confluence-report--pr-url pr)))
    (list (cons "source" source)
          (cons "repository" repo)
          (cons "id" id)
          (cons "title" title)
          (cons "url" url)
          (cons "state" state)
          (cons "author" author)
          (cons "fromBranch" from-branch)
          (cons "toBranch" to-branch)
          (cons "createdDate" (timfel/weekly-confluence-report--millis-date created))
          (cons "updatedDate" (timfel/weekly-confluence-report--millis-date updated))
          (cons "closedDate" (timfel/weekly-confluence-report--millis-date closed))
          (cons "jiraKeys"
                (timfel/weekly-confluence-report--extract-jira-keys
                 (string-join (delq nil (list title from-branch to-branch url)) " "))))))

(defun timfel/weekly-confluence-report--pr-paths ()
  "Return Bitbucket paths used for weekly PR collection."
  (append
   '("/rest/ui/latest/dashboard/pull-requests?limit=100&state=OPEN&role=AUTHOR"
     "/rest/ui/latest/dashboard/pull-requests?limit=100&state=OPEN&role=REVIEWER"
     "/rest/ui/latest/dashboard/pull-requests?limit=100&state=MERGED&role=AUTHOR")
   (seq-mapcat
    (lambda (repo)
      (list
       (format "/rest/api/latest/projects/G/repos/%s/pull-requests?state=ALL&limit=100&order=NEWEST" repo)
       (format "/rest/api/latest/projects/G/repos/%s/pull-requests?state=MERGED&limit=100&order=NEWEST" repo)))
    timfel/weekly-confluence-report-pr-repositories)))

(defun timfel/weekly-confluence-report--bitbucket-request-async
    (state path callback errback)
  "Request Bitbucket PATH asynchronously, calling CALLBACK or ERRBACK."
  (require 'emacs-ci)
  (let ((full-url (concat (string-remove-suffix "/" ci-dashboard-base-url) path)))
    (message "Weekly report: downloading %s" full-url)
    (timfel/weekly-confluence-report--add-request
     state
     (ci--request-with-proxy-aware-settings
      full-url
      :headers (ci--auth-headers)
      :parser 'json-read
      :timeout timfel/weekly-confluence-report-request-timeout
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (when (timfel/weekly-confluence-report--live-p state)
                    (funcall callback data))))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (when (timfel/weekly-confluence-report--live-p state)
                  (funcall errback
                           (format "Bitbucket request failed for %s: %s"
                                   path error-thrown)))))))))

(defun timfel/weekly-confluence-report--collect-prs-async
    (state since until progress callback)
  "Collect recent/open PRs asynchronously from Bitbucket."
  (let* ((since-ms (floor (* 1000 (float-time since))))
         (until-ms (floor (* 1000 (float-time until))))
         (paths (timfel/weekly-confluence-report--pr-paths))
         (remaining (length paths))
         (entries nil)
         (errors nil)
         (seen (make-hash-table :test #'equal)))
    (cl-labels
        ((finish-one ()
           (setq remaining (1- remaining))
           (funcall progress "Bitbucket: %d/%d requests complete"
                    (- (length paths) remaining) (length paths))
           (when (<= remaining 0)
             (funcall callback
                      (list (cons "pullRequests" (nreverse entries))
                            (cons "errors" (nreverse errors))))))
         (record-data (path data)
           (let* ((values (alist-get 'values data))
                  (items (cond ((vectorp values) (append values nil))
                               ((listp values) values)
                               (t nil))))
             (dolist (entry items)
               (let* ((pr (timfel/weekly-confluence-report--pr-object entry))
                      (time-ms (or (alist-get 'closedDate pr)
                                   (alist-get 'updatedDate pr)
                                   (alist-get 'createdDate pr)
                                   0))
                      (open-p (string= (alist-get 'state pr) "OPEN"))
                      (include-p (or open-p
                                     (and (>= time-ms since-ms)
                                          (< time-ms until-ms))))
                      (normalized (and include-p
                                       (timfel/weekly-confluence-report--normalize-pr
                                        entry path)))
                      (key (and normalized
                                (format "%s/%s"
                                        (alist-get "repository" normalized nil nil #'string=)
                                        (alist-get "id" normalized nil nil #'string=)))))
                 (when (and normalized key (not (gethash key seen)))
                   (puthash key t seen)
                   (push normalized entries)))))))
      (if (null paths)
          (funcall callback (list (cons "pullRequests" nil)
                                  (cons "errors" nil)))
        (dolist (path paths)
          (let ((this-path path))
           (timfel/weekly-confluence-report--bitbucket-request-async
             state
             this-path
             (lambda (data)
               (record-data this-path data)
               (finish-one))
             (lambda (error-message)
               (push error-message errors)
               (finish-one)))))))))

(defun timfel/weekly-confluence-report--date-strings-in-range (since until)
  "Return ISO date strings from SINCE inclusive to UNTIL exclusive."
  (let ((date since)
        dates)
    (while (time-less-p date until)
      (push (timfel/weekly-confluence-report--date-string date) dates)
      (setq date (time-add date (days-to-time 1))))
    (nreverse dates)))

(defun timfel/weekly-confluence-report--subtree-snippet (&optional limit)
  "Return a compact snippet for the current Org subtree, capped at LIMIT chars."
  (let* ((limit (or limit 2000))
         (beg (line-end-position))
         (end (save-excursion (org-end-of-subtree t t)))
         (text (buffer-substring-no-properties beg (min end (+ beg limit)))))
    (string-trim
     (replace-regexp-in-string
      "[ \t\n]+" " "
      (replace-regexp-in-string
       "^[ \t]*:LOGBOOK:.*?:END:[ \t\n]*" "" text)))))

(defun timfel/weekly-confluence-report--org-heading-record (file &optional snippet-limit)
  "Return a report record for the current Org heading in FILE."
  (let ((properties (org-entry-properties nil 'standard)))
    (list (cons "file" file)
          (cons "line" (line-number-at-pos))
          (cons "todo" (org-get-todo-state))
          (cons "heading" (substring-no-properties
                           (org-get-heading t t t t)))
          (cons "tags" (org-get-tags))
          (cons "closed" (cdr (assoc "CLOSED" properties)))
          (cons "scheduled" (cdr (assoc "SCHEDULED" properties)))
          (cons "deadline" (cdr (assoc "DEADLINE" properties)))
          (cons "created" (cdr (assoc "CREATED" properties)))
          (cons "snippet"
                (timfel/weekly-confluence-report--subtree-snippet
                 snippet-limit)))))

(defun timfel/weekly-confluence-report--collect-todo-org-notes
    (file dates progress)
  "Collect weekly TODO records from FILE for DATES."
  (when progress
    (funcall progress "Scanning %s by dated task metadata/log lines" file))
  (let ((date-regexp (regexp-opt dates))
        (seen (make-hash-table :test #'eql))
        notes)
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward date-regexp nil t)
         (let ((hit (point)))
           (when (save-excursion
                   (beginning-of-line)
                   (or (looking-at-p "[ \t]*\\(?:CLOSED\\|SCHEDULED\\|DEADLINE\\):")
                       (looking-at-p "[ \t]*:Created:")
                       (looking-at-p "[ \t]*- State ")))
             (save-excursion
               (goto-char hit)
               (ignore-errors
                 (org-back-to-heading t)
                 (let ((pos (point)))
                   (unless (gethash pos seen)
                     (puthash pos t seen)
                     (push
                      (timfel/weekly-confluence-report--org-heading-record
                       file 1800)
                      notes))))))))))
    (nreverse notes)))

(defun timfel/weekly-confluence-report--collect-notes-org-notes
    (file dates progress)
  "Collect daily note records from FILE for DATES."
  (when progress
    (funcall progress "Scanning %s by daily date headings" file))
  (let ((heading-regexp
         (concat "^\\(\\*+\\) \\(" (regexp-opt dates) "\\)\\b"))
        notes)
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward heading-regexp nil t)
         (let ((date (match-string-no-properties 2)))
           (save-excursion
             (beginning-of-line)
             (push
              (append
               (timfel/weekly-confluence-report--org-heading-record
                file 6000)
               (list (cons "date" date)))
              notes))))))
    (nreverse notes)))

(defun timfel/weekly-confluence-report--collect-org-notes (since until &optional progress)
  "Collect relevant Org records from `org-agenda-files' for SINCE..UNTIL."
  (require 'org)
  (require 'org-agenda)
  (let ((dates (timfel/weekly-confluence-report--date-strings-in-range since until))
        notes)
    (dolist (file (org-agenda-files t))
      (when (file-readable-p file)
        (when progress
          (funcall progress "Reading Org agenda file %s" file))
        (setq notes
              (append
               notes
               (cond
                ((string= (file-name-nondirectory file) "notes.org")
                 (timfel/weekly-confluence-report--collect-notes-org-notes
                  file dates progress))
                ((string= (file-name-nondirectory file) "todo.org")
                 (timfel/weekly-confluence-report--collect-todo-org-notes
                  file dates progress))
                (t nil))))))
    (nreverse notes)))

(defun timfel/weekly-confluence-report--write-json (file value)
  "Write VALUE as pretty JSON to FILE."
  (let ((json-encoding-pretty-print t))
    (with-temp-file file
      (insert (json-encode value))
      (insert "\n"))))

(defun timfel/weekly-confluence-report--slack-agent-prompt (since-date until-date output-file)
  "Return prompt for the Slack collection agent."
  (format
   (string-join
    (list
     "You are collecting Slack context for Tim's weekly Confluence update."
     ""
     "Use Slack access available to this agent. Search channel %s for messages from %s through before %s."
     ""
     "Collect only factual work signals:"
     "- Jira issues closed by the Graal Python team or mentioned as done/landed/resolved."
     "- Pull requests mentioned as merged, reviewed, blocked, or needing follow-up."
     "- Short notes that explain why an issue or PR mattered."
     ""
     "Write the result to this exact file in the current directory:"
     "%s"
     ""
     "Output file format:"
     "# Slack context"
     "- Jira: KEY - title/status if visible - short reason from Slack - date/author/permalink if available"
     "- PR: URL or repo#id - title/status if visible - short reason from Slack - date/author/permalink if available"
     "- Note: one short line, only if it does not duplicate a Jira or PR item"
     ""
     "End the file with a line containing exactly SLACK_CONTEXT_DONE."
     "If Slack is unavailable or access fails, still write the file with SLACK_UNAVAILABLE: reason, then SLACK_CONTEXT_DONE."
     "Do not build the Confluence report. Do not include secrets or private tokens.")
    "\n")
   timfel/weekly-confluence-report-slack-channel since-date until-date output-file))

(defun timfel/weekly-confluence-report--start-slack-agent (since-date until-date)
  "Start a temporary agent-shell to collect Slack data."
  (require 'agent-shell)
  (defvar agent-shell-preferred-agent-config)
  (let* ((agent-shell-preferred-agent-config 'codex)
         (buffer (agent-shell-new-temp-shell))
         (directory (with-current-buffer buffer
                      (file-name-as-directory default-directory)))
         (slack-file (expand-file-name "slack-context.md" directory))
         (prompt (timfel/weekly-confluence-report--slack-agent-prompt
                  since-date until-date slack-file)))
    (list (cons "buffer" (buffer-name buffer))
          (cons "bufferObject" buffer)
          (cons "directory" directory)
          (cons "slackFile" slack-file)
          (cons "prompt" prompt))))

(defun timfel/weekly-confluence-report--slack-ready-p (file)
  "Return non-nil when Slack context FILE exists and is marked done."
  (and (file-readable-p file)
       (with-temp-buffer
         (insert-file-contents file)
         (goto-char (point-min))
         (search-forward "SLACK_CONTEXT_DONE" nil t))))

(defun timfel/weekly-confluence-report--uuid-from-string (string)
  "Return deterministic UUID-looking id from STRING."
  (let ((hash (secure-hash 'md5 string)))
    (format "%s-%s-%s-%s-%s"
            (substring hash 0 8)
            (substring hash 8 12)
            (substring hash 12 16)
            (substring hash 16 20)
            (substring hash 20 32))))

(defun timfel/weekly-confluence-report--jira-macro (key ordinal)
  "Return a Confluence Jira macro for KEY and ORDINAL."
  (format
   (concat "<ac:structured-macro ac:macro-id=\"%s\" ac:name=\"jira\" ac:schema-version=\"1\">"
           "<ac:parameter ac:name=\"server\">%s</ac:parameter>"
           "<ac:parameter ac:name=\"serverId\">%s</ac:parameter>"
           "<ac:parameter ac:name=\"key\">%s</ac:parameter>"
           "</ac:structured-macro>")
   (timfel/weekly-confluence-report--uuid-from-string
    (format "%s:%s" key ordinal))
   timfel/weekly-confluence-report-jira-server
   timfel/weekly-confluence-report-jira-server-id
   key))

(defun timfel/weekly-confluence-report--strip-code-fences (text)
  "Strip a single wrapping Markdown code fence from TEXT."
  (let ((trimmed (string-trim text)))
    (if (string-prefix-p "```" trimmed)
        (with-temp-buffer
          (insert trimmed)
          (goto-char (point-min))
          (when (looking-at "```.*$")
            (delete-region (point) (min (point-max) (1+ (line-end-position)))))
          (goto-char (point-max))
          (when (re-search-backward "^```[ \t]*\\'" nil t)
            (delete-region (line-beginning-position) (point-max)))
          (string-trim (buffer-string)))
      trimmed)))

(defun timfel/weekly-confluence-report--expand-jira-placeholders (text)
  "Replace {{jira:KEY}} placeholders in TEXT with Confluence macros."
  (let ((ordinal 0)
        (start 0)
        (result (timfel/weekly-confluence-report--strip-code-fences text)))
    (while (string-match "{{jira:\\([A-Z][A-Z0-9]+-[0-9]+\\)}}" result start)
      (setq ordinal (1+ ordinal))
      (let ((macro (timfel/weekly-confluence-report--jira-macro
                    (match-string 1 result) ordinal)))
        (setq result (replace-match macro t t result))
        (setq start (+ (match-beginning 0) (length macro)))))
    result))

(defun timfel/weekly-confluence-report--final-prompt (local-json slack-context)
  "Return the final gptel prompt from LOCAL-JSON and SLACK-CONTEXT."
  (string-join
   (list
    "Build Tim's weekly Confluence update as a Confluence storage-format XHTML fragment."
    ""
    "Inputs:"
    "1. Local JSON: Jira issues worked on/closed by Tim, PRs from Bitbucket/CI, and Org notes/TODOs."
    "2. Slack context: issues and PRs mentioned in the Graal Python weekly Slack channel."
    ""
    "Hard output rules:"
    "- Output only the final XHTML fragment. No Markdown fences. No explanation."
    "- Use `<p>...</p>` paragraphs, `<br/>` line breaks inside paragraphs, and blank lines between concern groups."
    "- Group related items by area by ordering them together. Use no headings unless an area has several entries and a heading materially helps."
    "- Deduplicate aggressively by Jira key, PR URL, repository/id, and title."
    "- Prefer Jira issue macros over Jira URLs. Whenever you reference a Jira issue, write exactly `{{jira:KEY}}`; do not write Confluence macro XML yourself."
    "- The caller will replace `{{jira:KEY}}` placeholders with structured Confluence Jira macros."
    "- For pull requests, use HTML links: `<a href=\"URL\">short title</a>`."
    "- Escape plain ampersands as `&amp;` in text and links."
    "- For each item, keep notes short: one line, or at most two lines if the nuance matters."
    "- If a Jira title is already descriptive, the paragraph can be just the Jira placeholder or Jira placeholder plus PR link."
    "- If a title is vague or there is important extra context, add `<br/>&#160; - SHORT note`."
    "- If an item has no Jira issue, include it as a short standalone note, optionally with a PR link."
    "- Mention people as `@Name` only when ownership/review context is clear and useful."
    "- Drop low-signal TODOs, stale notes, duplicate status chatter, and CI-only noise unless it explains a user-visible outcome or blocker."
    ""
    "Shape examples:"
    "<p>@Roland: Wheel and SOABI platform tags (<a href=\"https://example/pr/13\">Merge request</a>)<br/>&#160; - Follow the platform tag spec so downstream projects can adopt it without recurring patches.</p>"
    "<p>{{jira:GR-12345}} &amp; {{jira:GRAALOS-6789}}<br/>&#160; - Shared fix for packaging fallout.</p>"
    "<p>@Chumer<br/>&#160; - Question whether JIT modes should be a Truffle-level setting for polyglot embeddings.</p>"
    ""
    "Local JSON:"
    "```json"
    local-json
    "```"
    ""
    "Slack context:"
    "```text"
    slack-context
    "```")
   "\n"))

(defun timfel/weekly-confluence-report--expand-jira-placeholders-in-region (beg end)
  "Expand Jira placeholders between BEG and END in the current buffer."
  (let ((expanded (timfel/weekly-confluence-report--expand-jira-placeholders
                   (buffer-substring-no-properties beg end))))
    (delete-region beg end)
    (goto-char beg)
    (insert expanded)))

(defun timfel/weekly-confluence-report--open-gptel (state directory)
  "Open and send the weekly Confluence gptel prompt from DIRECTORY."
  (require 'gptel)
  (let* ((directory (file-name-as-directory directory))
         (local-file (expand-file-name "local-context.json" directory))
         (slack-file (expand-file-name "slack-context.md" directory))
         (prompt-file (expand-file-name "final-prompt.md" directory))
         (buffer (gptel "*weekly Confluence report gptel*" nil nil t)))
    (puthash :gptel-buffer buffer state)
    (unless (file-readable-p local-file)
      (user-error "Missing local context file: %s" local-file))
    (unless (file-readable-p slack-file)
      (user-error "Missing Slack context file: %s" slack-file))
    (let* ((local-json (with-temp-buffer
                         (insert-file-contents local-file)
                         (buffer-string)))
           (slack-context (with-temp-buffer
                            (insert-file-contents slack-file)
                            (buffer-string)))
           (prompt (timfel/weekly-confluence-report--final-prompt
                    local-json slack-context)))
      (with-temp-file prompt-file
        (insert prompt))
      (with-current-buffer buffer
        (erase-buffer)
        ;; This report must not inherit a stale model from a reused gptel buffer.
        (setq-local gptel-model 'gpt-5.6-luna)
        (setq-local gptel-use-tools nil)
        (setq-local gptel-use-context nil)
        (setq-local gptel-include-reasoning nil)
        (add-hook 'gptel-post-response-functions
                  #'timfel/weekly-confluence-report--expand-jira-placeholders-in-region
                  nil t)
        (insert prompt)
        (goto-char (point-max)))
      (pop-to-buffer buffer)
      (message "Weekly report: sending gptel prompt from %s" prompt-file)
      (with-current-buffer buffer
        (gptel-send)))))

(defun timfel/weekly-confluence-report--progress-buffer ()
  "Return the weekly Confluence progress buffer."
  (get-buffer-create "*weekly Confluence report*"))

(defun timfel/weekly-confluence-report--progress (state format-string &rest args)
  "Log a progress message for STATE using FORMAT-STRING and ARGS."
  (unless (gethash :cancelled state)
    (let ((message-text (apply #'format format-string args)))
      (message "Weekly report: %s" message-text)
      (when (buffer-live-p (timfel/weekly-confluence-report--progress-buffer))
        (with-current-buffer (timfel/weekly-confluence-report--progress-buffer)
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert (format-time-string "[%H:%M:%S] ")
                    message-text "\n")))))))

(defun timfel/weekly-confluence-report--add-timer (state timer)
  "Track TIMER for STATE cleanup and return it."
  (when timer
    (puthash :timers (cons timer (gethash :timers state)) state))
  timer)

(defun timfel/weekly-confluence-report--add-request (state request)
  "Track REQUEST for STATE cleanup and return it."
  (when request
    (puthash :requests (cons request (gethash :requests state)) state))
  request)

(defun timfel/weekly-confluence-report--add-subscription (state buffer token)
  "Track agent-shell subscription TOKEN in BUFFER for STATE cleanup."
  (when (and buffer token)
    (puthash :subscriptions
             (cons (cons buffer token) (gethash :subscriptions state))
             state))
  token)

(defun timfel/weekly-confluence-report--live-p (state)
  "Return non-nil if weekly report STATE is still active."
  (not (gethash :cancelled state)))

(defun timfel/weekly-confluence-report--cleanup (state &optional reason)
  "Cancel timers, requests, and owned buffers for STATE.
REASON is a short human-readable string."
  (unless (gethash :cancelled state)
    (puthash :cancelled t state)
    (message "Weekly report cancelled%s"
             (if reason (format ": %s" reason) ""))
    (dolist (timer (gethash :timers state))
      (when (timerp timer)
        (ignore-errors (cancel-timer timer))))
    (dolist (request (gethash :requests state))
      (when (and (fboundp 'request-response-p)
                 (request-response-p request))
        (ignore-errors (request-abort request))))
    (dolist (subscription (gethash :subscriptions state))
      (let ((buffer (car subscription))
            (token (cdr subscription)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (ignore-errors
              (agent-shell-unsubscribe :subscription token))))))
    (when-let* ((slack-buffer (gethash :slack-buffer-object state)))
      (when (buffer-live-p slack-buffer)
        (ignore-errors (kill-buffer slack-buffer))))
    (when-let* ((gptel-buffer (gethash :gptel-buffer state)))
      (when (buffer-live-p gptel-buffer)
        (ignore-errors
          (with-current-buffer gptel-buffer
            (when (fboundp 'gptel-abort)
              (gptel-abort gptel-buffer))))
        (ignore-errors (kill-buffer gptel-buffer))))))

(defun timfel/weekly-confluence-report--progress-buffer-killed ()
  "Clean up weekly report state when the progress buffer is killed."
  (when (and (boundp 'timfel/weekly-confluence-report--state)
             timfel/weekly-confluence-report--state)
    (timfel/weekly-confluence-report--cleanup
     timfel/weekly-confluence-report--state "progress buffer killed")))

(defun timfel/weekly-confluence-report--source-done-p (state source)
  "Return non-nil when SOURCE is already done in STATE."
  (gethash source (gethash :done state)))

(defun timfel/weekly-confluence-report--source-done (state source value)
  "Record SOURCE VALUE in STATE and continue when all sources are done."
  (when (and (timfel/weekly-confluence-report--live-p state)
             (not (timfel/weekly-confluence-report--source-done-p state source)))
    (puthash source t (gethash :done state))
    (puthash source value (gethash :results state))
    (puthash :pending (1- (gethash :pending state)) state)
    (timfel/weekly-confluence-report--progress
     state "%s complete (%d remaining)" source (gethash :pending state))
    (when (<= (gethash :pending state) 0)
      (timfel/weekly-confluence-report--finish-collection state))))

(defun timfel/weekly-confluence-report--source-timeout (state source seconds)
  "Mark SOURCE timed out in STATE after SECONDS unless already complete."
  (timfel/weekly-confluence-report--add-timer
   state
   (run-at-time
    seconds nil
    (lambda ()
      (when (and (timfel/weekly-confluence-report--live-p state)
                 (not (timfel/weekly-confluence-report--source-done-p
                       state source)))
        (timfel/weekly-confluence-report--progress
         state "%s timed out after %s seconds; continuing without it"
         source seconds)
        (timfel/weekly-confluence-report--source-done
         state source
         (list (cons "error" (format "%s timed out after %s seconds"
                                     source seconds)))))))))

(defun timfel/weekly-confluence-report--slack-agent-ready-p (buffer)
  "Return non-nil when BUFFER's agent-shell session can accept input."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'agent-shell-mode)
              (boundp 'agent-shell--state)
              (map-nested-elt agent-shell--state '(:session :id))
              (eq (agent-shell-status :shell-buffer buffer) 'ready)
              (shell-maker-point-at-last-prompt-p)
              (save-excursion
                (goto-char (point-min))
                (and (search-forward "▶ Agent capabilities" nil t)
                     (search-forward "Available /commands" nil t)))))))

(defun timfel/weekly-confluence-report--submit-slack-agent (state)
  "Submit the Slack collection prompt in STATE when agent-shell is ready."
  (let ((buffer (gethash :slack-buffer-object state)))
    (cond
     ((gethash :slack-submitted state) t)
     ((not (buffer-live-p buffer))
      (puthash :slack-submitted t state)
      (timfel/weekly-confluence-report--progress
       state "Slack agent buffer died before prompt submission")
      nil)
     ((timfel/weekly-confluence-report--slack-agent-ready-p buffer)
      (puthash :slack-submitted t state)
      (timfel/weekly-confluence-report--progress
       state "Submitting Slack collection prompt to %s" (buffer-name buffer))
      (agent-shell-insert
       :text (gethash :slack-prompt state)
       :submit t
       :shell-buffer buffer)
      t)
     (t nil))))

(defun timfel/weekly-confluence-report--submit-slack-agent-after-prompt-ready (state)
  "Submit Slack prompt after agent-shell reports the shell prompt is ready."
  (puthash :slack-prompt-ready t state)
  (timfel/weekly-confluence-report--progress
   state "Slack agent-shell prompt ready")
  (timfel/weekly-confluence-report--submit-slack-agent state))

(defun timfel/weekly-confluence-report--maybe-submit-slack-agent-fallback (state)
  "Fallback submit path when the agent-shell prompt-ready event was missed."
  (when (and (timfel/weekly-confluence-report--live-p state)
             (not (gethash :slack-prompt-ready state))
             (timfel/weekly-confluence-report--slack-agent-ready-p
              (gethash :slack-buffer-object state)))
    (timfel/weekly-confluence-report--submit-slack-agent-after-prompt-ready
     state)))

(defun timfel/weekly-confluence-report--poll-slack-agent-ready (state)
  "Poll until Slack agent-shell in STATE is ready, then submit its prompt."
  (when (and (timfel/weekly-confluence-report--live-p state)
             (gethash :slack-prompt-ready state)
             (not (timfel/weekly-confluence-report--submit-slack-agent state)))
    (timfel/weekly-confluence-report--progress
     state "Waiting for Slack agent-shell prompt readiness")
    (timfel/weekly-confluence-report--add-timer
     state
     (run-at-time 3 nil
                  #'timfel/weekly-confluence-report--poll-slack-agent-ready
                  state))))

(defun timfel/weekly-confluence-report--finish-collection (state)
  "Write collected STATE and open the final gptel buffer."
  (when (timfel/weekly-confluence-report--live-p state)
    (let* ((results (gethash :results state))
         (directory (gethash :directory state))
         (local-file (gethash :local-file state))
         (slack-file (gethash :slack-file state))
         (jira (gethash 'jira results))
         (bitbucket (gethash 'bitbucket results))
         (org-notes (gethash 'org results))
         (slack (gethash 'slack results))
         (local-context
          (list
           (cons "period"
                 (list (cons "since" (gethash :since-date state))
                       (cons "untilExclusive"
                             (gethash :until-exclusive-date state))))
           (cons "jiraIssues" jira)
           (cons "pullRequests" (alist-get "pullRequests" bitbucket nil nil #'string=))
           (cons "pullRequestErrors" (alist-get "errors" bitbucket nil nil #'string=))
           (cons "orgNotes" org-notes))))
    (timfel/weekly-confluence-report--write-json local-file local-context)
    (with-temp-file slack-file
      (insert (or slack "SLACK_UNAVAILABLE: no Slack context was collected\nSLACK_CONTEXT_DONE\n")))
    (timfel/weekly-confluence-report--progress state "Local context written to %s" local-file)
    (timfel/weekly-confluence-report--progress state "Slack context available at %s" slack-file)
    (timfel/weekly-confluence-report--open-gptel state directory))))

(defun timfel/weekly-confluence-report--poll-slack (state)
  "Poll Slack collection file for STATE."
  (when (timfel/weekly-confluence-report--live-p state)
    (let* ((slack-file (gethash :slack-file state))
           (started (gethash :started state))
           (elapsed (float-time (time-subtract (current-time) started))))
      (cond
       ((timfel/weekly-confluence-report--source-done-p state 'slack)
        nil)
       ((timfel/weekly-confluence-report--slack-ready-p slack-file)
        (timfel/weekly-confluence-report--progress
         state "Slack context file is ready")
        (timfel/weekly-confluence-report--source-done
         state 'slack
         (with-temp-buffer
           (insert-file-contents slack-file)
           (buffer-string))))
       ((> elapsed timfel/weekly-confluence-report-slack-timeout)
        (timfel/weekly-confluence-report--progress
         state "Slack collection timed out after %s seconds"
         timfel/weekly-confluence-report-slack-timeout)
        (timfel/weekly-confluence-report--source-done
         state 'slack
         "SLACK_UNAVAILABLE: timed out waiting for agent-shell Slack collection\nSLACK_CONTEXT_DONE\n"))
       (t
        (timfel/weekly-confluence-report--progress
         state "Waiting for Slack context file %s" slack-file)
        (timfel/weekly-confluence-report--add-timer
         state
         (run-at-time 10 nil
                      #'timfel/weekly-confluence-report--poll-slack
                      state)))))))

;;;###autoload
(defun timfel/weekly-confluence-report-finalize-from-directory (directory)
  "Finalize a weekly Confluence report from context files in DIRECTORY."
  (interactive "DContext directory: ")
  (let ((state (make-hash-table :test #'eq)))
    (puthash :timers nil state)
    (puthash :requests nil state)
    (puthash :subscriptions nil state)
    (timfel/weekly-confluence-report--open-gptel state directory)))

;;;###autoload
(defun timfel/weekly-confluence-report (since-date until-date)
  "Collect weekly context and build a Confluence update.
SINCE-DATE and UNTIL-DATE are ISO dates; UNTIL-DATE is inclusive
for the interactive prompt and converted to an exclusive bound internally."
  (interactive
   (let* ((today (current-time))
          (default-since (timfel/weekly-confluence-report--date-string
                          (time-subtract today (days-to-time 7))))
          (default-until (timfel/weekly-confluence-report--date-string today)))
     (list
      (read-string "Weekly report since date: " default-since)
      (read-string "Weekly report through date: " default-until))))
  (let* ((since (timfel/weekly-confluence-report--parse-date since-date))
         (until (timfel/weekly-confluence-report--parse-date until-date t))
         (since-date (timfel/weekly-confluence-report--date-string since))
         (until-exclusive-date (timfel/weekly-confluence-report--date-string until))
         (slack-state (timfel/weekly-confluence-report--start-slack-agent
                       since-date until-exclusive-date))
         (directory (alist-get "directory" slack-state nil nil #'string=))
         (state (make-hash-table :test #'eq)))
    (puthash :started (current-time) state)
    (puthash :timers nil state)
    (puthash :requests nil state)
    (puthash :subscriptions nil state)
    (puthash :since since state)
    (puthash :until until state)
    (puthash :since-date since-date state)
    (puthash :until-exclusive-date until-exclusive-date state)
    (puthash :directory directory state)
    (puthash :local-file (expand-file-name "local-context.json" directory) state)
    (puthash :slack-file (alist-get "slackFile" slack-state nil nil #'string=) state)
    (puthash :slack-buffer (alist-get "buffer" slack-state nil nil #'string=) state)
    (puthash :slack-buffer-object
             (alist-get "bufferObject" slack-state nil nil #'string=)
             state)
    (puthash :slack-prompt (alist-get "prompt" slack-state nil nil #'string=) state)
    (puthash :pending 4 state)
    (puthash :done (make-hash-table :test #'eq) state)
    (puthash :results (make-hash-table :test #'eq) state)
    (with-current-buffer (timfel/weekly-confluence-report--progress-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq-local timfel/weekly-confluence-report--state state)
        (add-hook 'kill-buffer-hook
                  #'timfel/weekly-confluence-report--progress-buffer-killed
                  nil t)
        (insert "Collecting weekly report context asynchronously.\n\n")
        (insert "Local context: " (gethash :local-file state) "\n")
        (insert "Slack context: " (gethash :slack-file state) "\n")
        (insert "Slack agent: " (gethash :slack-buffer state) "\n")
        (insert "Period: " since-date " through before "
                until-exclusive-date "\n\n")))
    (timfel/weekly-confluence-report--progress state "Reading Org agenda files")
    (timfel/weekly-confluence-report--add-timer
     state
     (run-at-time
      0 nil
      (lambda ()
        (when (timfel/weekly-confluence-report--live-p state)
          (condition-case err
              (let ((notes (timfel/weekly-confluence-report--collect-org-notes
                            since until
                            (lambda (fmt &rest args)
                              (apply #'timfel/weekly-confluence-report--progress
                                     state fmt args)))))
                (timfel/weekly-confluence-report--progress
                 state "Org notes complete: %d entries" (length notes))
                (timfel/weekly-confluence-report--source-done state 'org notes))
            (error
             (timfel/weekly-confluence-report--source-done
              state 'org (list (cons "error" (error-message-string err))))))))))
    (timfel/weekly-confluence-report--progress state "Starting Jira query")
    (timfel/weekly-confluence-report--source-timeout
     state 'jira timfel/weekly-confluence-report-request-timeout)
    (timfel/weekly-confluence-report--collect-jira-async
     state since-date until-exclusive-date
     (lambda (issues)
       (timfel/weekly-confluence-report--progress
        state "Jira query complete: %d issues" (length issues))
       (timfel/weekly-confluence-report--source-done state 'jira issues))
     (lambda (error-message)
       (timfel/weekly-confluence-report--progress
        state "Jira query failed: %s" error-message)
       (timfel/weekly-confluence-report--source-done
        state 'jira (list (cons "error" error-message)))))
    (timfel/weekly-confluence-report--progress state "Starting Bitbucket PR queries")
    (timfel/weekly-confluence-report--collect-prs-async
     state since until
     (lambda (fmt &rest args)
       (apply #'timfel/weekly-confluence-report--progress state fmt args))
     (lambda (result)
       (timfel/weekly-confluence-report--progress
        state "Bitbucket complete: %d PRs, %d errors"
        (length (alist-get "pullRequests" result nil nil #'string=))
        (length (alist-get "errors" result nil nil #'string=)))
       (timfel/weekly-confluence-report--source-done state 'bitbucket result)))
    (timfel/weekly-confluence-report--progress state "Slack agent-shell started")
    (timfel/weekly-confluence-report--add-subscription
     state
     (gethash :slack-buffer-object state)
     (agent-shell-subscribe-to
      :shell-buffer (gethash :slack-buffer-object state)
      :event 'prompt-ready
      :on-event (lambda (_event)
                  (timfel/weekly-confluence-report--submit-slack-agent-after-prompt-ready
                   state))))
    (timfel/weekly-confluence-report--add-timer
     state
     (run-at-time 12 nil
                  #'timfel/weekly-confluence-report--maybe-submit-slack-agent-fallback
                  state))
    (timfel/weekly-confluence-report--add-timer
     state
     (run-at-time 10 nil #'timfel/weekly-confluence-report--poll-slack state))
    state))

;;;###autoload
(defun timfel/gptel-open-agents-orchestration (&optional inline?)
  "Open a gptel chat buffer configured for the AGENTS.md orchestration flow.

With prefix argument INLINE?, just send the current buffer and see.
`timfel/gptel-orchestration-buffer-name'."
  (interactive "P")
  (let* ((default-directory (expand-file-name (locate-user-emacs-file "")))
         (buffer-name timfel/gptel-orchestration-buffer-name)
         (buffer (if inline? (current-buffer) (gptel buffer-name nil nil t))))
    (with-current-buffer buffer
      (gptel--apply-preset
       'agents-orchestration
       (lambda (sym val)
         (set (make-local-variable sym) val))))

    (if inline?
        (gptel-request
            (if (use-region-p)
                (buffer-substring-no-properties (region-beginning)
                                                (region-end))
              (buffer-substring-no-properties (point-min)
                                              (point)))
          :stream gptel-stream))
    buffer))

(setq timfel/gptel-orchestration-tools
  (list
   (gptel-make-tool
    :name "inspect_current_work_context"
    :function #'timfel/gptel-tool-inspect-current-work-context
    :description
    "Inspect the user's current Emacs work context. Use this when the user asks about work they are currently doing. Returns interesting buffers, agent-shell buffers, and the recently persisted live agent-shell directory set."
    :args (list
           '(:name "limit"
             :type integer
             :description "Optional maximum number of buffers to return."
             :optional t))
    :category "orchestration"
    :confirm nil
    :include t)
   (gptel-make-tool
    :name "open_work_queues"
    :function #'timfel/gptel-tool-open-work-queues
    :description
    "Open the user's work queues directly inside Emacs instead of telling them to run commands. Use this when the user asks about work they have to do. Valid sources are jira, ci, and agenda."
    :args (list
           '(:name "sources"
             :type array
             :description "Optional list of queue sources to open."
             :items (:type string)
             :optional t))
    :category "orchestration"
    :confirm nil
    :include t)
   (gptel-make-tool
    :name "start_worktree_tasks"
    :function #'timfel/gptel-tool-start-worktree-tasks
    :description
    "Start one or more agent-shell tasks. If no directory is provided, this prompts with `read-directory-name` using the prompt `work where: `. When an explicit directory is provided, a single task starts directly in that absolute directory."
    :args (list
           '(:name "tasks"
             :type array
             :description "Task specs to start."
             :items (:type object
                     :properties (:title (:type string
                                   :description "Short task title for the worktree or buffer.")
                                  :prompt (:type string
                                   :description "Initial agent request and context for the task."))
                     :required ["title" "prompt"]))
           '(:name "directory"
             :type string
             :description "Optional repository or project directory. When omitted, Emacs prompts interactively."
             :optional t)
           '(:name "resume_previous_session"
             :type boolean
             :description "Optional flag to select if this should resume a previous session if any (the default), or if this is a new task."
             :optional t))
    :category "orchestration"
    :confirm t
    :include t)
   (gptel-make-tool
    :name "bitbucket"
    :function #'timfel/gptel-tool-bitbucket
    :confirm t
    :include t
    :category "orchestration"
    :description
    "A CLI tool to access bitbucket. It has subcommands and the argument is passed as argv"
    :args '((:name "argv" :type array :description "arguments to pass" :items (:type string))))
   (gptel-make-tool
    :name "jira"
    :function #'timfel/gptel-tool-jira
    :confirm t
    :include t
    :category "orchestration"
    :description
    "A CLI tool to access Jira. It has subcommand and the argument is passed as argv"
    :args '((:name "argv" :type array :description "arguments to pass" :items (:type string))))
   (gptel-make-tool
    :name "evaluate_workspace_elisp"
    :function #'timfel/gptel-tool-evaluate-workspace-elisp
    :description
    "Evaluate Elisp directly in the live Emacs workspace and return the printed value of the last form. Use this to inspect and read buffers or query editor state when the built-in orchestration tools are not enough. IMPORTANT: This runs arbitrary Elisp in the user's Emacs session; confirmation is required."
    :args (list
           '(:name "code"
             :type string
             :description "One or more Elisp forms to evaluate in the live Emacs session."))
    :category "orchestration"
    :confirm t
    :include t)))

(gptel-make-preset
 'agents-orchestration
 :description "Use Emacs-native orchestration tools for current work, work queues, worktree task startup, and direct live-workspace Elisp evaluation."
 :system 'agents-orchestration
 :tools (append (mapcar #'gptel-tool-name timfel/gptel-orchestration-tools)
                '("create_file"))
 :use-tools t)

(provide 'timfel-gptel-orchestration)

;;; timfel-gptel-orchestration.el ends here
