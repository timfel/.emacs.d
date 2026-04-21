;;; timfel-org.el --- Org helpers for Tim         -*- lexical-binding: t -*-

;; Copyright (C) 2026 Tim Felgentreff <timfelgentreff@gmail.com>
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, version 3.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see https://www.gnu.org/licenses/.

;;; Commentary:

;; Org capture and dynamic block helpers.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(declare-function ci--section-find-type "emacs-ci" (typ section))
(declare-function ci-dashboard "emacs-ci" ())
(declare-function jira-api-call "jira-api"
                  (verb endpoint &key params data callback parser sync error complete))
(declare-function jira-detail-show-issue "jira-detail" (key))
(declare-function jira-doc-markup "jira-doc" (doc))
(declare-function jira-utils-marked-item "jira-utils" ())
(declare-function magit-current-section "magit-section" ())
(declare-function org-before-first-heading-p "org" ())
(declare-function org-capture-get "org-capture" (property &optional local))
(declare-function org-end-of-subtree "org" (&optional invisible-ok to-heading))
(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))
(declare-function org-update-all-dblocks "org" (&optional arg))
(defvar ci-dashboard-base-url)
(defvar jira-detail--current)
(defvar jira-detail--current-key)
(defvar jira-issues-key-summary-map)
(defvar magit-root-section)
(defvar org-capture-last-stored-marker)

(defcustom timfel/org-refresh-cache-seconds 30
  "Keep Jira and CI refresh payloads for this many seconds."
  :type 'integer
  :group 'timfel)

(defvar timfel/org-jira--cache (make-hash-table :test #'equal)
  "Cached Jira issue payloads.")

(defvar timfel/org-jira--inflight (make-hash-table :test #'equal)
  "Callbacks waiting on an in-flight Jira issue request.")

(defvar timfel/org-ci--cache (make-hash-table :test #'equal)
  "Cached CI payloads keyed by project/repo/pr.")

(defvar timfel/org-ci--inflight (make-hash-table :test #'equal)
  "Callbacks waiting on in-flight CI payloads.")

(defun timfel/org-capture--original-buffer ()
  "Return the original buffer for the current capture, or current buffer."
  (let ((original (and (fboundp 'org-capture-get)
                       (ignore-errors (org-capture-get :original-buffer)))))
    (if (buffer-live-p original)
        original
      (current-buffer))))

(defun timfel/org-capture--line-bounds-around-point (&optional lines)
  "Return `(START . END)' around point spanning LINES on each side."
  (let ((lines (or lines 10)))
    (cons
     (save-excursion
       (forward-line (- lines))
       (line-beginning-position))
     (save-excursion
       (forward-line lines)
       (line-end-position)))))

(defun timfel/org-capture--context-snippet (&optional lines)
  "Return a snippet around point spanning LINES on each side."
  (pcase-let ((`(,start . ,end) (timfel/org-capture--line-bounds-around-point lines)))
    (let ((snippet (string-trim-right
                    (buffer-substring-no-properties start end))))
      (if (string-empty-p snippet)
          ""
        (format "#+begin_example\n%s\n#+end_example\n" snippet)))))

(defun timfel/org-capture--active-region-string ()
  "Return the active region as plain text, or nil."
  (when (use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end))))

(defun timfel/org--data-get (data key)
  "Return KEY from DATA, supporting alists and hash tables."
  (cond
   ((hash-table-p data)
    (or (gethash key data)
        (and (symbolp key) (gethash (symbol-name key) data))
        (and (stringp key) (gethash (intern key) data))))
   ((listp data)
    (or (alist-get key data)
        (and (stringp key) (alist-get (intern key) data))
        (and (symbolp key) (alist-get (symbol-name key) data nil nil #'equal))))
   (t nil)))

(defun timfel/org--data-get-in (data &rest keys)
  "Return nested KEYS from DATA."
  (let ((value data))
    (while (and value keys)
      (setq value (timfel/org--data-get value (pop keys))))
    value))

(defun timfel/org--cache-get (table key)
  "Return non-expired cached value from TABLE for KEY."
  (when-let ((entry (gethash key table)))
    (pcase-let ((`(,timestamp . ,value) entry))
      (if (< (- (float-time) timestamp) timfel/org-refresh-cache-seconds)
          value
        (remhash key table)
        nil))))

(defun timfel/org--cache-put (table key value)
  "Cache VALUE in TABLE for KEY."
  (puthash key (cons (float-time) value) table)
  value)

(defun timfel/org--cleanup-text (value)
  "Return VALUE as a cleaned-up plain string."
  (let ((text (cond
               ((null value) "")
               ((stringp value) value)
               (t (format "%s" value)))))
    (thread-last
      text
      (replace-regexp-in-string "\r" "")
      (replace-regexp-in-string "[ \t]+\n" "\n")
      string-trim-right)))

(defun timfel/org--insert-paragraph (text)
  "Insert TEXT as a paragraph if it is not empty."
  (let ((text (timfel/org--cleanup-text text)))
    (unless (string-empty-p text)
      (insert text "\n\n"))))

(defun timfel/org--insert-list-item (prefix text)
  "Insert TEXT as a list item using PREFIX."
  (let ((text (timfel/org--cleanup-text text)))
    (unless (string-empty-p text)
      (insert prefix
              (replace-regexp-in-string "\n" (concat "\n" (make-string (length prefix) ?\s))
                                        text)
              "\n"))))

(defun timfel/org--render-to-string (fn &rest args)
  "Return the string produced by calling FN with ARGS in a temp buffer."
  (with-temp-buffer
    (apply fn args)
    (buffer-string)))

(defun timfel/org-jira--key-at-point ()
  "Return the Jira issue key represented by point in the current buffer."
  (cond
   ((derived-mode-p 'jira-detail-mode)
    jira-detail--current-key)
   ((derived-mode-p 'jira-issues-mode)
    (jira-utils-marked-item))
   (t nil)))

(defun timfel/org-jira--title-at-point ()
  "Return the Jira issue title represented by point."
  (let ((key (timfel/org-jira--key-at-point)))
    (cond
     ((derived-mode-p 'jira-detail-mode)
      (timfel/org--data-get-in jira-detail--current 'fields 'summary))
     ((and (derived-mode-p 'jira-issues-mode)
           (hash-table-p jira-issues-key-summary-map)
           key)
      (gethash key jira-issues-key-summary-map))
     (t nil))))

(defun timfel/org-jira--status-at-point ()
  "Return the Jira issue status represented by point, when available."
  (cond
   ((derived-mode-p 'jira-detail-mode)
    (timfel/org--data-get-in jira-detail--current 'fields 'status 'name))
   (t nil)))

(defun timfel/org-jira--seed-at-point ()
  "Return a plist describing the Jira issue at point."
  (when-let ((key (timfel/org-jira--key-at-point)))
    (list :key key
          :title (timfel/org-jira--title-at-point)
          :status (timfel/org-jira--status-at-point)
          :issue (and (derived-mode-p 'jira-detail-mode)
                      jira-detail--current))))

(defun timfel/org-jira--format-doc (doc)
  "Return DOC formatted as readable Jira text."
  (timfel/org--cleanup-text
   (cond
    ((null doc) "")
    ((stringp doc) doc)
    ((and (require 'jira-doc nil t)
          (fboundp 'jira-doc-markup))
     (condition-case nil
         (jira-doc-markup doc)
       (error (format "%s" doc))))
    (t
     (format "%s" doc)))))

(defun timfel/org-jira--render-issue (issue key)
  "Return Org text for ISSUE identified by KEY."
  (let* ((fields (timfel/org--data-get issue 'fields))
         (status (or (timfel/org--data-get-in fields 'status 'name)
                     "Unknown"))
         (title (or (timfel/org--data-get fields 'summary)
                    key))
         (description (timfel/org-jira--format-doc
                       (timfel/org--data-get fields 'description)))
         (comments (append (timfel/org--data-get-in fields 'comment 'comments) nil)))
    (timfel/org--render-to-string
     (lambda ()
       (insert (format "_%s_ %s\n\n" status title))
       (timfel/org--insert-paragraph description)
       (insert "Comments\n\n")
       (if comments
           (dolist (comment comments)
             (let* ((author (or (timfel/org--data-get-in comment 'author 'displayName)
                                (timfel/org--data-get-in comment 'author 'name)
                                "Unknown"))
                    (body (timfel/org-jira--format-doc
                           (timfel/org--data-get comment 'body))))
               (timfel/org--insert-list-item (format "- %s: " author) body)))
         (insert "No comments.\n"))))))

(defun timfel/org-jira--with-issue (key callback)
  "Call CALLBACK with Jira issue KEY asynchronously.
CALLBACK receives `(ISSUE ERROR)'."
  (if-let ((cached (timfel/org--cache-get timfel/org-jira--cache key)))
      (funcall callback cached nil)
    (let ((waiting (gethash key timfel/org-jira--inflight)))
      (if waiting
          (puthash key (cons callback waiting) timfel/org-jira--inflight)
        (puthash key (list callback) timfel/org-jira--inflight)
        (jira-api-call
         "GET" (format "issue/%s" key)
         :callback
         (lambda (data _response)
           (let ((callbacks (prog1 (gethash key timfel/org-jira--inflight)
                              (remhash key timfel/org-jira--inflight))))
             (timfel/org--cache-put timfel/org-jira--cache key data)
             (dolist (fn callbacks)
               (funcall fn data nil))))
         :error
         (cl-function
          (lambda (&key response error-thrown &allow-other-keys)
            (let* ((status (and response
                                (ignore-errors
                                  (request-response-status-code response))))
                   (message
                    (string-trim
                     (format "Jira request failed%s%s"
                             (if status
                                 (format " (%s)" status)
                               "")
                             (if error-thrown
                                 (format ": %s" error-thrown)
                               ""))))
                   (callbacks (prog1 (gethash key timfel/org-jira--inflight)
                                (remhash key timfel/org-jira--inflight))))
              (dolist (fn callbacks)
                (funcall fn nil message))))))))))

(defun timfel/ci-dashboard--pr-data-at-point ()
  "Return a plist describing the pull request represented by point."
  (let* ((section (magit-current-section))
         (pr-section (and section (ci--section-find-type 'ci-pr-entry section)))
         (pr (and pr-section (oref pr-section value)))
         (pull-request (or (timfel/org--data-get pr 'pullRequest)
                           (when-let ((merge-job (timfel/org--data-get pr 'mergeJob)))
                             (timfel/org--data-get merge-job 'pullRequest))))
         (to-ref (timfel/org--data-get pull-request 'toRef))
         (to-repo (timfel/org--data-get to-ref 'repository))
         (project (timfel/org--data-get-in to-repo 'project 'key))
         (repo (timfel/org--data-get to-repo 'slug))
         (pr-id (timfel/org--data-get pull-request 'id))
         (title (timfel/org--data-get pull-request 'title))
         (base-url (or (and (boundp 'ci-dashboard-base-url) ci-dashboard-base-url)
                       "https://ol-bitbucket.us.oracle.com")))
    (when (and project repo pr-id)
      (list :project project
            :repo repo
            :pr pr-id
            :title title
            :url (format "%s/projects/%s/repos/%s/pull-requests/%s"
                         base-url project repo pr-id)))))

(defun timfel/ci-dashboard-show-pr (project repo pr-id)
  "Open the CI dashboard and jump to PROJECT, REPO, and PR-ID."
  (interactive "sProject: \nsRepo: \nnPR ID: ")
  (require 'emacs-ci)
  (ci-dashboard)
  (timfel/ci-dashboard--focus-pr project repo pr-id 10))

(defun timfel/ci-dashboard--find-pr-section (section repo pr-id)
  "Return the CI dashboard SECTION matching REPO and PR-ID."
  (let* ((value (and section (oref section value)))
         (pull-request (timfel/org--data-get value 'pullRequest))
         (current-id (timfel/org--data-get pull-request 'id))
         (current-repo (timfel/org--data-get-in pull-request 'toRef 'repository 'slug)))
    (or (when (and section
                   (eq (oref section type) 'ci-pr-entry)
                   (equal current-repo repo)
                   (equal current-id pr-id))
          section)
        (cl-loop for child in (and section (oref section children))
                 thereis (timfel/ci-dashboard--find-pr-section child repo pr-id)))))

(defun timfel/ci-dashboard--focus-pr (project repo pr-id retries)
  "Focus the CI dashboard PR identified by PROJECT, REPO, and PR-ID."
  (when-let ((buffer (get-buffer "*ci-dashboard*")))
    (pop-to-buffer buffer)
    (let* ((root (and (boundp 'magit-root-section) magit-root-section))
           (section (and root
                         (timfel/ci-dashboard--find-pr-section
                          root repo pr-id))))
      (cond
       (section
        (goto-char (oref section start))
        (recenter))
       ((> retries 0)
        (run-at-time
         0.5 nil
         #'timfel/ci-dashboard--focus-pr
         project repo pr-id (1- retries)))))))

(defun timfel/org--call-process-json-async (program args callback)
  "Call PROGRAM with ARGS asynchronously and parse JSON.
CALLBACK receives a plist `(:ok DATA)' or `(:error MESSAGE)'."
  (if-let ((executable (executable-find program)))
      (let* ((stdout-buffer (generate-new-buffer " *timfel-org-stdout*"))
             (stderr-buffer (generate-new-buffer " *timfel-org-stderr*")))
        (make-process
         :name (format "timfel-org-%s" program)
         :buffer stdout-buffer
         :stderr stderr-buffer
         :command (cons executable args)
         :noquery t
         :sentinel
         (lambda (process _event)
           (when (memq (process-status process) '(exit signal))
             (let (result)
               (unwind-protect
                   (setq result
                         (let* ((status (process-exit-status process))
                                (stdout (with-current-buffer stdout-buffer
                                          (buffer-string)))
                                (stderr (with-current-buffer stderr-buffer
                                          (buffer-string)))
                                (json-text (if-let ((start (string-match "[[{]" (or stdout ""))))
                                               (substring stdout start))))
                           (if (zerop status)
                               (condition-case err
                                   (list :ok
                                         (json-parse-string json-text
                                                            :object-type 'hash-table
                                                            :array-type 'list
                                                            :null-object nil
                                                            :false-object nil))
                                 (error
                                  (list :error
                                        (format "Failed to parse JSON from %s: %s"
                                                program
                                                (error-message-string err)))))
                             (list :error
                                   (string-trim
                                    (string-join
                                     (delq nil
                                           (list (unless (string-empty-p stdout) stdout)
                                                 (unless (string-empty-p stderr) stderr)))
                                     "\n"))))))
                 (kill-buffer stdout-buffer)
                 (kill-buffer stderr-buffer))
               (funcall callback result))))))
    (funcall callback (list :error (format "%s is not available in PATH" program)))))

(defun timfel/org--hash-get-any (table keys)
  "Return the first non-nil value from TABLE for KEYS."
  (cl-loop for key in keys
           for value = (and (hash-table-p table) (gethash key table))
           when value return value))

(defun timfel/org-ci--extract-comments (data)
  "Extract a flat list of comment hash tables from DATA."
  (let ((items (cond
                ((listp data) data)
                ((hash-table-p data)
                 (or (timfel/org--hash-get-any data '("values" "comments" "items"))
                     (list data)))
                (t nil)))
        comments)
    (cl-labels ((collect (item)
                  (when (hash-table-p item)
                    (when (timfel/org--hash-get-any item '("text" "body" "displayText"))
                      (push item comments))
                    (dolist (child (or (gethash "comments" item) '()))
                      (collect child)))))
      (dolist (item items)
        (collect item)))
    (nreverse comments)))

(defun timfel/org-ci--build-count-from-state-table (table)
  "Return a plist of build counts from state TABLE."
  (list :in-progress (or (and (hash-table-p table)
                              (gethash "INPROGRESS" table))
                         0)
        :successful (or (and (hash-table-p table)
                             (gethash "SUCCESSFUL" table))
                        0)
        :failed (or (and (hash-table-p table)
                         (gethash "FAILED" table))
                    0)))

(defun timfel/org-ci--extract-tasks (data)
  "Extract a flat list of task hash tables from DATA."
  (let ((items (cond
                ((listp data) data)
                ((hash-table-p data)
                 (or (timfel/org--hash-get-any data '("values" "tasks" "items"))
                     (list data)))
                (t nil))))
    (cl-loop for item in items
             when (and (hash-table-p item)
                       (timfel/org--hash-get-any item '("text" "message" "body" "anchor")))
             collect item)))

(defun timfel/org-ci--build-counts (data)
  "Return build counts plist derived from gate-build DATA."
  (or (when-let* ((summary (and (hash-table-p data) (gethash "summary" data)))
                  (by-state (and (hash-table-p summary) (gethash "byState" summary))))
        (timfel/org-ci--build-count-from-state-table by-state))
      (when-let* ((repos (and (hash-table-p data) (gethash "repos" data)))
                  (repo-counts
                   (cl-loop for repo in repos
                            when (hash-table-p repo)
                            collect (timfel/org-ci--build-count-from-state-table
                                     (gethash "byState" repo)))))
        (list :in-progress (cl-loop for counts in repo-counts
                                    sum (plist-get counts :in-progress))
              :successful (cl-loop for counts in repo-counts
                                   sum (plist-get counts :successful))
              :failed (cl-loop for counts in repo-counts
                               sum (plist-get counts :failed))))
      (let ((in-progress 0)
            (successful 0)
            (failed 0))
        (cl-labels ((walk (node)
                      (cond
                       ((hash-table-p node)
                        (let* ((state-raw (timfel/org--hash-get-any
                                           node
                                           '("state" "status" "buildState" "result")))
                               (state (and state-raw (upcase (format "%s" state-raw))))
                               (key (timfel/org--hash-get-any
                                     node
                                     '("key" "gateKey" "jobKey" "url" "buildUrl" "name"))))
                          (when (and key state)
                            (pcase state
                              ("INPROGRESS" (cl-incf in-progress))
                              ("SUCCESSFUL" (cl-incf successful))
                              ("FAILED" (cl-incf failed))))
                          (maphash (lambda (_ value) (walk value)) node)))
                       ((listp node)
                        (dolist (item node)
                          (walk item))))))
          (walk data))
        (list :in-progress in-progress
              :successful successful
              :failed failed))))

(defun timfel/org-ci--task-done-p (task)
  "Return non-nil when TASK is resolved."
  (let* ((state (timfel/org--hash-get-any task '("state" "status")))
         (open-sentinel (make-symbol "missing-open"))
         (open (gethash "open" task open-sentinel)))
    (or (eq (gethash "resolved" task) t)
        (and (not (eq open open-sentinel))
             (eq open nil))
        (member (upcase (format "%s" state))
                '("RESOLVED" "DONE" "CLOSED")))))

(defun timfel/org-ci--task-text (task)
  "Return a human-friendly task description for TASK."
  (or (timfel/org--hash-get-any task '("text" "message" "body"))
      (when-let ((anchor (gethash "anchor" task)))
        (timfel/org--hash-get-any anchor '("text" "body")))
      "Unnamed task"))

(defun timfel/org-ci--fetch-data-async (project repo pr-id callback)
  "Fetch build, comment, and task data for PROJECT, REPO, and PR-ID.
CALLBACK receives one plist with `:builds', `:comments', and `:tasks'."
  (let* ((request-key (format "%s/%s/%s" project repo pr-id))
         (cached (timfel/org--cache-get timfel/org-ci--cache request-key)))
    (if cached
        (funcall callback cached)
      (let ((waiting (gethash request-key timfel/org-ci--inflight)))
        (if waiting
            (puthash request-key (cons callback waiting) timfel/org-ci--inflight)
          (puthash request-key (list callback) timfel/org-ci--inflight)
          (let ((remaining 3)
                builds comments tasks)
            (cl-labels
                ((finish-one (slot result)
                   (pcase slot
                     (:builds (setq builds result))
                     (:comments (setq comments result))
                     (:tasks (setq tasks result)))
                   (setq remaining (1- remaining))
                   (when (zerop remaining)
                     (let* ((payloads (list :builds builds
                                            :comments comments
                                            :tasks tasks))
                            (callbacks (prog1 (gethash request-key timfel/org-ci--inflight)
                                         (remhash request-key timfel/org-ci--inflight))))
                       (unless (or (plist-get builds :error)
                                   (plist-get comments :error)
                                   (plist-get tasks :error))
                         (timfel/org--cache-put timfel/org-ci--cache request-key payloads))
                       (dolist (fn callbacks)
                         (funcall fn payloads))))))
              (timfel/org--call-process-json-async
               "gdev-cli"
               (list "buildbot" "gate-builds"
                     "-p" project "-r" repo "-pr" (format "%s" pr-id) "--json")
               (lambda (result)
                 (finish-one :builds result)))
              (timfel/org--call-process-json-async
               "gdev-cli"
               (list "bitbucket" "get-comments"
                     "-p" project "-r" repo "-pr" (format "%s" pr-id) "--all" "--json")
               (lambda (result)
                 (finish-one :comments result)))
              (timfel/org--call-process-json-async
               "gdev-cli"
               (list "bitbucket" "task" "list"
                     "-p" project "-r" repo "-pr" (format "%s" pr-id) "--json")
               (lambda (result)
                 (finish-one :tasks result))))))))))

(defun timfel/org-ci--render-data (pr-id title payloads)
  "Return Org text for PR-ID and TITLE using PAYLOADS."
  (let* ((builds (plist-get payloads :builds))
         (comments (plist-get payloads :comments))
         (tasks (plist-get payloads :tasks))
         (counts (if-let ((data (plist-get builds :ok)))
                     (timfel/org-ci--build-counts data)
                   '(:in-progress 0 :successful 0 :failed 0)))
         (comment-items (if-let ((data (plist-get comments :ok)))
                            (timfel/org-ci--extract-comments data)
                          nil))
         (task-items (if-let ((data (plist-get tasks :ok)))
                         (timfel/org-ci--extract-tasks data)
                       nil)))
    (timfel/org--render-to-string
     (lambda ()
       (insert (format "_PR %s_ %s\n\n" pr-id (or title "Pull request")))
       (insert (format "%d in progress jobs\n" (plist-get counts :in-progress)))
       (insert (format "%d successful jobs\n" (plist-get counts :successful)))
       (insert (format "%d failed jobs\n\n" (plist-get counts :failed)))
       (insert "Comments\n\n")
       (cond
        ((plist-get comments :error)
         (insert (format "Could not load comments: %s\n\n"
                         (plist-get comments :error))))
        (comment-items
         (dolist (comment comment-items)
           (let* ((author (or (and-let* ((author (gethash "author" comment)))
                                (timfel/org--hash-get-any
                                 author
                                 '("displayName" "name" "slug")))
                              "Unknown"))
                  (body (or (timfel/org--hash-get-any
                             comment
                             '("text" "body" "displayText"))
                            "")))
             (timfel/org--insert-list-item (format "- %s: " author) body)))
         (insert "\n"))
        (t
         (insert "No comments.\n\n")))
       (insert "Tasks\n\n")
       (cond
        ((plist-get tasks :error)
         (insert (format "Could not load tasks: %s\n" (plist-get tasks :error))))
        (task-items
         (dolist (task task-items)
           (insert (format "- [%c] %s\n"
                           (if (timfel/org-ci--task-done-p task) ?X ?\s)
                           (timfel/org--cleanup-text
                            (timfel/org-ci--task-text task))))))
        (t
         (insert "No tasks.\n")))
       (when-let ((error (plist-get builds :error)))
         (insert (format "\nBuild refresh note: %s\n" error)))))))

(defun timfel/org--replace-dblock-contents (begin-marker content)
  "Replace the dynamic block at BEGIN-MARKER with CONTENT."
  (when-let ((buffer (marker-buffer begin-marker)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char begin-marker)
        (forward-line 1)
        (let ((content-start (point)))
          (when (re-search-forward "^[ \t]*#\\+END:" nil t)
            (let ((content-end (match-beginning 0)))
              (delete-region content-start content-end)
              (goto-char content-start)
              (insert (if (string-suffix-p "\n" content)
                          content
                        (concat content "\n"))))))))))

(defun timfel/org-jira--refresh-block-at (begin-marker params)
  "Refresh Jira dynamic block at BEGIN-MARKER using PARAMS."
  (let* ((key (or (plist-get params :key)
                  (plist-get params 'key)))
         (title (or (plist-get params :title)
                    (plist-get params 'title)))
         (status (or (plist-get params :status)
                     (plist-get params 'status))))
    (when key
      (timfel/org-jira--with-issue
       key
       (lambda (issue error)
         (timfel/org--replace-dblock-contents
          begin-marker
          (concat
           (format "[[elisp:%s][Open]]\n" (prin1-to-string `(jira-detail-show-issue ,key)))
           (if issue
               (timfel/org-jira--render-issue issue key)
             (format "Could not load Jira issue %s.\n%s\n" key (or error "Unknown Jira error"))))))))))

(defun timfel/org-ci--refresh-block-at (begin-marker params)
  "Refresh CI dynamic block at BEGIN-MARKER using PARAMS."
  (let* ((project (or (plist-get params :project)
                      (plist-get params 'project)))
         (repo (or (plist-get params :repo)
                   (plist-get params 'repo)))
         (pr-id (or (plist-get params :pr)
                    (plist-get params 'pr)))
         (title (or (plist-get params :title)
                    (plist-get params 'title))))
    (when (and project repo pr-id)
      (timfel/org-ci--fetch-data-async
       project repo pr-id
       (lambda (payloads)
         (timfel/org--replace-dblock-contents
          begin-marker
          (concat
           (format "[[elisp:%s][Open]]\n" (prin1-to-string `(timfel/ci-dashboard-show-pr ,project ,repo ,pr-id)))
           (timfel/org-ci--render-data pr-id title payloads))))))))

(defun org-dblock-write:timfel-jira (params)
  (let ((key (plist-get params :key))
        (title (plist-get params :title))
        (status (plist-get params :status))
        (begin-marker (point-marker)))
    (timfel/org-jira--refresh-block-at begin-marker params)))

(defun org-dblock-write:timfel-ci (params)
  (let ((pr-id (plist-get params :pr))
        (title (plist-get params :title))
        (begin-marker (point-marker)))
    (timfel/org-ci--refresh-block-at begin-marker params)))

(defun timfel/org-capture-dwim-title ()
  "Return a context-specific title prefix for Org capture."
  (with-current-buffer (timfel/org-capture--original-buffer)
    (save-excursion
      (cond
       ((derived-mode-p 'jira-issues-mode 'jira-detail-mode)
        (when-let* ((seed (timfel/org-jira--seed-at-point))
                    (key (plist-get seed :key)))
          (concat
           (string-trim
            (format "%s %s"
                    key
                    (or (plist-get seed :title) "")))
           " ")))
       ((derived-mode-p 'ci-dashboard-mode)
        (when-let* ((pr-data (timfel/ci-dashboard--pr-data-at-point))
                    (pr-id (plist-get pr-data :pr)))
          (concat
           (string-trim
            (format "PR %s %s"
                    pr-id
                    (or (plist-get pr-data :title) "")))
           " ")))
       (t "")))))

(defun timfel/org-jira--capture-string (seed)
  "Return the capture payload for Jira issue SEED."
  (when-let ((key (plist-get seed :key)))
    (let ((title (plist-get seed :title))
          (status (plist-get seed :status))
          (issue (plist-get seed :issue)))
      (string-join
       (delq nil
             (list
              (timfel/org-jira--open-link key)
              ""
              (format "#+BEGIN: timfel-jira :key %S :title %S :status %S"
                      key title status)
              "#+END:"))
       "\n"))))

(defun timfel/org-ci--capture-string (pr-data)
  "Return the capture payload for PR-DATA."
  (when pr-data
    (let ((project (plist-get pr-data :project))
          (repo (plist-get pr-data :repo))
          (pr-id (plist-get pr-data :pr))
          (url (plist-get pr-data :url))
          (title (plist-get pr-data :title)))
      (string-join
       (delq nil
             (list
              (timfel/org-ci--open-link project repo pr-id)
              ""
              (format "#+BEGIN: timfel-ci :project %S :repo %S :pr %S :title %S"
                      project repo pr-id title)
              "#+END:"))
       "\n"))))

(defun timfel/org-visit-agent-shell (dir)
  (let ((default-directory dir))
    (call-interactively #'agent-shell)))

(defun timfel/org-capture-dwim ()
  "Return capture content based on the original source buffer."
  (interactive)
  (let ((capture-buffer (current-buffer)))
    (when (called-interactively-p)
      (run-with-idle-timer 1 nil
                           (lambda (buf)
                             (message "Refreshing block")
                             (when (buffer-live-p buf)
                               (with-current-buffer buf
                                 (when (derived-mode-p 'org-mode)
                                   (ignore-errors
                                     (org-update-all-dblocks))))))
                           capture-buffer))
    (let ((result
           (with-current-buffer (timfel/org-capture--original-buffer)
             (save-excursion
               (cond
                ((derived-mode-p 'jira-issues-mode 'jira-detail-mode)
                 (or (timfel/org-jira--capture-string
                      (timfel/org-jira--seed-at-point))
                     ""))
                ((derived-mode-p 'ci-dashboard-mode)
                 (or (timfel/org-ci--capture-string
                      (timfel/ci-dashboard--pr-data-at-point))
                     ""))
                ((derived-mode-p 'agent-shell-mode)
                 (org-link-make-string
                  (concat "elisp:"
                          (prin1-to-string `(timfel/org-visit-agent-shell ,default-directory)))
                  (concat "Agent Shell in " (abbreviate-file-name default-directory))))
                ((use-region-p)
                 (or (timfel/org-capture--active-region-string)
                     ""))
                ((buffer-file-name)
                 "")
                (t
                 (timfel/org-capture--context-snippet 10)))))))
      (when (called-interactively-p)
        (message "Copied to kill ring")
        (kill-new
         (concat
          (org-link-make-string
           (concat "file:" (abbreviate-file-name default-directory))
           (file-name-nondirectory (directory-file-name default-directory)))
          "\n"
          result)))
      result)))

(provide 'timfel-org)

;;; timfel-org.el ends here
