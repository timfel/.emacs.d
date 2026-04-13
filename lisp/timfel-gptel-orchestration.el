;;; timfel-gptel-orchestration.el --- GPTel orchestration helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs-native gptel tools for the AGENTS.md workflows around current work,
;; upcoming work, and starting worktree-backed tasks.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'seq)
(require 'subr-x)
(require 'timfel)
(require 'timfel-gptel-tools nil t)

(declare-function gptel--apply-preset "gptel" (preset &optional setter))
(declare-function agent-shell-buffers "agent-shell" ())
(declare-function ci-dashboard "emacs-ci" ())
(declare-function org-agenda "org-agenda" (&optional arg keys restriction-lock))
(declare-function timfel/agent-shell-fan-out-worktrees
                  "timfel-agent-shell-extensions"
                  (task-specs &optional directory))
(declare-function timfel/jira "timfel-jira-extensions" ())

(defconst timfel/gptel-orchestration-live-set-file
  (locate-user-emacs-file ".agent-shell/live-agent-shell-set.el")
  "Location of the persisted live agent-shell directory set.")

(defconst timfel/gptel-orchestration-tool-names
  '("inspect_current_work_context"
    "open_work_queues"
    "start_worktree_tasks"
    "evaluate_workspace_elisp")
  "GPTel tool names used by the orchestration preset.")

(defconst timfel/gptel-orchestration-buffer-name "*gptel-agents*"
  "Default buffer name for the AGENTS.md orchestration chat.")

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
directory as the task-spec title so `timfel/agent-shell-fan-out-worktrees'
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

(defun timfel/gptel-tool-start-worktree-tasks (tasks &optional directory)
  "Prompt for a work directory when needed and fan out TASKS there."
  (unless (require 'timfel-agent-shell-extensions nil t)
    (user-error "timfel-agent-shell-extensions is not available"))
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
    (timfel/agent-shell-fan-out-worktrees effective-task-specs directory)
    (list :ok t
          :directory directory
          :used_worktrees used-worktrees
          :created_directory (plist-get repo-state :created_directory)
          :initialized_git (plist-get repo-state :initialized_git)
          :tasks_started (length effective-task-specs))))

;;;###autoload
(defun timfel/gptel-open-agents-orchestration (&optional new-buffer)
  "Open a gptel chat buffer configured for the AGENTS.md orchestration flow.

With prefix argument NEW-BUFFER, create a fresh buffer instead of reusing
`timfel/gptel-orchestration-buffer-name'."
  (interactive "P")
  (let* ((default-directory (expand-file-name (locate-user-emacs-file "")))
         (buffer-name (if new-buffer
                          (generate-new-buffer-name
                           timfel/gptel-orchestration-buffer-name)
                        timfel/gptel-orchestration-buffer-name))
         (buffer (gptel buffer-name nil nil t)))
    (with-current-buffer buffer
      (gptel--apply-preset
       'agents-orchestration
       (lambda (sym val)
         (set (make-local-variable sym) val))))
    buffer))

(defvar timfel/gptel-orchestration-tools
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
             :optional t))
    :category "orchestration"
    :confirm t
    :include t)
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
    :include t))
  "GPTel tools that map directly to Tim's AGENTS.md orchestration workflow.")

(setq gptel-tools
      (append
       (cl-remove-if
        (lambda (tool)
          (member (gptel-tool-name tool) timfel/gptel-orchestration-tool-names))
        gptel-tools)
       timfel/gptel-orchestration-tools))

(gptel-make-preset
 'agents-orchestration
 :description "Use Emacs-native orchestration tools for current work, work queues, worktree task startup, and direct live-workspace Elisp evaluation."
 :system 'agents-orchestration
 :tools timfel/gptel-orchestration-tool-names
 :use-tools t)

(provide 'timfel-gptel-orchestration)

;;; timfel-gptel-orchestration.el ends here
