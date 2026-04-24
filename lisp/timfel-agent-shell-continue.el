;;; timfel-agent-shell-continue.el --- Keep agent-shell going while conditions hold -*- lexical-binding: t -*-

;;; Commentary:

;; Buffer-local continuation rules for `agent-shell' buffers.

;;; Code:

(require 'agent-shell)
(require 'shell-maker)
(require 'subr-x)

(defgroup timfel-agent-shell-continue nil
  "Keep agent-shell buffers moving while a local condition holds."
  :group 'tools)

(defcustom timfel/agent-shell-continue-check-on-enable t
  "When non-nil, evaluate the continuation rule immediately on enable.

This is only attempted when the current `agent-shell' buffer is idle."
  :type 'boolean
  :group 'timfel-agent-shell-continue)

(defvar timfel/agent-shell-continue--type-history nil)
(defvar timfel/agent-shell-continue--trigger-history nil)
(defvar timfel/agent-shell-continue--shell-history nil)
(defvar timfel/agent-shell-continue--elisp-history nil)
(defvar timfel/agent-shell-continue--prompt-history nil)

(defvar-local timfel/agent-shell-continue--check-type nil
  "Continuation check type for the current buffer.

The value is either the symbol `shell' or `elisp'.")

(defvar-local timfel/agent-shell-continue--check-form nil
  "Continuation check form for the current buffer.

For `shell' checks this is a shell command string.  For `elisp'
checks this is a string containing a form to read and evaluate.")

(defvar-local timfel/agent-shell-continue--trigger 'success
  "Outcome that should inject the continuation prompt.

The value is either `success' or `failure'.")

(defvar-local timfel/agent-shell-continue--prompt nil
  "Prompt to inject into the current `agent-shell' buffer.")

(defvar-local timfel/agent-shell-continue--subscription nil
  "Event subscription token for the current continuation mode.")

(defvar-local timfel/agent-shell-continue--last-result nil
  "Most recent continuation check result plist.")

(defun timfel/agent-shell-continue--ensure-agent-shell ()
  "Fail unless the current buffer is an `agent-shell' buffer."
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "This command only works in an agent-shell buffer")))

(defun timfel/agent-shell-continue--read-check-type ()
  "Read a continuation check type."
  (intern
   (completing-read
    "Continue when checking via: "
    '("shell" "elisp")
    nil t nil 'timfel/agent-shell-continue--type-history
    (when timfel/agent-shell-continue--check-type
      (symbol-name timfel/agent-shell-continue--check-type)))))

(defun timfel/agent-shell-continue--read-trigger ()
  "Read which outcome should inject the continuation prompt."
  (intern
   (completing-read
    "Inject prompt on: "
    '("success" "failure")
    nil t nil 'timfel/agent-shell-continue--trigger-history
    (symbol-name timfel/agent-shell-continue--trigger))))

(defun timfel/agent-shell-continue--read-check-form (check-type)
  "Read a continuation check for CHECK-TYPE."
  (pcase check-type
    ('shell
     (read-shell-command "Shell command: "
                         (or (and (eq timfel/agent-shell-continue--check-type 'shell)
                                  timfel/agent-shell-continue--check-form)
                             nil)
                         'timfel/agent-shell-continue--shell-history))
    ('elisp
     (read-from-minibuffer
      "Elisp form: "
      (or (and (eq timfel/agent-shell-continue--check-type 'elisp)
               timfel/agent-shell-continue--check-form)
          nil)
      read-expression-map nil
      'timfel/agent-shell-continue--elisp-history))
    (_
     (user-error "Unsupported continuation check type: %S" check-type))))

(defun timfel/agent-shell-continue--describe-rule ()
  "Return a short description of the current continuation rule."
  (format "%s %s -> %s"
          (capitalize (symbol-name timfel/agent-shell-continue--trigger))
          (pcase timfel/agent-shell-continue--check-type
            ('shell "shell")
            ('elisp "elisp")
            (_ "check"))
          (string-trim (or timfel/agent-shell-continue--prompt ""))))

(defun timfel/agent-shell-continue-configure ()
  "Configure the continuation rule for the current `agent-shell' buffer."
  (interactive)
  (timfel/agent-shell-continue--ensure-agent-shell)
  (let* ((check-type (timfel/agent-shell-continue--read-check-type))
         (check-form (string-trim
                      (timfel/agent-shell-continue--read-check-form check-type)))
         (trigger (timfel/agent-shell-continue--read-trigger))
         (prompt (string-trim
                  (read-string "Injected prompt: "
                               timfel/agent-shell-continue--prompt
                               'timfel/agent-shell-continue--prompt-history))))
    (when (string-empty-p check-form)
      (user-error "Continuation check cannot be empty"))
    (when (string-empty-p prompt)
      (user-error "Injected prompt cannot be empty"))
    (setq-local timfel/agent-shell-continue--check-type check-type
                timfel/agent-shell-continue--check-form check-form
                timfel/agent-shell-continue--trigger trigger
                timfel/agent-shell-continue--prompt prompt)
    (message "agent-shell continue rule configured: %s"
             (timfel/agent-shell-continue--describe-rule))))

(defun timfel/agent-shell-continue-setup ()
  "Configure and enable continuation mode in the current `agent-shell' buffer."
  (interactive)
  (timfel/agent-shell-continue-configure)
  (timfel-agent-shell-continue-mode 1))

(defun timfel/agent-shell-continue-disable ()
  "Disable continuation mode in the current `agent-shell' buffer."
  (interactive)
  (timfel-agent-shell-continue-mode -1))

(defun timfel/agent-shell-continue--eval-shell (command)
  "Run COMMAND in the current buffer's `default-directory'."
  (with-temp-buffer
    (let ((status (call-process shell-file-name nil (current-buffer) nil
                                shell-command-switch command))
          (output nil))
      (setq output (string-trim (buffer-string)))
      (list :success (and (integerp status) (zerop status))
            :status status
            :output output))))

(defun timfel/agent-shell-continue--eval-elisp (form-string)
  "Evaluate FORM-STRING and return a continuation result plist."
  (condition-case err
      (let ((value (eval (read form-string) t)))
        (list :success (not (null value))
              :status value
              :output (string-trim (format "%S" value))))
    (error
     (list :success nil
           :status 'error
           :output (error-message-string err)))))

(defun timfel/agent-shell-continue--evaluate ()
  "Evaluate the current continuation rule."
  (pcase timfel/agent-shell-continue--check-type
    ('shell
     (timfel/agent-shell-continue--eval-shell
      timfel/agent-shell-continue--check-form))
    ('elisp
     (timfel/agent-shell-continue--eval-elisp
      timfel/agent-shell-continue--check-form))
    (_
     (user-error "No continuation rule configured in %s" (buffer-name)))))

(defun timfel/agent-shell-continue--should-inject-p (result)
  "Return non-nil when RESULT matches the configured trigger."
  (eq (if (plist-get result :success) 'success 'failure)
      timfel/agent-shell-continue--trigger))

(defun timfel/agent-shell-continue--maybe-inject (&optional source)
  "Evaluate the continuation rule and queue a prompt when it matches.

SOURCE is a short string used for status messages."
  (when (and timfel-agent-shell-continue-mode
             (not (shell-maker-busy)))
    (let ((result (timfel/agent-shell-continue--evaluate)))
      (setq-local timfel/agent-shell-continue--last-result result)
      (when (timfel/agent-shell-continue--should-inject-p result)
        (agent-shell-queue-request timfel/agent-shell-continue--prompt)
        (message "agent-shell continue queued in %s (%s: %s)"
                 (buffer-name)
                 (or source "manual")
                 (or (plist-get result :output)
                     (plist-get result :status)))
        t))))

(defun timfel/agent-shell-continue-run-now ()
  "Evaluate the continuation rule immediately in the current buffer."
  (interactive)
  (timfel/agent-shell-continue--ensure-agent-shell)
  (unless timfel-agent-shell-continue-mode
    (user-error "Continuation mode is not enabled in %s" (buffer-name)))
  (unless (timfel/agent-shell-continue--maybe-inject "manual")
    (message "agent-shell continue did not queue anything in %s"
             (buffer-name))))

(defun timfel/agent-shell-continue--subscribe ()
  "Subscribe to the current buffer's turn completion events."
  (unless timfel/agent-shell-continue--subscription
    (let ((buffer (current-buffer)))
      (setq-local
       timfel/agent-shell-continue--subscription
       (agent-shell-subscribe-to
        :shell-buffer buffer
        :event 'turn-complete
        :on-event (lambda (_event)
                    (when (buffer-live-p buffer)
                      (with-current-buffer buffer
                        (timfel/agent-shell-continue--maybe-inject
                         "turn-complete")))))))))

(defun timfel/agent-shell-continue--unsubscribe ()
  "Remove the current continuation mode subscription."
  (when timfel/agent-shell-continue--subscription
    (agent-shell-unsubscribe
     :subscription timfel/agent-shell-continue--subscription)
    (setq-local timfel/agent-shell-continue--subscription nil)))

(define-minor-mode timfel-agent-shell-continue-mode
  "Keep the current `agent-shell' moving while a local rule matches.

When enabled, the current buffer stores a single continuation rule.
At the end of each turn, the rule is evaluated.  If it matches the
configured trigger, `agent-shell-queue-request' is called with the
configured prompt."
  :lighter " Continue"
  (if timfel-agent-shell-continue-mode
      (condition-case err
          (progn
            (timfel/agent-shell-continue--ensure-agent-shell)
            (unless (and timfel/agent-shell-continue--check-type
                         timfel/agent-shell-continue--check-form
                         timfel/agent-shell-continue--prompt)
              (timfel/agent-shell-continue-configure))
            (timfel/agent-shell-continue--subscribe)
            (when (and timfel/agent-shell-continue-check-on-enable
                       (not (shell-maker-busy)))
              (timfel/agent-shell-continue--maybe-inject "enable")))
        (quit
         (setq timfel-agent-shell-continue-mode nil))
        (error
         (setq timfel-agent-shell-continue-mode nil)
         (signal (car err) (cdr err))))
    (timfel/agent-shell-continue--unsubscribe)))

(provide 'timfel-agent-shell-continue)

;;; timfel-agent-shell-continue.el ends here
