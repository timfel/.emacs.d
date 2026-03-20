;;; timfel-agent-shell-unstick.el --- Recover stuck agent-shell turns -*- lexical-binding: t -*-

;;; Commentary:

;; Recover `agent-shell' buffers that remain busy after rate-limit failures.
;; The backend sometimes only writes a 429 failure to stderr ("Notices")
;; without delivering a terminal ACP error response, leaving the shell stuck.
;; This module tracks prompt submissions, unsticks those buffers locally, and
;; retries the last prompt through a globally coordinated delayed queue.

;;; Code:

(require 'acp)
(require 'agent-shell)
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'shell-maker)
(require 'subr-x)

(declare-function agent-shell--make-error-dialog-text "agent-shell")
(declare-function agent-shell--update-fragment "agent-shell")
(declare-function agent-shell--display-pending-requests "agent-shell")
(declare-function agent-shell--display-buffer "agent-shell")
(declare-function agent-shell--finalize-session-init "agent-shell")
(declare-function agent-shell--handle "agent-shell")
(declare-function agent-shell-viewport--buffer "agent-shell-viewport")
(declare-function agent-shell--shutdown "agent-shell")
(declare-function agent-shell-heartbeat-stop "agent-shell-heartbeat")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")
(declare-function agent-shell-viewport--update-header "agent-shell-viewport")

(defgroup timfel-agent-shell-unstick nil
  "Rate-limit recovery helpers for `agent-shell'."
  :group 'agent-shell)

(defcustom timfel/agent-shell-unstick-retry-delay-min 30
  "Minimum automatic retry delay in seconds."
  :type 'integer
  :group 'timfel-agent-shell-unstick)

(defcustom timfel/agent-shell-unstick-retry-delay-max 60
  "Maximum automatic retry delay in seconds."
  :type 'integer
  :group 'timfel-agent-shell-unstick)

(defcustom timfel/agent-shell-unstick-stderr-grace-seconds 3
  "Seconds to wait for a proper ACP error after a stderr-only 429 notice."
  :type 'integer
  :group 'timfel-agent-shell-unstick)

(defcustom timfel/agent-shell-unstick-rate-limit-memory-seconds 15
  "How long to remember a recent 429 notice for ACP error correlation."
  :type 'integer
  :group 'timfel-agent-shell-unstick)

(defvar timfel/agent-shell-unstick--retry-queue nil)
(defvar timfel/agent-shell-unstick--retry-timer nil)
(defvar timfel/agent-shell-unstick--scheduled-retry nil)
(defvar timfel/agent-shell-unstick--next-retry-id 0)

(defvar-local timfel/agent-shell-unstick--last-prompt nil)
(defvar-local timfel/agent-shell-unstick--last-rate-limit-request-count nil)
(defvar-local timfel/agent-shell-unstick--last-rate-limit-time nil)
(defvar-local timfel/agent-shell-unstick--grace-timer nil)
(defvar-local timfel/agent-shell-unstick--grace-request-count nil)
(defvar-local timfel/agent-shell-unstick--recovered-request-counts nil)
(defvar-local timfel/agent-shell-unstick--pending-retry-id nil)
(defvar-local timfel/agent-shell-unstick--stderr-observer-installed nil)

(defun timfel/agent-shell-unstick--stringify (value)
  "Turn VALUE into a readable string."
  (cond
   ((null value) nil)
   ((stringp value) value)
   (t (let ((print-circle t)
            (print-level 8)
            (print-length 40))
        (prin1-to-string value)))))

(defun timfel/agent-shell-unstick--rate-limit-text-p (text)
  "Return non-nil when TEXT looks like a retry-exhausted rate-limit failure."
  (and text
       (string-match-p
        (rx (or "429 Too Many Requests"
                "Too Many Requests"
                "rate limit"
                "rate-limit"
                "ResponseTooManyFailedAttempts"
                "exceeded retry limit"))
        text)))

(defun timfel/agent-shell-unstick--current-request-count ()
  "Return the current request count for the active shell buffer."
  (map-elt agent-shell--state :request-count))

(defun timfel/agent-shell-unstick--extract-prompt-from-buffer ()
  "Extract the most recent submitted prompt from the current shell buffer."
  (save-excursion
    (let ((end-marker "<shell-maker-end-of-prompt>")
          (start-marker "<shell-maker-failed-command>\nCodex> "))
      (goto-char (point-max))
      (when (search-backward end-marker nil t)
        (let ((end (match-beginning 0)))
          (when (search-backward start-marker nil t)
            (string-trim-right
             (buffer-substring-no-properties (match-end 0) end))))))))

(defun timfel/agent-shell-unstick--extract-prompt-from-transcript ()
  "Extract the last user prompt from the current shell transcript file."
  (when-let ((file agent-shell--transcript-file))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-max))
        (when (re-search-backward "^## User ([^)]+)\n\n" nil t)
          (let ((start (match-end 0)))
            (if (re-search-forward "\n\n## " nil t)
                (string-trim-right
                 (buffer-substring-no-properties start (match-beginning 0)))
              (string-trim-right
               (buffer-substring-no-properties start (point-max))))))))))

(defun timfel/agent-shell-unstick--sanitize-prompt (prompt)
  "Normalize PROMPT into a plain string suitable for resubmission."
  (when prompt
    (let ((text (string-trim-right (substring-no-properties prompt))))
      (replace-regexp-in-string "^> ?" "" text))))

(defun timfel/agent-shell-unstick--recoverable-prompt ()
  "Return the best available prompt text for retrying the current shell."
  (or (timfel/agent-shell-unstick--sanitize-prompt timfel/agent-shell-unstick--last-prompt)
      (setq timfel/agent-shell-unstick--last-prompt
            (or (timfel/agent-shell-unstick--sanitize-prompt
                 (timfel/agent-shell-unstick--extract-prompt-from-transcript))
                (timfel/agent-shell-unstick--sanitize-prompt
                 (timfel/agent-shell-unstick--extract-prompt-from-buffer))))))

(defun timfel/agent-shell-unstick--recovered-p (request-count)
  "Return non-nil if REQUEST-COUNT already triggered local recovery."
  (memq request-count timfel/agent-shell-unstick--recovered-request-counts))

(defun timfel/agent-shell-unstick--mark-recovered (request-count)
  "Remember REQUEST-COUNT as already handled."
  (cl-pushnew request-count timfel/agent-shell-unstick--recovered-request-counts :test #'eql))

(defun timfel/agent-shell-unstick--cancel-grace-timer ()
  "Cancel any pending stderr grace timer in the current buffer."
  (when (timerp timfel/agent-shell-unstick--grace-timer)
    (cancel-timer timfel/agent-shell-unstick--grace-timer))
  (setq timfel/agent-shell-unstick--grace-timer nil
        timfel/agent-shell-unstick--grace-request-count nil))

(defun timfel/agent-shell-unstick--recent-rate-limit-p (request-count)
  "Return non-nil if REQUEST-COUNT recently hit a stderr 429 notice."
  (and (eql request-count timfel/agent-shell-unstick--last-rate-limit-request-count)
       timfel/agent-shell-unstick--last-rate-limit-time
       (< (- (float-time) timfel/agent-shell-unstick--last-rate-limit-time)
          timfel/agent-shell-unstick-rate-limit-memory-seconds)))

(defun timfel/agent-shell-unstick--rate-limit-error-p (request-count acp-error raw-message)
  "Return non-nil if ACP-ERROR/RAW-MESSAGE correspond to a 429 failure."
  (or (seq-some #'timfel/agent-shell-unstick--rate-limit-text-p
                (list (timfel/agent-shell-unstick--stringify (map-elt acp-error 'message))
                      (timfel/agent-shell-unstick--stringify (map-elt acp-error 'data))
                      (timfel/agent-shell-unstick--stringify raw-message)))
      (timfel/agent-shell-unstick--recent-rate-limit-p request-count)))

(defun timfel/agent-shell-unstick--buffer-has-rate-limit-text-p ()
  "Return non-nil if the current shell buffer already shows a 429 notice."
  (let* ((end (point-max))
         (start (max (point-min) (- end 4000))))
    (timfel/agent-shell-unstick--rate-limit-text-p
     (buffer-substring-no-properties start end))))

(defun timfel/agent-shell-unstick--active-prompt-request ()
  "Return the active `session/prompt' request in the current buffer."
  (seq-find (lambda (request)
              (equal (map-elt request :method) "session/prompt"))
            (map-elt agent-shell--state :active-requests)))

(defun timfel/agent-shell-unstick--buffer-pending-request-p ()
  "Return non-nil if the ACP client still tracks a pending request for this buffer."
  (when-let ((client (map-elt agent-shell--state :client)))
    (seq-some (lambda (entry)
                (eq (map-elt (cdr entry) :buffer) (current-buffer)))
              (map-elt client :pending-requests))))

(defun timfel/agent-shell-unstick--clear-client-pending-requests ()
  "Remove ACP pending requests associated with the current buffer."
  (when-let ((client (map-elt agent-shell--state :client)))
    (map-put! client :pending-requests
              (seq-remove (lambda (entry)
                            (eq (map-elt (cdr entry) :buffer) (current-buffer)))
                          (map-elt client :pending-requests)))))

(defun timfel/agent-shell-unstick--clear-active-request ()
  "Drop the in-flight prompt request from the current shell state."
  (map-put! agent-shell--state :active-requests
            (seq-remove (lambda (request)
                          (equal (map-elt request :method) "session/prompt"))
                        (map-elt agent-shell--state :active-requests))))

(defun timfel/agent-shell-unstick--update-viewport ()
  "Refresh the viewport header for the current shell buffer."
  (when-let ((viewport-buffer (and (fboundp 'agent-shell-viewport--buffer)
                                   (agent-shell-viewport--buffer
                                    :shell-buffer (current-buffer)
                                    :existing-only t))))
    (with-current-buffer viewport-buffer
      (when (fboundp 'agent-shell-viewport--update-header)
        (agent-shell-viewport--update-header)))))

(defun timfel/agent-shell-unstick--prompt-present-p ()
  "Return non-nil if the current shell buffer already ends with its prompt."
  (let* ((config (or (and (boundp 'shell-maker--config) shell-maker--config)
                     (and (boundp 'agent-shell--shell-maker-config) agent-shell--shell-maker-config)))
         (prompt (and config
                      (shell-maker-prompt config)))
         (end (point-max))
         (start (and prompt (max (point-min) (- end (length prompt))))))
    (and prompt
         start
         (string= (buffer-substring-no-properties start end) prompt))))

(defun timfel/agent-shell-unstick--ensure-prompt ()
  "Ensure the current idle shell buffer shows a prompt."
  (let ((config (or (and (boundp 'shell-maker--config) shell-maker--config)
                    (and (boundp 'agent-shell--shell-maker-config) agent-shell--shell-maker-config))))
    (when (and config
               (not (shell-maker-busy))
               (not (timfel/agent-shell-unstick--prompt-present-p)))
      (setq shell-maker--busy nil)
      (let* ((prompt (shell-maker-prompt config))
             (reply (save-excursion
                      (goto-char (point-max))
                      (cond ((looking-back "\n\n" nil) "")
                            ((looking-back "\n" nil) "\n")
                            (t "\n\n"))))
             (proc (get-buffer-process (current-buffer)))
             (output (concat reply
                             (propertize "\n<shell-maker-failed-command>\n"
                                         'invisible (not shell-maker--show-invisible-markers))
                             prompt)))
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (if proc
              (shell-maker--output-filter proc output)
            (insert output)))))))

(defun timfel/agent-shell-unstick--retry-block-id (request-count)
  "Return the fragment block id for REQUEST-COUNT."
  (format "timfel-rate-limit-retry-%s" request-count))

(defun timfel/agent-shell-unstick--append-status (request-count text)
  "Append TEXT to the retry status fragment for REQUEST-COUNT."
  (agent-shell--update-fragment
   :state agent-shell--state
   :block-id (timfel/agent-shell-unstick--retry-block-id request-count)
   :label-left (propertize "Rate Limit Recovery"
                           'font-lock-face 'font-lock-doc-markup-face)
   :body text
   :append t
   :create-new t))

(defun timfel/agent-shell-unstick--submit-retry-prompt (prompt)
  "Submit PROMPT directly through `agent-shell' without shell input replay."
  (setq prompt (timfel/agent-shell-unstick--sanitize-prompt prompt))
  (unless (and prompt (not (string-empty-p prompt)))
    (error "Missing retry prompt"))
  (let ((config (or (and (boundp 'shell-maker--config) shell-maker--config)
                    (and (boundp 'agent-shell--shell-maker-config) agent-shell--shell-maker-config))))
    (unless config
      (error "No shell-maker config available"))
    (setq shell-maker--busy t)
    (agent-shell--handle :command prompt :shell-buffer (current-buffer))))

(defun timfel/agent-shell-unstick--annotate-restarted-shell (shell-buffer request-count source)
  "Add a recovery note to SHELL-BUFFER for REQUEST-COUNT from SOURCE."
  (when (buffer-live-p shell-buffer)
    (with-current-buffer shell-buffer
      (agent-shell--update-fragment
       :state agent-shell--state
       :block-id (timfel/agent-shell-unstick--retry-block-id request-count)
       :label-left (propertize "Rate Limit Recovery"
                               'font-lock-face 'font-lock-doc-markup-face)
       :body (format "Restarted the shell backend after %s and requested session resume."
                     source)
       :create-new t))))

(defun timfel/agent-shell-unstick--reset-shell-state-for-resume (session-id)
  "Reset the current shell buffer so SESSION-ID will be resumed on next handle."
  (agent-shell--shutdown)
  (timfel/agent-shell-unstick--clear-client-pending-requests)
  (map-put! agent-shell--state :active-requests nil)
  (map-put! agent-shell--state :pending-requests nil)
  (map-put! agent-shell--state :tool-calls nil)
  (map-put! agent-shell--state :usage nil)
  (map-put! agent-shell--state :last-entry-type nil)
  (map-put! agent-shell--state :session nil)
  (map-put! agent-shell--state :resume-session-id session-id)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (when-let ((proc (get-buffer-process (current-buffer))))
    (set-marker (process-mark proc) (point-max))))

(defun timfel/agent-shell-unstick--next-retry-id ()
  "Return a fresh retry id."
  (setq timfel/agent-shell-unstick--next-retry-id
        (1+ timfel/agent-shell-unstick--next-retry-id)))

(defun timfel/agent-shell-unstick--cancel-buffer-retry (buffer)
  "Drop queued or scheduled retries associated with BUFFER."
  (setq timfel/agent-shell-unstick--retry-queue
        (seq-remove (lambda (item)
                      (eq (plist-get item :buffer) buffer))
                    timfel/agent-shell-unstick--retry-queue))
  (when (and timfel/agent-shell-unstick--scheduled-retry
             (eq (plist-get timfel/agent-shell-unstick--scheduled-retry :buffer) buffer))
    (when (timerp timfel/agent-shell-unstick--retry-timer)
      (cancel-timer timfel/agent-shell-unstick--retry-timer))
    (setq timfel/agent-shell-unstick--retry-timer nil
          timfel/agent-shell-unstick--scheduled-retry nil))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq timfel/agent-shell-unstick--pending-retry-id nil)))
  (timfel/agent-shell-unstick--schedule-next-retry))

(defun timfel/agent-shell-unstick--retry-delay ()
  "Return a jittered retry delay in seconds."
  (let* ((min-delay timfel/agent-shell-unstick-retry-delay-min)
         (max-delay (max min-delay timfel/agent-shell-unstick-retry-delay-max)))
    (+ min-delay (random (1+ (- max-delay min-delay))))))

(defun timfel/agent-shell-unstick--schedule-next-retry ()
  "Schedule the next retry from the global queue."
  (unless (or timfel/agent-shell-unstick--retry-timer
              timfel/agent-shell-unstick--scheduled-retry
              (null timfel/agent-shell-unstick--retry-queue))
    (let* ((item (pop timfel/agent-shell-unstick--retry-queue))
           (delay (timfel/agent-shell-unstick--retry-delay))
           (buffer (plist-get item :buffer))
           (request-count (plist-get item :request-count)))
      (setq item (plist-put item :delay delay)
            timfel/agent-shell-unstick--scheduled-retry item
            timfel/agent-shell-unstick--retry-timer
            (run-at-time delay nil #'timfel/agent-shell-unstick--run-retry item))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (timfel/agent-shell-unstick--append-status
           request-count
           (format "\nAutomatic retry scheduled via global queue in %ss." delay)))))))

(defun timfel/agent-shell-unstick--enqueue-retry (buffer prompt request-count source)
  "Queue PROMPT for BUFFER after recovering REQUEST-COUNT from SOURCE."
  (setq prompt (timfel/agent-shell-unstick--sanitize-prompt prompt))
  (when (and (buffer-live-p buffer)
             (stringp prompt)
             (not (string-empty-p (string-trim prompt))))
    (with-current-buffer buffer
      (unless timfel/agent-shell-unstick--pending-retry-id
        (let ((retry-id (timfel/agent-shell-unstick--next-retry-id)))
          (setq timfel/agent-shell-unstick--pending-retry-id retry-id)
          (setq timfel/agent-shell-unstick--retry-queue
                (append timfel/agent-shell-unstick--retry-queue
                        (list (list :id retry-id
                                    :buffer buffer
                                    :prompt prompt
                                    :request-count request-count
                                    :source source
                                    :attempt 0))))
          (timfel/agent-shell-unstick--append-status
           request-count
           (format "\nQueued an automatic retry for the last prompt after %s."
                   source))
          (timfel/agent-shell-unstick--schedule-next-retry))))))

(defun timfel/agent-shell-unstick--run-retry (item)
  "Execute queued retry ITEM."
  (setq timfel/agent-shell-unstick--retry-timer nil
        timfel/agent-shell-unstick--scheduled-retry nil)
  (let* ((buffer (plist-get item :buffer))
         (retry-id (plist-get item :id))
         (prompt (plist-get item :prompt))
         (request-count (plist-get item :request-count))
         (attempt (plist-get item :attempt)))
    (cond
     ((not (buffer-live-p buffer))
      nil)
     (t
      (with-current-buffer buffer
        (cond
         ((not (derived-mode-p 'agent-shell-mode))
          (setq timfel/agent-shell-unstick--pending-retry-id nil))
         ((not (eql retry-id timfel/agent-shell-unstick--pending-retry-id))
          nil)
         ((shell-maker-busy)
          (setq timfel/agent-shell-unstick--retry-queue
                (append timfel/agent-shell-unstick--retry-queue
                        (list (plist-put (copy-sequence item) :attempt (1+ attempt)))))
          (timfel/agent-shell-unstick--append-status
           request-count
           "\nRetry reached the front of the global queue, but the buffer is still busy; requeueing."))
         (t
          (setq timfel/agent-shell-unstick--pending-retry-id nil)
          (timfel/agent-shell-unstick--append-status
           request-count
           "\nSubmitting automatic retry now.")
          (timfel/agent-shell-unstick--submit-retry-prompt prompt)))))))
  (timfel/agent-shell-unstick--schedule-next-retry))

(defun timfel/agent-shell-unstick--recover-buffer (request-count source)
  "Locally unstick the current shell for REQUEST-COUNT reported by SOURCE."
  (unless (timfel/agent-shell-unstick--recovered-p request-count)
    (timfel/agent-shell-unstick--mark-recovered request-count)
    (timfel/agent-shell-unstick--cancel-grace-timer)
    (let ((prompt (timfel/agent-shell-unstick--recoverable-prompt)))
      (timfel/agent-shell-unstick--clear-client-pending-requests)
      (timfel/agent-shell-unstick--clear-active-request)
      (agent-shell-heartbeat-stop :heartbeat (map-elt agent-shell--state :heartbeat))
      (agent-shell--update-fragment
       :state agent-shell--state
       :block-id (format "timfel-rate-limit-failure-%s" request-count)
       :body (agent-shell--make-error-dialog-text
              :code 429
              :message "Too Many Requests"
              :raw-message (format "Recovered locally after %s. The backend exhausted its retries and did not finish the turn cleanly." source))
       :create-new t)
      (timfel/agent-shell-unstick--append-status
       request-count
       (format "Recovered a stuck turn after %s." source))
      (shell-maker-finish-output :config shell-maker--config :success t)
      (timfel/agent-shell-unstick--update-viewport)
      (timfel/agent-shell-unstick--enqueue-retry (current-buffer) prompt request-count source)
      (when (map-elt agent-shell--state :pending-requests)
        (agent-shell--display-pending-requests)))))

(defun timfel/agent-shell-unstick--restart-shell-and-retry (request-count source)
  "Restart the current shell backend, resume its session, and queue a retry.

REQUEST-COUNT identifies the failed turn.  SOURCE describes why recovery is
needed."
  (let* ((shell-buffer (current-buffer))
         (session-id (map-nested-elt agent-shell--state '(:session :id)))
         (prompt (timfel/agent-shell-unstick--recoverable-prompt))
         (token nil)
         (done nil))
    (unless session-id
      (user-error "Cannot restart/resume without an existing session id"))
    (timfel/agent-shell-unstick--cancel-grace-timer)
    (timfel/agent-shell-unstick--mark-recovered request-count)
    (setq timfel/agent-shell-unstick--last-prompt prompt)
    (timfel/agent-shell-unstick--reset-shell-state-for-resume session-id)
    (timfel/agent-shell-unstick--annotate-restarted-shell
     shell-buffer request-count source)
    (when (and prompt (not (string-empty-p (string-trim prompt))))
      (let ((resume-and-queue
             (lambda ()
               (when (and (not done)
                          (buffer-live-p shell-buffer))
                 (with-current-buffer shell-buffer
                   (setq done t)
                   (when token
                     (agent-shell-unsubscribe :subscription token))
                   (timfel/agent-shell-unstick--ensure-prompt)
                   (timfel/agent-shell-unstick--enqueue-retry
                    shell-buffer
                    prompt
                    request-count
                    source))))))
        (setq token
              (agent-shell-subscribe-to
               :shell-buffer shell-buffer
               :event 'prompt-ready
               :on-event (lambda (_event)
                           (funcall resume-and-queue))))
        ;; Some resumed shells end up idle without a prompt-ready event.
        ;; Fall back to restoring the prompt locally and queueing the retry.
        (run-at-time
         2 nil
         (lambda ()
           (when (and (not done)
                      (buffer-live-p shell-buffer))
             (with-current-buffer shell-buffer
               (when (and (map-nested-elt agent-shell--state '(:session :id))
                          (not (shell-maker-busy))
                          (not (map-elt agent-shell--state :active-requests)))
                 (funcall resume-and-queue))))))))
    (agent-shell--handle :shell-buffer shell-buffer)
    (agent-shell--display-buffer shell-buffer)
    shell-buffer))

(defun timfel/agent-shell-unstick--finish-rate-limit (buffer request-count source &optional restart-backend)
  "Finalize rate-limit handling for BUFFER REQUEST-COUNT from SOURCE.

When RESTART-BACKEND is non-nil, discard the current backend process and start
a fresh shell that resumes the previous session before queueing a retry."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (timfel/agent-shell-unstick--cancel-grace-timer)
      (if (and restart-backend
               (or (shell-maker-busy)
                   (timfel/agent-shell-unstick--buffer-pending-request-p)))
          (timfel/agent-shell-unstick--restart-shell-and-retry request-count source)
        (if (and (shell-maker-busy)
                 (or (timfel/agent-shell-unstick--active-prompt-request)
                     (timfel/agent-shell-unstick--buffer-pending-request-p)))
            (timfel/agent-shell-unstick--recover-buffer request-count source)
        (unless (timfel/agent-shell-unstick--recovered-p request-count)
          (timfel/agent-shell-unstick--mark-recovered request-count)
          (timfel/agent-shell-unstick--enqueue-retry
           buffer
           (timfel/agent-shell-unstick--recoverable-prompt)
           request-count
           source)))))))

(defun timfel/agent-shell-unstick--stderr-grace-fired (buffer request-count)
  "Recover BUFFER if REQUEST-COUNT is still stuck after the stderr grace period."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eql request-count timfel/agent-shell-unstick--grace-request-count)
        (setq timfel/agent-shell-unstick--grace-timer nil
              timfel/agent-shell-unstick--grace-request-count nil))
      (when (and (derived-mode-p 'agent-shell-mode)
                 (eql request-count (timfel/agent-shell-unstick--current-request-count))
                 (timfel/agent-shell-unstick--recent-rate-limit-p request-count)
                 (not (timfel/agent-shell-unstick--recovered-p request-count))
                 (shell-maker-busy)
                 (or (timfel/agent-shell-unstick--active-prompt-request)
                     (timfel/agent-shell-unstick--buffer-pending-request-p)))
        (timfel/agent-shell-unstick--finish-rate-limit
         buffer request-count "stderr-only 429 notice" t)))))

(defun timfel/agent-shell-unstick--on-stderr-error (state acp-error)
  "Observe stderr ACP-ERROR for STATE and schedule a local recovery on 429."
  (when (timfel/agent-shell-unstick--rate-limit-text-p
         (timfel/agent-shell-unstick--stringify (map-elt acp-error 'message)))
    (let ((buffer (map-elt state :buffer)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((request-count (timfel/agent-shell-unstick--current-request-count)))
            (setq timfel/agent-shell-unstick--last-rate-limit-request-count request-count
                  timfel/agent-shell-unstick--last-rate-limit-time (float-time))
            (unless (or (timfel/agent-shell-unstick--recovered-p request-count)
                        (and (timerp timfel/agent-shell-unstick--grace-timer)
                             (eql request-count timfel/agent-shell-unstick--grace-request-count)))
              (setq timfel/agent-shell-unstick--grace-request-count request-count
                    timfel/agent-shell-unstick--grace-timer
                    (run-at-time timfel/agent-shell-unstick-stderr-grace-seconds
                                 nil
                                 #'timfel/agent-shell-unstick--stderr-grace-fired
                                 buffer request-count)))))))))

(defun timfel/agent-shell-unstick--install-error-observer (state)
  "Install the stderr rate-limit observer for STATE once."
  (when-let ((buffer (map-elt state :buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (unless timfel/agent-shell-unstick--stderr-observer-installed
          (setq timfel/agent-shell-unstick--stderr-observer-installed t)
          (acp-subscribe-to-errors
           :client (map-elt state :client)
           :on-error (lambda (acp-error)
                       (timfel/agent-shell-unstick--on-stderr-error state acp-error))
           :buffer buffer))))))

(defun timfel/agent-shell-unstick--remember-prompt (prompt shell-buffer)
  "Track PROMPT for SHELL-BUFFER and cancel stale automatic retries."
  (when (buffer-live-p shell-buffer)
    (with-current-buffer shell-buffer
      (setq timfel/agent-shell-unstick--last-prompt prompt)
      (unless (eql timfel/agent-shell-unstick--pending-retry-id
                   (and timfel/agent-shell-unstick--scheduled-retry
                        (plist-get timfel/agent-shell-unstick--scheduled-retry :id)))
        (timfel/agent-shell-unstick--cancel-buffer-retry shell-buffer)))))

(defun timfel/agent-shell-unstick--send-command-advice (orig-fun &rest args)
  "Advice ORIG-FUN to remember the last prompt before submission."
  (let ((prompt (plist-get args :prompt))
        (shell-buffer (plist-get args :shell-buffer)))
    (timfel/agent-shell-unstick--remember-prompt prompt shell-buffer)
    (apply orig-fun args)))

(defun timfel/agent-shell-unstick--subscribe-errors-advice (orig-fun &rest args)
  "Advice ORIG-FUN to subscribe a second error observer for 429 recovery."
  (let ((result (apply orig-fun args))
        (state (plist-get args :state)))
    (timfel/agent-shell-unstick--install-error-observer state)
    result))

(defun timfel/agent-shell-unstick--make-error-handler-advice (orig-fun &rest args)
  "Advice ORIG-FUN to retry prompt turns that failed from a 429."
  (let* ((state (plist-get args :state))
         (shell-buffer (plist-get args :shell-buffer))
         (handler (apply orig-fun args)))
    (lambda (acp-error raw-message)
      (let ((request-count (map-elt state :request-count)))
        (funcall handler acp-error raw-message)
        (when (and (buffer-live-p shell-buffer)
                   (with-current-buffer shell-buffer
                     (timfel/agent-shell-unstick--rate-limit-error-p
                      request-count acp-error raw-message)))
          (timfel/agent-shell-unstick--finish-rate-limit
           shell-buffer request-count "ACP 429 error response"))))))

(defun timfel/agent-shell-unstick--finalize-session-init-advice (orig-fun &rest args)
  "Ensure resumed shells have a live `shell-maker--config' during finalization."
  (let ((config (or (and (boundp 'shell-maker--config) shell-maker--config)
                    (and (boundp 'agent-shell--shell-maker-config)
                         agent-shell--shell-maker-config))))
    (when config
      (setq-local shell-maker--config config)
      (setq-local shell-maker--buffer-name-override (buffer-name))
      (when-let ((shell-buffer (ignore-errors (shell-maker-buffer config))))
        (when (buffer-live-p shell-buffer)
          (with-current-buffer shell-buffer
            (setq-local shell-maker--config config)
            (setq-local shell-maker--buffer-name-override (buffer-name))))))
    (apply orig-fun args)))

(defun timfel/agent-shell-unstick-buffer ()
  "Recover the current stuck `agent-shell' turn after a 429 failure."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))
  (let ((request-count (timfel/agent-shell-unstick--current-request-count)))
    (unless (or (timfel/agent-shell-unstick--recent-rate-limit-p request-count)
                (timfel/agent-shell-unstick--buffer-has-rate-limit-text-p))
      (user-error "No recent 429 notice found in %s" (buffer-name)))
    (setq timfel/agent-shell-unstick--last-rate-limit-request-count request-count
          timfel/agent-shell-unstick--last-rate-limit-time (float-time))
    (timfel/agent-shell-unstick--finish-rate-limit
     (current-buffer) request-count "manual recovery command" t)))

(defun timfel/agent-shell-force-recover-buffer ()
  "Force recovery of the current stuck `agent-shell' turn.

Unlike `timfel/agent-shell-unstick-buffer', this does not require a detected
429 notice.  It is meant for silent stalls where the backend never surfaces a
terminal error."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))
  (timfel/agent-shell-restart-and-retry-buffer))

(defun timfel/agent-shell-restart-and-retry-buffer ()
  "Restart the current shell, resume its session, and queue the last prompt.

This is the simple manual recovery path for silent stalls: capture the current
session id and recoverable prompt, kill the shell buffer, start a fresh shell
in the same directory with `:session-id', and immediately queue the last
prompt on the new shell."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))
  (let* ((old-buffer (current-buffer))
         (buffer-name (buffer-name old-buffer))
         (config (map-elt agent-shell--state :agent-config))
         (session-id (map-nested-elt agent-shell--state '(:session :id)))
         (prompt (timfel/agent-shell-unstick--recoverable-prompt))
         (cwd default-directory)
         (new-buffer nil))
    (unless session-id
      (user-error "No active session id found for %s" buffer-name))
    (unless (and prompt (not (string-empty-p prompt)))
      (user-error "No recoverable prompt found for %s" buffer-name))
    (timfel/agent-shell-unstick--cancel-buffer-retry old-buffer)
    (timfel/agent-shell-unstick--cancel-grace-timer)
    (let ((kill-buffer-query-functions nil))
      (kill-buffer old-buffer))
    (let ((default-directory cwd))
      (setq new-buffer
            (agent-shell--start :config config
                                :session-id session-id
                                :new-session t
                                :no-focus t)))
    (unless (buffer-live-p new-buffer)
      (error "Failed to start replacement shell for %s" buffer-name))
    (with-current-buffer new-buffer
      (rename-buffer buffer-name t)
      (setq timfel/agent-shell-unstick--last-prompt prompt)
      (timfel/agent-shell-unstick--cancel-buffer-retry new-buffer)
      (setq shell-maker--busy nil)
      (timfel/agent-shell-unstick--ensure-prompt)
      (agent-shell-queue-request prompt)
      (agent-shell--display-buffer new-buffer))
    (message "Restarted %s, resumed session %s, and queued the last prompt"
             buffer-name session-id)))

(defun timfel/agent-shell-restart-fresh-and-retry-buffer ()
  "Restart the current shell fresh in the same folder and queue the last prompt.

Unlike `timfel/agent-shell-restart-and-retry-buffer', this does not attempt to
resume the prior session id.  It is the reliable fallback when the
`agent-shell' resume path itself is broken."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))
  (let* ((old-buffer (current-buffer))
         (buffer-name (buffer-name old-buffer))
         (config (map-elt agent-shell--state :agent-config))
         (prompt (timfel/agent-shell-unstick--recoverable-prompt))
         (cwd default-directory)
         (new-buffer nil))
    (unless (and prompt (not (string-empty-p prompt)))
      (user-error "No recoverable prompt found for %s" buffer-name))
    (timfel/agent-shell-unstick--cancel-buffer-retry old-buffer)
    (timfel/agent-shell-unstick--cancel-grace-timer)
    (let ((kill-buffer-query-functions nil))
      (kill-buffer old-buffer))
    (let ((default-directory cwd))
      (setq new-buffer
            (agent-shell--start :config config
                                :new-session t
                                :no-focus t)))
    (unless (buffer-live-p new-buffer)
      (error "Failed to start replacement shell for %s" buffer-name))
    (with-current-buffer new-buffer
      (rename-buffer buffer-name t)
      (setq timfel/agent-shell-unstick--last-prompt prompt)
      (timfel/agent-shell-unstick--cancel-buffer-retry new-buffer)
      (setq shell-maker--busy nil)
      (timfel/agent-shell-unstick--ensure-prompt)
      (timfel/agent-shell-unstick--submit-retry-prompt prompt)
      (agent-shell--display-buffer new-buffer))
    (message "Restarted %s fresh and queued the last prompt" buffer-name)))

(defun timfel/agent-shell-retry-last-prompt ()
  "Queue a delayed retry of the last prompt for the current shell buffer."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))
  (let ((prompt (timfel/agent-shell-unstick--recoverable-prompt))
        (request-count (timfel/agent-shell-unstick--current-request-count)))
    (unless (and prompt (not (string-empty-p (string-trim prompt))))
      (user-error "No recoverable prompt found in %s" (buffer-name)))
    (timfel/agent-shell-unstick--enqueue-retry
     (current-buffer)
     prompt
     request-count
     "manual retry request")
    (message "Queued retry for %s" (buffer-name))))

(defun timfel/agent-shell-unstick-stuck-buffers ()
  "Recover all busy `agent-shell' buffers that currently show a 429 notice."
  (interactive)
  (let ((recovered 0))
    (dolist (buffer (agent-shell-buffers))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and (derived-mode-p 'agent-shell-mode)
                     (shell-maker-busy)
                     (timfel/agent-shell-unstick--buffer-has-rate-limit-text-p))
            (setq timfel/agent-shell-unstick--last-rate-limit-request-count
                  (timfel/agent-shell-unstick--current-request-count)
                  timfel/agent-shell-unstick--last-rate-limit-time (float-time))
            (timfel/agent-shell-unstick--finish-rate-limit
             buffer
             (timfel/agent-shell-unstick--current-request-count)
             "manual bulk recovery")
            (setq recovered (1+ recovered))))))
    (message "Recovered %d rate-limited agent-shell buffer%s"
             recovered
             (if (= recovered 1) "" "s"))))

(defun timfel/agent-shell-unstick--install-existing-clients ()
  "Attach stderr observers to already-running agent-shell clients."
  (dolist (buffer (agent-shell-buffers))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (derived-mode-p 'agent-shell-mode)
                   (map-elt agent-shell--state :client))
          (timfel/agent-shell-unstick--install-error-observer agent-shell--state))))))

(advice-add 'agent-shell--send-command :around #'timfel/agent-shell-unstick--send-command-advice)
(advice-add 'agent-shell--subscribe-to-client-events :around #'timfel/agent-shell-unstick--subscribe-errors-advice)
(advice-add 'agent-shell--make-error-handler :around #'timfel/agent-shell-unstick--make-error-handler-advice)
(advice-add 'agent-shell--finalize-session-init :around #'timfel/agent-shell-unstick--finalize-session-init-advice)

(timfel/agent-shell-unstick--install-existing-clients)

(provide 'timfel-agent-shell-unstick)

;;; timfel-agent-shell-unstick.el ends here
