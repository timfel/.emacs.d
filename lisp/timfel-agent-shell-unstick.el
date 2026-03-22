;;; timfel-agent-shell-unstick.el --- Minimal agent-shell retry helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Two recovery paths only:
;;
;; 1. `timfel/agent-shell-restart-and-retry-buffer'
;;    Capture the current session id and last prompt, kill the shell, start a
;;    fresh shell in the same directory with `:session-id', and once the new
;;    shell reports `prompt-ready', rename it back and resubmit the prompt.
;;
;; 2. Automatic retry for proper ACP 429 failures.
;;    If `agent-shell' reports a 429 through its normal ACP failure path and
;;    the shell already returned to a prompt, queue a delayed retry with global
;;    jittered backoff.

;;; Code:

(require 'agent-shell)
(require 'map)
(require 'shell-maker)
(require 'subr-x)

(declare-function agent-shell--display-buffer "agent-shell")
(declare-function agent-shell--handle "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")

(defgroup timfel-agent-shell-unstick nil
  "Minimal recovery helpers for `agent-shell'."
  :group 'agent-shell)

(defcustom timfel/agent-shell-unstick-retry-delay-min 30
  "Minimum automatic retry delay in seconds."
  :type 'integer
  :group 'timfel-agent-shell-unstick)

(defcustom timfel/agent-shell-unstick-retry-delay-max 60
  "Maximum automatic retry delay in seconds."
  :type 'integer
  :group 'timfel-agent-shell-unstick)

(defvar timfel/agent-shell-unstick--retry-queue nil)
(defvar timfel/agent-shell-unstick--retry-timer nil)
(defvar timfel/agent-shell-unstick--scheduled-retry nil)
(defvar timfel/agent-shell-unstick--next-retry-id 0)

(defvar-local timfel/agent-shell-unstick--last-prompt nil)
(defvar-local timfel/agent-shell-unstick--pending-retry-id nil)

(defun timfel/agent-shell-unstick--rate-limit-p (text)
  "Return non-nil when TEXT looks like a 429 / rate-limit failure."
  (and text
       (string-match-p
        (rx (or "429 Too Many Requests"
                "Too Many Requests"
                "rate limit"
                "rate-limit"
                "ResponseTooManyFailedAttempts"
                "exceeded retry limit"))
        text)))

(defun timfel/agent-shell-unstick--prompt-at-end-p ()
  "Return non-nil if the current shell buffer visibly ends with its prompt."
  (let* ((config (or (and (boundp 'shell-maker--config) shell-maker--config)
                     (and (boundp 'agent-shell--shell-maker-config) agent-shell--shell-maker-config)))
         (prompt (and config (shell-maker-prompt config)))
         (end (point-max))
         (start (and prompt (max (point-min) (- end (length prompt))))))
    (and prompt start
         (string= (buffer-substring-no-properties start end) prompt))))

(defun timfel/agent-shell-unstick--recoverable-prompt ()
  "Return the last prompt, falling back to the transcript if needed."
  (or (and timfel/agent-shell-unstick--last-prompt
           (replace-regexp-in-string
            "^> ?"
            ""
            (string-trim-right
             (substring-no-properties timfel/agent-shell-unstick--last-prompt))))
      (setq timfel/agent-shell-unstick--last-prompt
            (when-let ((file agent-shell--transcript-file))
              (when (file-readable-p file)
                (with-temp-buffer
                  (insert-file-contents file)
                  (goto-char (point-max))
                  (when (re-search-backward "^## User ([^)]+)\n\n" nil t)
                    (let ((start (match-end 0)))
                      (replace-regexp-in-string
                       "^> ?"
                       ""
                       (string-trim-right
                        (if (re-search-forward "\n\n## " nil t)
                            (buffer-substring-no-properties start (match-beginning 0))
                          (buffer-substring-no-properties start (point-max)))))))))))))

(defun timfel/agent-shell-unstick--drop-buffer-retries (buffer)
  "Remove queued retry state associated with BUFFER."
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
      (setq timfel/agent-shell-unstick--pending-retry-id nil))))

(defun timfel/agent-shell-unstick--schedule-next-retry ()
  "Schedule the next queued retry item."
  (unless (or timfel/agent-shell-unstick--retry-timer
              timfel/agent-shell-unstick--scheduled-retry
              (null timfel/agent-shell-unstick--retry-queue))
    (let ((item (pop timfel/agent-shell-unstick--retry-queue)))
      (setq item
            (plist-put
             item :delay
             (+ timfel/agent-shell-unstick-retry-delay-min
                (random
                 (1+ (- (max timfel/agent-shell-unstick-retry-delay-min
                               timfel/agent-shell-unstick-retry-delay-max)
                        timfel/agent-shell-unstick-retry-delay-min))))))
      (setq timfel/agent-shell-unstick--scheduled-retry item
            timfel/agent-shell-unstick--retry-timer
            (run-at-time (plist-get item :delay) nil
                         #'timfel/agent-shell-unstick--run-retry item)))))

(defun timfel/agent-shell-unstick--run-retry (item)
  "Run queued retry ITEM."
  (setq timfel/agent-shell-unstick--retry-timer nil
        timfel/agent-shell-unstick--scheduled-retry nil)
  (let ((buffer (plist-get item :buffer))
        (prompt (plist-get item :prompt))
        (retry-id (plist-get item :id)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (if (or (not (derived-mode-p 'agent-shell-mode))
                (not (eql retry-id timfel/agent-shell-unstick--pending-retry-id))
                (shell-maker-busy)
                (not (timfel/agent-shell-unstick--prompt-at-end-p)))
            (setq timfel/agent-shell-unstick--pending-retry-id nil)
          (setq timfel/agent-shell-unstick--pending-retry-id nil
                shell-maker--busy t)
          (agent-shell--handle :command prompt :shell-buffer (current-buffer))))))
  (timfel/agent-shell-unstick--schedule-next-retry))

(defun timfel/agent-shell-unstick--send-command-advice (orig-fun &rest args)
  "Remember the last submitted prompt before ORIG-FUN runs."
  (let ((prompt (plist-get args :prompt))
        (shell-buffer (plist-get args :shell-buffer)))
    (when (buffer-live-p shell-buffer)
      (with-current-buffer shell-buffer
        (setq timfel/agent-shell-unstick--last-prompt
              (and prompt
                   (replace-regexp-in-string
                    "^> ?"
                    ""
                    (string-trim-right (substring-no-properties prompt)))))))
    (apply orig-fun args)))

(defun timfel/agent-shell-unstick--make-error-handler-advice (orig-fun &rest args)
  "Wrap ORIG-FUN so proper ACP 429 failures queue an automatic retry."
  (let ((handler (apply orig-fun args))
        (shell-buffer (plist-get args :shell-buffer)))
    (lambda (acp-error raw-message)
      (funcall handler acp-error raw-message)
      (when (and (buffer-live-p shell-buffer)
                 (with-current-buffer shell-buffer
                   (and (timfel/agent-shell-unstick--rate-limit-p
                         (format "%s %s %s"
                                 (or (map-elt acp-error 'message) "")
                                 (or (map-elt acp-error 'data) "")
                                 (or raw-message "")))
                        (not (shell-maker-busy))
                        (timfel/agent-shell-unstick--prompt-at-end-p)
                        (timfel/agent-shell-unstick--recoverable-prompt)
                        (null timfel/agent-shell-unstick--pending-retry-id))))
        (with-current-buffer shell-buffer
          (setq timfel/agent-shell-unstick--pending-retry-id
                (cl-incf timfel/agent-shell-unstick--next-retry-id))
          (setq timfel/agent-shell-unstick--retry-queue
                (append timfel/agent-shell-unstick--retry-queue
                        (list (list :id timfel/agent-shell-unstick--pending-retry-id
                                    :buffer shell-buffer
                                    :prompt (timfel/agent-shell-unstick--recoverable-prompt)))))
          (timfel/agent-shell-unstick--schedule-next-retry))))))

(defun timfel/agent-shell-restart-and-retry-buffer ()
  "Kill current shell, start a fresh one with the same session id, retry prompt."
  (interactive)
  (if-let* ((old-name (buffer-name))
            (prompt (timfel/agent-shell-unstick--recoverable-prompt))
            (window-for-new-shell (agent-shell-reload))
            (shell-buffer (window-buffer window-for-new-shell)))
      (with-current-buffer shell-buffer
        (unless (derived-mode-p 'agent-shell-mode)
          (user-error "Error when reloading, not in an agent-shell buffer"))
        (run-with-timer 1 nil
                        (lambda (buffer)
                          (shell-maker-set-buffer-name buffer old-name)
                          (with-current-buffer buffer
                            (agent-shell-queue-request prompt)))))))

(advice-add 'agent-shell--send-command :around #'timfel/agent-shell-unstick--send-command-advice)
(advice-add 'agent-shell--make-error-handler :around #'timfel/agent-shell-unstick--make-error-handler-advice)

(provide 'timfel-agent-shell-unstick)

;;; timfel-agent-shell-unstick.el ends here
