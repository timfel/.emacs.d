;;; timfel-agent-shell-unstick.el --- Minimal agent-shell retry helpers -*- lexical-binding: t -*-

(require 'agent-shell)
(require 'map)
(require 'shell-maker)
(require 'subr-x)

(defun timfel/agent-shell-unstick--rate-limit-p (event)
  "Return non-nil when TEXT looks like a 429 / rate-limit failure."
  (let ((code (map-elt event :code))
        (text (map-elt event :message)))
    (or (= code 429)
        (and text
             (string-match-p
              (rx (or "429 Too Many Requests"
                      "Too Many Requests"
                      "rate limit"
                      "rate-limit"
                      "ResponseTooManyFailedAttempts"
                      "exceeded retry limit"))
              text)))))

(defun timfel/agent-shell-retry-on-hitting-rate-limit ()
  (interactive)
  (agent-shell-subscribe-to
   :shell-buffer (current-buffer)
   :event 'error
   :on-event (lambda (event)
               (when (timfel/agent-shell-unstick--rate-limit-p event)
                 (message "Rate limit, scheduling a retry in %s" (current-buffer))
                 (run-with-timer (+ 10 (random 20)) nil
                                 (lambda (buf)
                                   (with-current-buffer buf
                                     (if (and (buffer-live-p buf)
                                              (not (shell-maker-busy)))
                                         (agent-shell-queue-request "We got interrupted, continue.")
                                       (message "Agent shell %s busy again after interruption, not queuing" buf))))
                                 (current-buffer))))))

(defun timfel/agent-shell-restart-and-retry-buffer (&optional force)
  "Restart shell with the same session id then re-send last prompt."
  (interactive "P")
  (if (or (shell-maker-busy) force)
      (if-let* ((agent-directory default-directory)
                (file agent-shell--transcript-file)
                (prompt (when (and (file-readable-p file) (shell-maker-busy))
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
                                    (buffer-substring-no-properties start (point-max)))))))))))
          (let ((default-directory agent-directory))
            (if-let* ((old-name (buffer-name))
                      (window-for-new-shell (agent-shell-reload))
                      (shell-buffer (window-buffer window-for-new-shell)))
                (with-current-buffer shell-buffer
                  (unless (derived-mode-p 'agent-shell-mode)
                    (user-error "Error when reloading, not in an agent-shell buffer"))
                  (run-with-timer 4 nil
                                  (lambda (buffer)
                                    (shell-maker-set-buffer-name buffer old-name)
                                    (with-current-buffer buffer
                                      (agent-shell-queue-request prompt)))
                                  shell-buffer)))
            (user-error "Could not find last prompt"))
        (message "This agent-shell is not busy, use prefix to force reloading"))))

(provide 'timfel-agent-shell-unstick)

;;; timfel-agent-shell-unstick.el ends here
