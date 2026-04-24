;;; timfel-agent-shell-context.el --- Inject recent Emacs context into agent-shell prompts -*- lexical-binding: t -*-

;; minor-mode to prepend recent editor context to outbound agent-shell prompts.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'ring)
(require 'seq)
(require 'server)
(require 'subr-x)

(defgroup timfel-agent-shell-context nil
  "Inject recent Emacs context into agent-shell prompts."
  :group 'tools)

(defcustom timfel/agent-shell-context-buffer-limit 5
  "Maximum number of recently used buffers to include."
  :type 'integer
  :group 'timfel-agent-shell-context)

(defcustom timfel/agent-shell-context-lines-around-point 10
  "How many lines of context to include before and after point."
  :type 'integer
  :group 'timfel-agent-shell-context)

(defcustom timfel/agent-shell-context-shell-output-lines 20
  "How many trailing lines of shell output to include."
  :type 'integer
  :group 'timfel-agent-shell-context)

(defcustom timfel/agent-shell-context-special-buffer-regexps
  '("\\`\\*Backtrace\\*\\'"
    "\\`\\*Jira.*"
    "\\`\\*ci-dashboard\\*\\'")
  "Buffer name regexps that should always be eligible for context collection.

These buffers are considered even when they are not part of the target
agent-shell buffer's project."
  :type '(repeat regexp)
  :group 'timfel-agent-shell-context)

(defvar timfel/agent-shell-context-sent-states (make-hash-table :test 'equal)
  "Stores the last sent state for buffers keyed by buffer name.")

(defvar timfel/agent-shell-context-last-xref-summary nil
  "Stores the last sent xref summary.")

(defun timfel/agent-shell-context--project-root (buffer)
  "Return BUFFER's project root, or nil when none can be determined."
  (with-current-buffer buffer
    (let ((directory
           (cond
            ((and (derived-mode-p 'vterm-mode)
                  (boundp 'vterm--process)
                  (process-live-p vterm--process))
             (ignore-errors
               (file-truename
                (format "/proc/%d/cwd/" (process-id vterm--process)))))
            (default-directory
             (expand-file-name default-directory))
            (t nil))))
      (or (when-let ((project (and directory
                                   (project-current nil directory))))
            (expand-file-name (project-root project)))
          directory))))

(defun timfel/agent-shell-context--eligible-buffer-p (buffer project-root)
  "Return non-nil when BUFFER should be considered for PROJECT-ROOT."
  (let ((name (buffer-name buffer)))
    (and name
         (not (string-prefix-p " " name))
         (not (with-current-buffer buffer
                (derived-mode-p 'agent-shell-mode
                                'agent-shell-viewport-edit-mode
                                'agent-shell-viewport-view-mode
                                'agent-shell-diff-mode)))
         (or (seq-some (lambda (regexp)
                         (string-match-p regexp name))
                       timfel/agent-shell-context-special-buffer-regexps)
             (when-let* ((root project-root)
                         (buffer-root (timfel/agent-shell-context--project-root buffer)))
               (string-prefix-p (file-name-as-directory root)
                                (file-name-as-directory buffer-root)))))))

(defun timfel/agent-shell-context--buffer-state (buffer)
  "Return a compact state object for BUFFER."
  (with-current-buffer buffer
    (list (point)
          (buffer-chars-modified-tick)
          (point-max))))

(defun timfel/agent-shell-context--tail-lines (start end line-count)
  "Return up to LINE-COUNT trailing lines between START and END."
  (save-excursion
    (goto-char end)
    (forward-line (- line-count))
    (buffer-substring-no-properties (max start (point)) end)))

(defun timfel/agent-shell-context--lines-around-point ()
  "Return a snippet around point in the current buffer."
  (save-excursion
    (let* ((start-line (max 1 (- (line-number-at-pos) timfel/agent-shell-context-lines-around-point)))
           (end-line (+ (line-number-at-pos) timfel/agent-shell-context-lines-around-point))
           (start-pos (progn
                        (goto-char (point-min))
                        (forward-line (1- start-line))
                        (point)))
           (end-pos (progn
                      (goto-char (point-min))
                      (forward-line end-line)
                      (point))))
      (buffer-substring-no-properties start-pos end-pos))))

(defun timfel/agent-shell-context--ring-most-recent (ring-symbol)
  "Return the newest entry in RING-SYMBOL, or nil when unavailable."
  (when (and (boundp ring-symbol)
             (ring-p (symbol-value ring-symbol))
             (> (ring-length (symbol-value ring-symbol)) 0))
    (string-trim (ring-ref (symbol-value ring-symbol) 0))))

(defun timfel/agent-shell-context--guess-last-command-from-output (text)
  "Heuristically extract the last shell command from TEXT."
  (let ((lines (reverse (split-string text "\n" t "[ \t]+"))))
    (seq-some
     (lambda (line)
       (when (string-match
              "^[^#$%>\n]*\\(?:[$#%>]\\|❯\\|λ\\)\\s-*\\(.+\\)$"
              line)
         (string-trim (match-string 1 line))))
     lines)))

(defun timfel/agent-shell-context--last-shell-command ()
  "Return the most recent shell command for the current shell buffer."
  (cond
   ((derived-mode-p 'eshell-mode)
    (or (timfel/agent-shell-context--ring-most-recent 'eshell-history-ring)
        (timfel/agent-shell-context--guess-last-command-from-output
         (timfel/agent-shell-context--tail-lines
          (point-min) (point-max) (* 3 timfel/agent-shell-context-shell-output-lines)))))
   ((derived-mode-p 'comint-mode 'shell-mode 'term-mode)
    (or (timfel/agent-shell-context--ring-most-recent 'comint-input-ring)
        (timfel/agent-shell-context--guess-last-command-from-output
         (timfel/agent-shell-context--tail-lines
          (point-min) (point-max) (* 3 timfel/agent-shell-context-shell-output-lines)))))
   ((derived-mode-p 'vterm-mode)
    ;; vterm does not expose a stable generic input ring the way comint/eshell
    ;; do, so fall back to a prompt-shaped line heuristic.
    (timfel/agent-shell-context--guess-last-command-from-output
     (timfel/agent-shell-context--tail-lines
      (point-min) (point-max) (* 3 timfel/agent-shell-context-shell-output-lines))))
   (t nil)))

(defun timfel/agent-shell-context--shell-snippet ()
  "Return shell context for the current buffer."
  (let* ((output (string-trim-right
                  (timfel/agent-shell-context--tail-lines
                   (point-min) (point-max) timfel/agent-shell-context-shell-output-lines)))
         (command (timfel/agent-shell-context--last-shell-command))
         (command-already-present
          (and command
               (not (string-empty-p output))
               (string-match-p (regexp-quote command) output))))
    (string-join
     (delq nil
           (list (when (and command (not command-already-present))
                   (format "Last command: %s" command))
                 (unless (string-empty-p output)
                   (format "Recent output:\n%s" output))))
     "\n\n")))

(defun timfel/agent-shell-context--buffer-snippet (buffer)
  "Extract a concise snippet from BUFFER."
  (with-current-buffer buffer
    (if (derived-mode-p 'vterm-mode 'term-mode 'eshell-mode 'comint-mode 'shell-mode)
        (timfel/agent-shell-context--shell-snippet)
      (timfel/agent-shell-context--lines-around-point))))

(defun timfel/agent-shell-context--format-xref-history (history-list limit project-root)
  "Format up to LIMIT items from HISTORY-LIST into a readable path.

Return nil unless at least one xref target buffer would qualify for inclusion
under PROJECT-ROOT."
  (let ((items (seq-take history-list limit))
        (path '())
        (has-eligible-buffer nil))
    (dolist (item items)
      (let ((marker (cond
                     ((markerp item) item)
                     ((and (consp item) (markerp (car item))) (car item))
                     ((and (consp item) (markerp (cdr item))) (cdr item))
                     (t nil))))
        (when (and marker (marker-buffer marker))
          (let ((buffer (marker-buffer marker)))
            (when (timfel/agent-shell-context--eligible-buffer-p buffer project-root)
              (setq has-eligible-buffer t))
            (with-current-buffer buffer
              (save-excursion
                (goto-char marker)
                (push (format "%s (%s)"
                              (or (thing-at-point 'symbol t)
                                  (format "[%d]" (line-number-at-pos)))
                              (buffer-name))
                      path)))))))
    (when has-eligible-buffer
      (string-join (reverse path) " -> "))))

(defun timfel/agent-shell-context-source ()
  "Gather recent Emacs context for SHELL-BUFFER that has not already been sent.

This current includes:

   - the command to use to gather more info from the live emacs session

   - the current point and hunk around buffers in the same project.el
     project up to TIMFEL/AGENT-SHELL-CONTEXT-BUFFER-LIMIT

   - recent xref symbol navigation within the same project
"
  (let* ((context-parts '())
         (buffers-found 0)
         (project-root (timfel/agent-shell-context--project-root (current-buffer)))
         ;; let the agent know if it may inspect my emacs
         (emacsclient-command
          (when (and (boundp 'server-process)
                     (process-live-p server-process))
            (string-join
             `("emacsclient"
               ,(cond
                 ((or server-use-tcp
                      (memq system-type '(windows-nt ms-dos cygwin)))
                  (format "--server-file=%s"
                          (shell-quote-argument (expand-file-name server-name server-auth-dir))))
                 (t
                  (format "--socket-name=%s"
                          (shell-quote-argument (expand-file-name server-name server-socket-dir)))))
               ,(if (eq system-type 'windows-nt)
                    "--eval '(load-file (string ASCII CHAR CODES OF PATH TO ELISP SCRIPT))'"
                  ""))
             " "))))
    (catch 'timfel/agent-shell-context-limit-reached
      (dolist (buffer (seq-subseq (buffer-list)
                                  0
                                  (min (seq-length (buffer-list))
                                       (* 2 timfel/agent-shell-context-buffer-limit))))
        (when (timfel/agent-shell-context--eligible-buffer-p buffer project-root)
          (let* ((name (buffer-name buffer))
                 (state (timfel/agent-shell-context--buffer-state buffer))
                 (last-state (gethash name timfel/agent-shell-context-sent-states)))
            (unless (equal state last-state)
              (let ((snippet (string-trim
                              (timfel/agent-shell-context--buffer-snippet buffer))))
                (unless (string-empty-p snippet)
                  (push (format "### Buffer: %s\n```\n%s\n```" name snippet)
                        context-parts)
                  (puthash name state timfel/agent-shell-context-sent-states)
                  (setq buffers-found (1+ buffers-found))
                  (when (>= buffers-found timfel/agent-shell-context-buffer-limit)
                    (throw 'timfel/agent-shell-context-limit-reached nil)))))))))
    (let* ((current-xref (and (boundp 'xref--history)
                              (seq-take xref--history 5)))
           (xref-summary (and current-xref
                              (timfel/agent-shell-context--format-xref-history
                               current-xref 5 project-root))))
      (when (and xref-summary
                 (not (string-empty-p xref-summary))
                 (not (equal xref-summary timfel/agent-shell-context-last-xref-summary)))
        (push (format "### Recent Navigation (Xref)\n%s" xref-summary)
              context-parts)
        (setq timfel/agent-shell-context-last-xref-summary xref-summary)))
    (when context-parts
      (concat
       "\n\n[USER ENVIRONMENT CONTEXT - EMACS STATE]\n"
       (if emacsclient-command
           (concat
            "FIRST evaluate if some contextual buffers may be relevant "
            "to this prompt. If so you SHOULD use `" emacsclient-command "` "
            "to query contents and key map bindings of those buffers, those "
            "will reveal ways to collect more data before working on the task. "
            "Many buffers have key bindings that lead to more information, "
            "to logs, issue comments, history, TODOs, etc.\n"
            (if (eq system-type 'windows-nt)
                (concat
                 "ALWAYS write a temporary script file with the elisp code, "
                 "that prints output using (princ), this avoids quoting issues. "
                 "DO NOT write the elisp file using the shell, use your edit "
                 "tool.\n\n")
              ""))
         "\n")
       (mapconcat #'identity (nreverse context-parts) "\n\n")
       "[END CONTEXT]\n"))))

(provide 'timfel-agent-shell-context)

;;; timfel-agent-shell-context.el ends here
