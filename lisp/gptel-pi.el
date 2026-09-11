;;; gptel-pi.el --- GPTel configuration to be a minimal agent. Like Pi :) -*- lexical-binding: t -*-

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

;;; Author: Tim Felgentreff <timfelgentreff@gmail.com>

;;; Commentary:

;; gptel-pi is a small, Org-native coding-agent harness built on top of gptel.
;; It supplies a coding system prompt, a handful of local tools, bounded tool
;; use, durable in-buffer tool receipts, explicit transcript compaction, and
;; advisory context accounting.  gptel still owns requests, streaming, tool
;; dispatch, response properties, persistence, and rewrite review.
;;
;; Design principles
;; -----------------
;;
;; The conversation buffer is the source of truth.  Conversation history,
;; archived output, compaction checkpoints, links, and branches are ordinary
;; editable Org text.  This library deliberately does not provide a dashboard,
;; command palette, custom agent loop, automatic retry, automatic compaction,
;; or a second acceptance UI.  In particular, it does not advise
;; `gptel-send'.  It uses gptel's public request and tool hooks and composes
;; with the normal `gptel-rewrite' workflow.
;;
;; User-created Org headings define conversation branches.  Model-created
;; headings do not: after each completed response,
;; `gptel-pi-normalize-assistant-headings' changes assistant headings such as
;; "** Findings" into presentational labels such as "*Findings*".  It leaves
;; user text and headings inside source, example, quote, reasoning, and tool
;; blocks alone.
;;
;; Starting and using a session
;; ----------------------------
;;
;; Run `gptel-pi' in a project.  It reuses the most recently visited gptel-pi
;; session for the current project root.  With C-u it creates another session;
;; with C-u C-u it prompts for an existing session.  The command uses
;; `project-current' when possible and otherwise uses `default-directory'.
;; Continue to send prompts with ordinary `gptel-send' and use the standard
;; gptel commands and menus for model, backend, tool, and request control.
;;
;; A new Org session has this shape:
;;
;;   * Archive
;;   ** Tool outputs
;;   ** Compactions
;;   * Conversation
;;   @user
;;
;; Point starts in Conversation.  `gptel-org-branching-context' is enabled
;; buffer-locally, so the Archive sibling is not replayed while point remains
;; in the active conversation lineage.  Following an archive link is ordinary
;; Org navigation; return to Conversation before sending another prompt unless
;; the archive itself is intentionally being used as context.
;;
;; Saved Org chats retain the gptel-pi preset through gptel's normal state
;; persistence.  When such a file is reopened and `gptel-mode' restores the
;; preset, gptel-pi reinstalls its buffer-local hooks and mode-line entries.
;;
;; Important commands
;; ------------------
;;
;; - `gptel-pi' starts, reuses, or selects a project session.
;; - `gptel-send' is the unchanged, standard way to send a prompt.
;; - `gptel-pi-archive-region' copies a selected transcript region verbatim to
;;   Archive/Compactions and leaves an ordinary link beside the source.
;; - `gptel-pi-compact-region' first performs that archival operation and then
;;   starts standard `gptel-rewrite' with tools disabled and a checkpoint
;;   directive.  Accept, reject, diff, ediff, merge, or iterate using gptel's
;;   normal rewrite actions.  Rejecting a rewrite intentionally leaves the
;;   visible archive snapshot and link, which can be removed by normal editing
;;   or undo.
;;
;; Tools and retained output
;; -------------------------
;;
;; The preset advertises five tools: `read', `bash', `eval', `write', and
;; `edit'.  Reads use one-indexed line offsets, independent line and UTF-8 byte
;; limits, head truncation, and actionable continuation offsets.  Bash uses a
;; pipe rather than a PTY, keeps the useful tail, separates stdout and stderr,
;; and reports exit, timeout, and signal status.  Eval is intended for relevant
;; live Emacs state, not general project exploration.  Edit rejects empty or
;; non-unique old text.  Write is intended for new files or complete rewrites.
;;
;; While Bash or eval is running, a purely visual tail overlay is shown at the
;; end of the session buffer.  Bash's stdout and stderr process buffers are
;; sampled periodically; the overlay is removed when the tool result arrives.
;; It never inserts transcript text or marks the conversation modified.  Eval
;; is synchronous, so it can show a waiting indicator but cannot expose
;; intermediate output before evaluation returns.
;;
;; Large read, Bash, and printed eval results are stored in Archive/Tool
;; outputs.  The active tool result contains a concise receipt and an ordinary
;; Org link to the complete output.  The archive is in the same buffer; no
;; temporary file is treated as durable history.  Text reads are the supported
;; read path at present; binary media should be attached through gptel's normal
;; media/context facilities rather than assumed to be available from a tool
;; result in the current turn.
;;
;; Tool calls are autonomous under the preset, so the user is assumed to be
;; comfortable allowing the advertised file and shell operations in the
;; current project.  gptel-pi stops a request through
;; `gptel-pre-tool-call-functions' after a hard budget, repeated identical
;; calls, or repeated identical failures.  It never restarts the request.  A
;; small "Pi tools N" mode-line item shows the current count and adds "!" near
;; or at a stop condition.
;;
;; If `agent-shell-command-prefix' is buffer-local and effective, Bash commands
;; are prefixed with it.  In that sandboxed situation the Emacs-hosted read,
;; write, edit, and eval tools are removed from the request because they would
;; bypass the shell sandbox.  Bash assumes `shell-file-name' accepts
;; `shell-command-switch'; positive timeouts additionally assume a compatible
;; external `timeout' executable.
;;
;; Project context and workflow assumptions
;; ----------------------------------------
;;
;; The harness assumes coding work is rooted at an Emacs project (or at least a
;; meaningful `default-directory'), that prompts and assistant replies remain
;; in the Org conversation, and that the user creates branches with normal Org
;; headings.  A root AGENTS.md, the local and home skill indexes, and nearby
;; subdirectory AGENTS.md paths are inserted as editable quote blocks under
;; the Conversation heading, so they remain part of the prompt and can be
;; changed directly in the buffer.
;;
;; Context pressure is advisory only.  "Pi ctx N%" estimates the active Org
;; lineage plus the system prompt, uses model context-window metadata when
;; present, and otherwise falls back to `gptel-pi-default-context-window' and
;; `gptel-pi-context-chars-per-token'.  One or two exclamation marks indicate
;; warning and critical thresholds.  The estimate never blocks or intercepts
;; a send, mutates history, invokes compaction, or retries an overflow.  Exact
;; provider token accounting is preferred conceptually, but the current
;; indicator is intentionally documented as an estimate.

;;; Code:

(require 'gptel)
(require 'gptel-context)
(require 'gptel-org)
(require 'org)
(require 'project)
(require 'cl-lib)
(require 'subr-x)
(require 'eww)

(defvar gptel-rewrite-directives-hook)
(defvar gptel-post-rewrite-functions)
(defvar gptel--rewrite-message)

(defvar-local gptel-pi-session-p nil
  "Non-nil when the current buffer is a gptel-pi session.")

(defcustom gptel-pi-max-tool-calls 40
  "Hard limit on tool calls in one gptel request."
  :type 'integer
  :group 'gptel)

(defcustom gptel-pi-max-identical-tool-calls 3
  "Number of identical tool calls allowed before stopping a request."
  :type 'integer
  :group 'gptel)

(defcustom gptel-pi-max-identical-tool-errors 2
  "Number of identical tool failures allowed before stopping a request."
  :type 'integer
  :group 'gptel)

(defcustom gptel-pi-context-chars-per-token 3.0
  "Conservative fallback character-to-token ratio for context estimates."
  :type 'number
  :group 'gptel)

(defcustom gptel-pi-default-context-window 128000
  "Fallback model context window in tokens when metadata is unavailable."
  :type 'integer
  :group 'gptel)

(defcustom gptel-pi-context-warning-percent 75
  "Context percentage at which the mode-line estimate shows one warning mark."
  :type 'integer
  :group 'gptel)

(defcustom gptel-pi-context-critical-percent 90
  "Context percentage at which the mode-line estimate shows two warning marks."
  :type 'integer
  :group 'gptel)

(defvar-local gptel-pi--tool-call-count 0)
(defvar-local gptel-pi--tool-call-fingerprints nil)
(defvar-local gptel-pi--tool-error-fingerprints nil)
(defvar-local gptel-pi--tools-active-p nil)
(defvar-local gptel-pi--tools-stopped-p nil)
(defvar-local gptel-pi--request-active-p nil)
(defvar-local gptel-pi--context-percent 0)
(defvar-local gptel-pi--context-warning-level 0)
(defvar-local gptel-pi--context-update-timer nil)
(defvar-local gptel-pi--context-branch nil)
(defvar-local gptel-pi--tool-overlays nil
  "Live, purely visual overlays for currently running tools.")

(defcustom gptel-pi-tool-overlay-max-lines 12
  "Maximum number of output lines shown in a running-tool overlay."
  :type 'integer
  :group 'gptel)

(defcustom gptel-pi-tool-overlay-max-bytes 4096
  "Maximum UTF-8 bytes shown in a running-tool overlay."
  :type 'integer
  :group 'gptel)

(defconst gptel-pi-system-prompt "You are an expert coding assistant.
You help users with coding tasks by reading files, executing commands, editing code, and writing new files.

Guidelines:
- Use `eval` to get recent buffers, kill ring, or other editor state
- Use `bash` for file operations like ls, grep, find
- Use `read` to examine files before editing
- Use `edit` for precise changes
- Use `write` only for new files or complete rewrites
- When summarizing your actions, output plain text directly - do NOT use cat or bash to display what you did
- Be concise in your responses")

(defconst gptel-pi--protected-org-blocks
  '("src" "example" "quote" "reasoning" "tool")
  "Org block names in which assistant headings remain untouched.")

(defun gptel-pi--response-properties (position)
  "Return role-related text properties copied from POSITION."
  (let ((gptel (get-text-property position 'gptel)))
    (when gptel
      (list 'gptel gptel 'front-sticky '(gptel)))))

(defun gptel-pi--normalize-heading-at-point (begin)
  "Turn the Org heading at point into a bold label when it starts after BEGIN."
  (when (looking-at "^\\(\\*+\\)[ \t]+\\(.+?\\)[ \t]*$")
    (let* ((heading-start (match-beginning 1))
           (title-start (match-beginning 2))
           (title-end (match-end 2))
           (properties (gptel-pi--response-properties title-start)))
      (when (>= heading-start begin)
        (goto-char title-end)
        (insert (apply #'propertize "*" properties))
        (delete-region heading-start title-start)
        (goto-char heading-start)
        (insert (apply #'propertize "*" properties))))))

(defun gptel-pi-normalize-assistant-headings (begin end)
  "Turn assistant Org headings between BEGIN and END into bold labels.

Headings in source, example, quote, reasoning, and tool blocks are not changed."
  (when (and gptel-pi-session-p
             (derived-mode-p 'org-mode)
             (< begin end))
    (let ((end-marker (copy-marker end t))
          (case-fold-search t)
          block-stack)
      (unwind-protect
          (save-excursion
            (save-restriction
              (widen)
              (goto-char begin)
              (unless (bolp) (forward-line 1))
              (while (< (point) end-marker)
                (let ((next-line
                       (copy-marker
                        (save-excursion (forward-line 1) (point)) t)))
                  (cond
                   ((looking-at "^[ \t]*#\\+begin_\\([[:alnum:]_-]+\\)\\b")
                    (let ((name (downcase (match-string-no-properties 1))))
                      (when (member name gptel-pi--protected-org-blocks)
                        (push name block-stack))))
                   ((looking-at "^[ \t]*#\\+end_\\([[:alnum:]_-]+\\)\\b")
                    (let ((name (downcase (match-string-no-properties 1))))
                      (when (equal name (car block-stack))
                        (pop block-stack))))
                   ((null block-stack)
                    (gptel-pi--normalize-heading-at-point begin)))
                  (goto-char next-line)
                  (set-marker next-line nil)))))
        (set-marker end-marker nil)))))

(defun gptel-pi--insert-project-context (root)
  "Insert editable project instructions for ROOT at the start of the buffer."
  (let* ((heading (if (eq gptel-default-mode 'org-mode) "**" "#"))
         (begin-quote (if (eq gptel-default-mode 'org-mode)
                          "\n#+begin_quote %s\n"
                        "\n``` %s\n"))
         (end-quote
          (lambda ()
            (if (eq gptel-default-mode 'org-mode)
                (insert "#+end_quote\n")
              (insert "```\n"))
            (let ((point (point)))
              (forward-line -1)
              (if (eq gptel-default-mode 'org-mode)
                  (org-cycle)
                (gptel-markdown-cycle-block))
              (goto-char point))))
         (agents (expand-file-name "AGENTS.md" root))
         (agents (and (file-readable-p agents) agents))
         (skills
          (cl-loop for directory in (list (expand-file-name ".agents/skills/" root)
                                          (expand-file-name "~/.agents/skills/"))
                   when (file-directory-p directory)
                   append
                   (cl-loop for entry in (directory-files directory t)
                            when (and (file-directory-p entry)
                                      (file-exists-p (file-name-concat entry "SKILL.md")))
                            collect (file-name-concat entry "SKILL.md"))))
         (extra-agents
          (cl-remove-if
           (lambda (path) (and agents (string-equal path agents)))
           (directory-files-recursively
            root "AGENTS\\.md\\'" nil
            (lambda (directory)
              (< (length (file-name-split (file-relative-name directory root)))
                 4))))))
    (when (or agents skills extra-agents)
      (goto-char (point-min))
      (insert (format "%s Project instructions:\n" heading))
      (when agents
        (insert (format begin-quote "AGENTS.md"))
        (let ((start (point)))
          (forward-char (cadr (insert-file-contents agents)))
          (indent-rigidly start (point) 1))
        (funcall end-quote))
      (when skills
        (insert (format begin-quote
                        "Skills (read when it might be useful for the user request)"))
        (dolist (skill skills)
          (insert " - " (file-relative-name skill root) " : ")
          ;; Keep the editable index concise, as the old buffer insertion did.
          (let ((end (+ (point) (cadr (insert-file-contents skill nil 0 500)))))
            (when (re-search-forward
                   (rx (or (seq "---" (* (or space "\\n"))
                                "name: " (group (* not-newline))
                                (* (or space "\\n"))
                                "description:" (group (* not-newline)) (* anything))
                           (* anything)))
                   end t)
              (replace-match "\\1: \\2\n"))))
        (funcall end-quote))
      (when extra-agents
        (insert (format begin-quote
                        "Subdirectory-specific instructions (read when operating underneath)"))
        (dolist (agent extra-agents)
          (insert " - " (file-relative-name agent root) "\n"))
        (funcall end-quote))
      (insert "\n")
      (run-with-idle-timer 0.1 nil #'org-fold-hide-block-all)
      (goto-char (point-max)))))

(defun gptel-pi--initialize-org-session ()
  "Give a new Org session explicit archive and conversation subtrees."
  (when (derived-mode-p 'org-mode)
    (let ((initial (buffer-substring (point-min) (point-max))))
      (erase-buffer)
      (insert "* Archive\n** Tool outputs\n\n** Compactions\n\n"
              "* Conversation\n"
              initial)
      (goto-char (point-max)))))

(defun gptel-pi--next-target (kind)
  "Return a unique, monotonically increasing target for KIND."
  (let ((regexp (format "<<gptel-pi-%s-\\([0-9]+\\)>>" (regexp-quote kind)))
        (maximum 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (setq maximum (max maximum (string-to-number (match-string 1))))))
    (format "gptel-pi-%s-%04d" kind (1+ maximum))))

(defun gptel-pi--archive-section-position (section)
  "Return insertion position at the end of Archive subsection SECTION."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^\\* Archive[ \t]*$" nil t)
      (error "This gptel-pi buffer has no Archive heading"))
    (let ((archive-end (save-excursion (org-end-of-subtree t) (point))))
      (unless (re-search-forward
               (format "^\\*\\* %s[ \t]*$" (regexp-quote section))
               archive-end t)
        (error "This gptel-pi buffer has no Archive/%s section" section))
      (org-end-of-subtree t)
      (point))))

(defun gptel-pi--archive-entry (kind section title body &optional metadata)
  "Archive BODY under SECTION and return a link to its KIND target.

TITLE is used for the level-three heading.  Optional METADATA is ordinary Org
text inserted before the example block."
  (unless (and gptel-pi-session-p (derived-mode-p 'org-mode))
    (error "Tool output can only be archived in an Org gptel-pi session"))
  (let ((target (gptel-pi--next-target kind)))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (gptel-pi--archive-section-position section))
        (unless (bolp) (insert "\n"))
        (insert (format "*** %s\n<<%s>>\n\n" title target))
        (when metadata
          (insert metadata)
          (unless (string-suffix-p "\n" metadata) (insert "\n"))
          (insert "\n"))
        (insert "#+begin_example\n"
                (org-escape-code-in-string body))
        (unless (string-suffix-p "\n" body) (insert "\n"))
        (insert "#+end_example\n\n")))
    (format "[[%s][%s]]" target title)))

(defun gptel-pi--archive-transcript (text)
  "Archive transcript TEXT verbatim and return its link."
  (let* ((target (gptel-pi--next-target "compaction"))
         (number (progn
                   (string-match "-\\([0-9]+\\)\\'" target)
                   (string-to-number (match-string 1 target))))
         (title (format "compaction %04d" number)))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (gptel-pi--archive-section-position "Compactions"))
        (unless (bolp) (insert "\n"))
        (insert (format "*** %s\n<<%s>>\n\n" title target) text)
        (unless (string-suffix-p "\n" text) (insert "\n"))
        (insert "\n")))
    (format "[[%s][compaction archive %04d]]" target number)))

(defun gptel-pi--inside-tool-block-p (position)
  "Return non-nil when POSITION splits an Org gptel tool block."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
          (depth 0))
      (while (re-search-forward
              "^[ \t]*#\\+\\(begin\\|end\\)_tool\\b" position t)
        (if (string-equal (downcase (match-string 1)) "begin")
            (setq depth (1+ depth))
          (setq depth (max 0 (1- depth)))))
      (> depth 0))))

(defun gptel-pi--validate-compaction-region (begin end)
  "Validate that BEGIN and END form a complete compaction region."
  (unless (< begin end)
    (user-error "Select a non-empty region to compact"))
  (when (or (gptel-pi--inside-tool-block-p begin)
            (gptel-pi--inside-tool-block-p end))
    (user-error "Compaction region must not split a gptel tool block")))

;;;###autoload
(defun gptel-pi-archive-region (begin end)
  "Copy the active region from BEGIN to END into the compaction archive.

The source is left untouched, an ordinary link is inserted immediately after
it, and the original source remains the active region."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (user-error "Select a region to archive")))
  (unless (and gptel-pi-session-p (derived-mode-p 'org-mode))
    (user-error "This command requires an Org gptel-pi session"))
  (unless (< begin end)
    (user-error "Select a non-empty region to archive"))
  (let ((begin-marker (copy-marker begin nil))
        (end-marker (copy-marker end nil))
        (text (buffer-substring begin end))
        link)
    (unwind-protect
        (progn
          (setq link (gptel-pi--archive-transcript text))
          (goto-char end-marker)
          (unless (bolp) (insert "\n"))
          (insert (format "\nOriginal transcript: %s\n" link))
          (goto-char end-marker)
          (set-mark begin-marker)
          (setq deactivate-mark nil)
          (activate-mark)
          link)
      (set-marker begin-marker nil)
      (set-marker end-marker nil))))

(defconst gptel-pi--compaction-directive
  (concat
   "Summarize the selected transcript as a standalone context checkpoint. "
   "Generate only replacement checkpoint text and do not continue the conversation. "
   "Do not use real Org headings; use bold labels and lists. Preserve exact paths, "
   "function names, decisions, current errors and test results, next steps, and the "
   "cumulative read and modified file lists. Use this format:\n\n"
   "*Context checkpoint*\n\n"
   "- *Goal:* ...\n- *Constraints:* ...\n- *Completed:* ...\n"
   "- *Current work:* ...\n- *Blocked:* ...\n- *Decisions:* ...\n"
   "- *Next steps:* ...\n- *Critical context:* ...\n"
   "- *Read files:* ...\n- *Modified files:* ...")
  "Rewrite instruction used for explicit transcript compaction.")

(defun gptel-pi--compaction-system-directive ()
  "Return the standalone system directive for compaction rewrites."
  (concat
   "You compact coding-agent transcripts. " gptel-pi--compaction-directive
   "\nDo not call tools, narrate progress, add markdown fences, or add commentary."))

(defun gptel-pi--start-compaction-rewrite ()
  "Start standard `gptel-rewrite' for the active compaction region."
  (require 'gptel-rewrite)
  ;; gptel does not yet expose a public argument for the initial rewrite
  ;; instruction.  Dynamically binding its documented transient infix backing
  ;; variable avoids any dependency on rewrite overlays or callbacks.
  (let ((gptel-use-tools nil)
        (gptel-tools nil)
        (gptel--rewrite-message gptel-pi--compaction-directive)
        (gptel-rewrite-directives-hook
         (cons #'gptel-pi--compaction-system-directive
               gptel-rewrite-directives-hook)))
    (call-interactively #'gptel-rewrite)))

;;;###autoload
(defun gptel-pi-compact-region (begin end)
  "Archive and invoke standard `gptel-rewrite' on the region BEGIN to END."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (user-error "Select a region to compact")))
  (when (or gptel-pi--request-active-p gptel-pi--tools-active-p)
    (user-error "Wait for the active gptel request to finish before compacting"))
  (gptel-pi--validate-compaction-region begin end)
  (gptel-pi-archive-region begin end)
  (gptel-pi--start-compaction-rewrite))

(defun gptel-pi--archive-tool-output (tool summary output metadata)
  "Archive complete tool OUTPUT and return its ordinary Org link.

TOOL and SUMMARY form the archive heading, and METADATA is inserted before the
output block."
  (let* ((next (gptel-pi--next-target "tool"))
         (number (string-to-number
                  (progn (string-match "-\\([0-9]+\\)\\'" next)
                         (match-string 1 next))))
         (clean-summary (truncate-string-to-width
                         (replace-regexp-in-string "[\n\r]+" " " summary)
                         80 nil nil "…"))
         (title (format "%s %04d: %s" tool number clean-summary)))
    (gptel-pi--archive-entry "tool" "Tool outputs" title output metadata)))


(defun gptel-pi--normalize-fingerprint-value (value)
  "Return VALUE in a deterministic form suitable for fingerprints."
  (cond
   ((and (listp value) (keywordp (car value)))
    (sort (cl-loop for (key val) on value by #'cddr
                   collect (cons key (gptel-pi--normalize-fingerprint-value val)))
          (lambda (left right)
            (string< (symbol-name (car left)) (symbol-name (car right))))))
   ((vectorp value)
    (vconcat (mapcar #'gptel-pi--normalize-fingerprint-value value)))
   ((consp value)
    (mapcar #'gptel-pi--normalize-fingerprint-value value))
   (t value)))

(defun gptel-pi--tool-fingerprint (name args)
  "Return a stable fingerprint for tool NAME and ARGS."
  (secure-hash 'sha256
               (prin1-to-string
                (list name (gptel-pi--normalize-fingerprint-value args)))))

(defun gptel-pi--increment-fingerprint (table fingerprint)
  "Increment FINGERPRINT in hash TABLE and return its new count."
  (let ((count (1+ (gethash fingerprint table 0))))
    (puthash fingerprint count table)
    count))

(defun gptel-pi--tool-overlay-key (name args)
  "Return the display key for tool NAME and ARGS.

The key deliberately ignores optional operational arguments such as Bash's
 timeout: two calls for the same command should use the same visual identity."
  (list name
        (or (plist-get args :command)
            (plist-get args :elisp)
            (plist-get args :path)
            (prin1-to-string (gptel-pi--normalize-fingerprint-value args)))))

(defun gptel-pi--tool-overlay-output (overlay)
  "Return the currently available output associated with OVERLAY."
  (let ((stdout (overlay-get overlay 'gptel-pi-stdout-buffer))
        (stderr (overlay-get overlay 'gptel-pi-stderr-buffer)))
    (concat
     (when (buffer-live-p stdout)
       (with-current-buffer stdout (buffer-string)))
     (when (buffer-live-p stderr)
       (with-current-buffer stderr (buffer-string))))))

(defun gptel-pi--tool-overlay-update (overlay)
  "Update OVERLAY's visual tail without changing its buffer's text."
  (when (and (overlayp overlay) (overlay-buffer overlay))
    (let* ((name (overlay-get overlay 'gptel-pi-tool-name))
           (args (overlay-get overlay 'gptel-pi-tool-args))
           (summary (or (plist-get args :command)
                        (plist-get args :elisp)
                        (plist-get args :path)
                        (prin1-to-string args)))
           (output (gptel-pi--tool-overlay-output overlay))
           (tail (if (string-empty-p output)
                     "Waiting for output …"
                   (plist-get
                    (gptel-pi--truncate-result
                     output 'tail gptel-pi-tool-overlay-max-bytes
                     gptel-pi-tool-overlay-max-lines)
                    :content))))
      (overlay-put
       overlay 'after-string
       (concat
        "\n"
        (propertize
         (format "⏳ %s: %s\n" name
                 (truncate-string-to-width
                  (replace-regexp-in-string "[\\n\\r]+" " " summary)
                  100 nil nil "…"))
         'face 'mode-line-emphasis)
        (propertize tail 'face 'shadow)
        (unless (string-suffix-p "\\n" tail) "\\n")))
      ;; Keep the display at the end when gptel inserts its tool receipt.
      (move-overlay overlay (point-max) (point-max) (current-buffer)))))

(defun gptel-pi--tool-overlay-tick (overlay)
  "Refresh OVERLAY while its asynchronous tool is running."
  (when (and (overlayp overlay) (overlay-buffer overlay))
    (with-current-buffer (overlay-buffer overlay)
      (gptel-pi--tool-overlay-update overlay)
      (redisplay t))))

(defun gptel-pi--begin-tool-overlay (name args)
  "Create a purely visual running-tool overlay for NAME and ARGS."
  (when (and gptel-pi-session-p (derived-mode-p 'org-mode))
    (let ((overlay (make-overlay (point-max) (point-max) nil t t)))
      (overlay-put overlay 'gptel-pi-tool-key
                   (gptel-pi--tool-overlay-key name args))
      (overlay-put overlay 'gptel-pi-tool-name name)
      (overlay-put overlay 'gptel-pi-tool-args args)
      (push overlay gptel-pi--tool-overlays)
      (gptel-pi--tool-overlay-update overlay)
      ;; A synchronous eval cannot repaint while it is running.  Repaint now so
      ;; that its waiting indicator is visible before the evaluator takes over.
      (redisplay t)
      (let ((timer (run-with-timer
                    0.2 0.2 #'gptel-pi--tool-overlay-tick overlay)))
        (overlay-put overlay 'gptel-pi-timer timer))
      overlay)))

(defun gptel-pi--find-tool-overlay (name args)
  "Find the running overlay for tool NAME and ARGS in the current buffer."
  (let ((key (gptel-pi--tool-overlay-key name args)))
    (seq-find (lambda (overlay)
                (and (overlay-buffer overlay)
                     (equal key (overlay-get overlay 'gptel-pi-tool-key))))
              gptel-pi--tool-overlays)))

(defun gptel-pi--remove-tool-overlay (name args)
  "Remove the running-tool overlay for NAME and ARGS."
  (let ((overlay (gptel-pi--find-tool-overlay name args)))
    (when overlay
      (let ((timer (overlay-get overlay 'gptel-pi-timer)))
        (when (timerp timer) (cancel-timer timer)))
      (setq gptel-pi--tool-overlays (delq overlay gptel-pi--tool-overlays))
      (delete-overlay overlay))))

(defun gptel-pi--remove-tool-overlays ()
  "Remove all running-tool overlays in the current buffer."
  (dolist (overlay gptel-pi--tool-overlays)
    (let ((timer (overlay-get overlay 'gptel-pi-timer)))
      (when (timerp timer) (cancel-timer timer)))
    (delete-overlay overlay))
  (setq gptel-pi--tool-overlays nil))

(defun gptel-pi--pre-tool-call (info)
  "Enforce the per-request tool budget using public hook INFO."
  (unless gptel-pi--tool-call-fingerprints
    (setq gptel-pi--tool-call-fingerprints (make-hash-table :test #'equal)))
  (unless gptel-pi--tool-error-fingerprints
    (setq gptel-pi--tool-error-fingerprints (make-hash-table :test #'equal)))
  (setq gptel-pi--tools-active-p t
        gptel-pi--tool-call-count (1+ gptel-pi--tool-call-count))
  (let* ((name (plist-get info :name))
         (fingerprint (gptel-pi--tool-fingerprint name (plist-get info :args)))
         (identical-count
          (gptel-pi--increment-fingerprint
           gptel-pi--tool-call-fingerprints fingerprint))
         reason)
    (cond
     ((> gptel-pi--tool-call-count gptel-pi-max-tool-calls)
      (setq reason
            (format "gptel-pi stopped after the %d-call tool budget was exhausted"
                    gptel-pi-max-tool-calls)))
     ((>= identical-count gptel-pi-max-identical-tool-calls)
      (setq reason
            (format "gptel-pi stopped repeated call %s with identical arguments (%d times)"
                    name identical-count))))
    (when (and (null reason) (member name '("bash" "eval")))
      (gptel-pi--begin-tool-overlay name (plist-get info :args)))
    (when reason
      (setq gptel-pi--tools-stopped-p t)
      (force-mode-line-update)
      (list :stop t :stop-reason reason))))

(defun gptel-pi--failure-result-p (result)
  "Return non-nil when RESULT clearly describes a tool failure."
  (and (stringp result)
       (string-match-p
        (rx (or string-start line-start)
            (* space)
            (or "Error:"
                (seq "Bash exited with status " (not (any "0\n")))
                "Bash timed out"
                "Bash was terminated by signal"))
        result)))

(defun gptel-pi--post-tool-call (info)
  "Stop after repeated identical failures described by hook INFO."
  (when (member (plist-get info :name) '("bash" "eval"))
    (gptel-pi--remove-tool-overlay (plist-get info :name)
                                    (plist-get info :args)))
  (when (gptel-pi--failure-result-p (plist-get info :result))
    (unless gptel-pi--tool-error-fingerprints
      (setq gptel-pi--tool-error-fingerprints (make-hash-table :test #'equal)))
    (let* ((fingerprint
            (secure-hash
             'sha256
             (prin1-to-string
              (list (gptel-pi--tool-fingerprint
                     (plist-get info :name) (plist-get info :args))
                    (plist-get info :result)))))
           (count (gptel-pi--increment-fingerprint
                   gptel-pi--tool-error-fingerprints fingerprint)))
      (when (>= count gptel-pi-max-identical-tool-errors)
        (setq gptel-pi--tools-stopped-p t)
        (force-mode-line-update)
        (list :stop t
              :stop-reason
              (format "gptel-pi stopped after the same %s failure occurred %d times"
                      (plist-get info :name) count))))))

(defun gptel-pi--mark-request-active ()
  "Record that a public gptel request has been sent from this buffer."
  (setq gptel-pi--request-active-p t))

(defun gptel-pi--reset-tool-state (&rest _)
  "Reset per-request tool-loop state."
  (setq gptel-pi--request-active-p nil
        gptel-pi--tool-call-count 0
        gptel-pi--tool-call-fingerprints (make-hash-table :test #'equal)
        gptel-pi--tool-error-fingerprints (make-hash-table :test #'equal)
        gptel-pi--tools-active-p nil
        gptel-pi--tools-stopped-p nil)
  (gptel-pi--remove-tool-overlays)
  (force-mode-line-update))

(defun gptel-pi--tool-mode-line ()
  "Return the gptel-pi tool count for the mode line."
  (when gptel-pi--tools-active-p
    (format " Pi tools %d%s"
            gptel-pi--tool-call-count
            (if (or gptel-pi--tools-stopped-p
                    (>= gptel-pi--tool-call-count gptel-pi-max-tool-calls))
                "!" ""))))

(defun gptel-pi--active-org-lineage-string ()
  "Return the Org lineage up to point, matching gptel branching semantics."
  (if (not (and (derived-mode-p 'org-mode) gptel-org-branching-context))
      (buffer-substring-no-properties (point-min) (point))
    (save-excursion
      (let* ((source (current-buffer))
             (prompt-end (point))
             (lineage
              (org-element-lineage-map
                  (org-element-at-point)
                  (lambda (node) (org-element-property :begin node))
                '(headline) t))
             (starts
              (sort (append lineage
                            (unless (save-excursion
                                      (goto-char (point-min))
                                      (looking-at-p outline-regexp))
                              (list (point-min))))
                    #'<))
             ends)
        (setq ends
              (cl-loop for tail on starts
                       for start = (car tail)
                       collect
                       (if (cdr tail)
                           (progn
                             (goto-char start)
                             (outline-next-heading)
                             (point))
                         prompt-end)))
        (with-temp-buffer
          (cl-loop for start in starts
                   for end in ends
                   do (insert-buffer-substring-no-properties source start end))
          (buffer-string))))))

(defun gptel-pi--model-context-window ()
  "Return the current model context window in tokens."
  (let ((thousands (and (symbolp gptel-model)
                        (get gptel-model :context-window))))
    (if (numberp thousands)
        (round (* thousands 1000))
      gptel-pi-default-context-window)))

(defun gptel-pi--update-context-estimate (&rest _)
  "Recalculate the advisory active-lineage context estimate."
  (when (and gptel-pi-session-p (buffer-live-p (current-buffer)))
    (when (timerp gptel-pi--context-update-timer)
      (cancel-timer gptel-pi--context-update-timer))
    (setq gptel-pi--context-update-timer nil)
    (let* ((active (gptel-pi--active-org-lineage-string))
           (system (if (stringp gptel-system-prompt) gptel-system-prompt ""))
           (tokens (ceiling (/ (+ (length active) (length system))
                               gptel-pi-context-chars-per-token)))
           (window (max 1 (gptel-pi--model-context-window)))
           (percent (round (* 100.0 (/ (float tokens) window))))
           (level (cond ((>= percent gptel-pi-context-critical-percent) 2)
                        ((>= percent gptel-pi-context-warning-percent) 1)
                        (t 0))))
      (setq gptel-pi--context-percent percent)
      (when (> level gptel-pi--context-warning-level)
        (message
         "gptel-pi context is approximately %d%%; compact a region or edit the buffer before continuing"
         percent))
      (setq gptel-pi--context-warning-level level)
      (force-mode-line-update))))

(defun gptel-pi--schedule-context-update (&rest _)
  "Schedule an idle context estimate update for the current buffer."
  (when (timerp gptel-pi--context-update-timer)
    (cancel-timer gptel-pi--context-update-timer))
  (let ((buffer (current-buffer)))
    (setq gptel-pi--context-update-timer
          (run-with-idle-timer
           0.75 nil
           (lambda ()
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (gptel-pi--update-context-estimate))))))))

(defun gptel-pi--track-context-branch ()
  "Schedule context accounting when the active Org lineage changes."
  (when (derived-mode-p 'org-mode)
    (let ((branch (org-get-outline-path t t)))
      (unless (equal branch gptel-pi--context-branch)
        (setq gptel-pi--context-branch branch)
        (gptel-pi--schedule-context-update)))))

(defun gptel-pi--cancel-context-timer ()
  "Cancel the pending context update timer, if any."
  (when (timerp gptel-pi--context-update-timer)
    (cancel-timer gptel-pi--context-update-timer))
  (setq gptel-pi--context-update-timer nil))

(defun gptel-pi--context-mode-line ()
  "Return the advisory context percentage for the mode line."
  (format " Pi ctx %d%%%s" gptel-pi--context-percent
          (cond ((>= gptel-pi--context-percent
                     gptel-pi-context-critical-percent) "!!")
                ((>= gptel-pi--context-percent
                     gptel-pi-context-warning-percent) "!")
                (t ""))))

(defun gptel-pi--setup-buffer ()
  "Set up the current buffer as a gptel-pi session."
  (setq-local gptel-pi-session-p t
              gptel-org-branching-context t)
  (gptel-pi--reset-tool-state)
  (add-hook 'gptel-post-request-hook #'gptel-pi--mark-request-active nil t)
  (add-hook 'gptel-pre-tool-call-functions #'gptel-pi--pre-tool-call nil t)
  (add-hook 'gptel-post-tool-call-functions #'gptel-pi--post-tool-call nil t)
  (add-hook 'gptel-post-response-functions
            #'gptel-pi-normalize-assistant-headings nil t)
  (add-hook 'gptel-post-response-functions #'gptel-pi--update-context-estimate 80 t)
  (add-hook 'gptel-post-response-functions #'gptel-pi--reset-tool-state 90 t)
  (add-hook 'gptel-post-rewrite-functions #'gptel-pi--update-context-estimate 80 t)
  (add-hook 'gptel-post-rewrite-functions #'gptel-pi--reset-tool-state 90 t)
  (add-hook 'after-change-functions #'gptel-pi--schedule-context-update nil t)
  (add-hook 'post-command-hook #'gptel-pi--track-context-branch nil t)
  (add-hook 'kill-buffer-hook #'gptel-pi--cancel-context-timer nil t)
  (add-to-list (make-local-variable 'mode-line-misc-info)
               '(:eval (gptel-pi--tool-mode-line)) t)
  (add-to-list 'mode-line-misc-info
               '(:eval (gptel-pi--context-mode-line)) t)
  (gptel-pi--schedule-context-update))

(defun gptel-pi--maybe-setup-restored-buffer ()
  "Restore gptel-pi hooks when opening a saved gptel-pi Org session."
  (when (and gptel-mode
             (derived-mode-p 'org-mode)
             (string-equal (org-entry-get (point-min) "GPTEL_PRESET") "gptel-pi"))
    (gptel-pi--setup-buffer)))

(add-hook 'gptel-mode-hook #'gptel-pi--maybe-setup-restored-buffer)

(defun gptel-pi--sandboxed-p (buffer)
  "Return non-nil when BUFFER has an effective shell command prefix."
  (when (and (buffer-live-p buffer)
             (boundp 'agent-shell-command-prefix))
    (let ((prefix (buffer-local-value 'agent-shell-command-prefix buffer)))
      (if (functionp prefix)
          (with-current-buffer buffer
            (funcall prefix buffer))
        prefix))))

(defun gptel-pi--restrict-tools (fsm)
  "Remove Emacs-host tools from the request when shell access is sandboxed.

The shell command prefix only affects processes started by the `bash' tool;
`read', `write', `edit', and `eval' execute in Emacs itself.  Do not advertise
those tools in a sandboxed request."
  (let ((buffer (plist-get (gptel-fsm-info fsm) :buffer)))
    (when (gptel-pi--sandboxed-p buffer)
      (setq gptel-tools
            (cl-remove-if
             (lambda (tool)
               (member (gptel-tool-name tool) '("eval" "read" "write" "edit")))
             gptel-tools)))))

(defcustom gptel-pi-max-tool-bytes (* 50 1024)
  "Maximum UTF-8 bytes returned directly by a gptel-pi tool."
  :type 'integer
  :group 'gptel)

(defcustom gptel-pi-max-tool-lines 2000
  "Maximum complete lines returned directly by a gptel-pi tool."
  :type 'integer
  :group 'gptel)

(defun gptel-pi--truncate-result (text direction &optional max-bytes max-lines)
  "Truncate TEXT by complete lines and UTF-8 bytes.

DIRECTION is either `head' or `tail'.  MAX-BYTES and MAX-LINES default to
`gptel-pi-max-tool-bytes' and `gptel-pi-max-tool-lines'.  Return a plist with
`:content', `:truncated', `:lines', `:bytes', and `:first-line-too-long'."
  (let* ((max-bytes (or max-bytes gptel-pi-max-tool-bytes))
         (max-lines (or max-lines gptel-pi-max-tool-lines))
         (total-bytes (string-bytes text))
         segments)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((start (point)))
          (forward-line 1)
          (push (buffer-substring-no-properties start (point)) segments))))
    (setq segments (nreverse segments))
    (if (and (<= total-bytes max-bytes)
             (<= (length segments) max-lines))
        (list :content text :truncated nil :lines (length segments)
              :bytes total-bytes :first-line-too-long nil)
      (let ((candidates (if (eq direction 'tail) (reverse segments) segments))
            (kept nil)
            (bytes 0)
            (lines 0))
        (while (and candidates (< lines max-lines))
          (let* ((segment (car candidates))
                 (segment-bytes (string-bytes segment)))
            (if (> (+ bytes segment-bytes) max-bytes)
                (setq candidates nil)
              (push segment kept)
              (setq bytes (+ bytes segment-bytes)
                    lines (1+ lines)
                    candidates (cdr candidates)))))
        (when (eq direction 'head)
          (setq kept (nreverse kept)))
        (list :content (apply #'concat kept)
              :truncated t
              :lines lines
              :bytes bytes
              :first-line-too-long (and (eq direction 'head) (zerop lines)
                                         (not (string-empty-p text))))))))

(defun gptel-pi--format-read-result
    (origin path selected offset total-lines truncation)
  "Format a read result and archive SELECTED when TRUNCATION occurred."
  (let* ((shown-lines (plist-get truncation :lines))
         (end-line (+ offset shown-lines -1))
         (next-offset (and (< end-line total-lines) (1+ end-line)))
         (link (when (and (plist-get truncation :truncated)
                          (buffer-live-p origin)
                          (buffer-local-value 'gptel-pi-session-p origin))
                 (with-current-buffer origin
                   (gptel-pi--archive-tool-output
                    "read" (file-name-nondirectory path) selected
                    (format "- Path :: =%s=\n- Requested offset :: %d\n"
                            path offset))))))
    (if (plist-get truncation :first-line-too-long)
        (concat
         (format (concat "Line %d exceeds the %d-byte read limit. "
                         "Inspect it in chunks, for example with: "
                         "sed -n '%dp' %s | head -c %d")
                 offset gptel-pi-max-tool-bytes offset
                 (shell-quote-argument path) gptel-pi-max-tool-bytes)
         (when link (format "\n\nFull selected output: %s" link)))
      (concat
       (plist-get truncation :content)
       (unless (or (string-empty-p (plist-get truncation :content))
                   (string-suffix-p "\n" (plist-get truncation :content)))
         "\n")
       (format "[Showing lines %d-%d of %d%s.]"
               offset end-line total-lines
               (if next-offset
                   (format "; use offset=%d to continue" next-offset)
                 ""))
       (when link (format "\n\nFull selected output: %s" link))))))

(defun gptel-pi--tool-read (path &optional offset limit)
  "Read text file PATH from one-indexed OFFSET for at most LIMIT lines."
  (let ((expanded-path (expand-file-name path))
        (offset (or offset 1))
        (origin (current-buffer)))
    (cond
     ((or (not (integerp offset)) (< offset 1))
      (format "Error: offset must be a positive integer, got %S" offset))
     ((and limit (or (not (integerp limit)) (< limit 1)))
      (format "Error: limit must be a positive integer, got %S" limit))
     ((not (file-readable-p expanded-path))
      (format "Error: file is not readable: %s" expanded-path))
     ((file-directory-p expanded-path)
      (format "Error: path is a directory: %s" expanded-path))
     (t
      (with-temp-buffer
        (insert-file-contents expanded-path)
        (let ((total-lines (line-number-at-pos (point-max))))
          (cond
           ((zerop total-lines)
            (format "File is empty: %s" expanded-path))
           ((> offset total-lines)
            (format "Error: offset %d is beyond end of file (%d lines total): %s"
                    offset total-lines expanded-path))
           (t
            (goto-char (point-min))
            (forward-line (1- offset))
            (let ((start (point)))
              (if limit
                  (forward-line limit)
                (goto-char (point-max)))
              (let* ((selected (buffer-substring-no-properties start (point)))
                     (truncation (gptel-pi--truncate-result selected 'head)))
                (gptel-pi--format-read-result
                 origin expanded-path selected offset total-lines truncation)))))))))))

(defun gptel-pi--tool-eval (elisp)
  "Evaluate one ELISP form and return its safely printed result."
  (let* ((full (prin1-to-string (eval (read elisp))))
         (truncation (gptel-pi--truncate-result full 'head))
         (content (plist-get truncation :content)))
    (if (not (plist-get truncation :truncated))
        content
      (let ((link (when (and gptel-pi-session-p (derived-mode-p 'org-mode))
                    (gptel-pi--archive-tool-output
                     "eval" (truncate-string-to-width elisp 60 nil nil "…") full
                     (format "- Elisp :: ~%s~\n" elisp)))))
        (concat
         (if (plist-get truncation :first-line-too-long)
             (format "The printed result starts with a line exceeding %d bytes."
                     gptel-pi-max-tool-bytes)
           content)
         (when link (format "\n\nFull printed result: %s" link)))))))

(defun gptel-pi--tool-edit (path old new)
  "Replace the unique literal occurrence of OLD with NEW in PATH."
  (let ((expanded-path (expand-file-name path)))
    (cond
     ((string-empty-p old)
      "Error: old text must not be empty")
     ((not (file-exists-p expanded-path))
      (format "Error: file not found: %s" expanded-path))
     ((file-directory-p expanded-path)
      (format "Error: path is a directory: %s" expanded-path))
     (t
      (with-temp-buffer
        (insert-file-contents expanded-path)
        (let ((matches 0)
              first-start)
          (goto-char (point-min))
          (while (search-forward old nil t)
            (setq matches (1+ matches))
            (unless first-start
              (setq first-start (match-beginning 0)))
            ;; Advance one character from the beginning to count overlapping
            ;; literal matches as ambiguous too.
            (goto-char (1+ (match-beginning 0))))
          (cond
           ((zerop matches)
            (format "Error: old text not found in %s" expanded-path))
           ((> matches 1)
            (format "Error: old text matches %d times in %s; it must be unique"
                    matches expanded-path))
           (t
            (goto-char first-start)
            (let ((line (line-number-at-pos)))
              (delete-region first-start (+ first-start (length old)))
              (insert new)
              (write-region (point-min) (point-max) expanded-path nil 'silent)
              (format "Edited %s at line %d" expanded-path line))))))))))

(defun gptel-pi--format-bash-result
    (origin command directory stdout stderr status exit-code timeout)
  "Format and possibly archive one completed Bash invocation."
  (let* ((full (concat "STDOUT:\n" stdout
                       (unless (or (string-empty-p stdout)
                                   (string-suffix-p "\n" stdout)) "\n")
                       "\nSTDERR:\n" stderr))
         (truncation (gptel-pi--truncate-result full 'tail))
         (status-text
          (cond
           ((eq status 'signal)
            (format "Bash was terminated by signal %d." exit-code))
           ((and timeout (= exit-code 124))
            (format "Bash timed out after %d seconds (status 124)." timeout))
           (t (format "Bash exited with status %d." exit-code))))
         (link
          (when (and (plist-get truncation :truncated)
                     (buffer-live-p origin)
                     (buffer-local-value 'gptel-pi-session-p origin))
            (with-current-buffer origin
              (gptel-pi--archive-tool-output
               "bash" command full
               (format (concat "- Command :: ~%s~\n- Directory :: =%s=\n"
                               "- Completion :: %s\n")
                       command directory status-text))))))
    (concat status-text
            (unless (and (string-empty-p stdout) (string-empty-p stderr))
              (concat "\n\n"
                      (if (plist-get truncation :truncated)
                          "Relevant tail:\n"
                        "")
                      (plist-get truncation :content)))
            (when link (format "\n\nFull output: %s" link)))))

(defun gptel-pi--tool-bash (callback command &optional timeout)
  "Run COMMAND asynchronously through a pipe and invoke CALLBACK with output."
  (let* ((origin (current-buffer))
         (directory default-directory)
         (name (string-replace "*" "" (buffer-name)))
         (stdout-buffer (generate-new-buffer (format "*%s bash-stdout*" name)))
         (stderr-buffer (generate-new-buffer (format "*%s bash-stderr*" name)))
         (overlay (gptel-pi--find-tool-overlay "bash" (list :command command)))
         (effective-timeout (let ((seconds (or timeout 300)))
                              (when (> seconds 0) seconds)))
         (timeout-argv (when effective-timeout
                         (list "timeout" (format "%ss" effective-timeout))))
         (prefix (when (boundp 'agent-shell-command-prefix)
                   (if (functionp agent-shell-command-prefix)
                       (funcall agent-shell-command-prefix origin)
                     agent-shell-command-prefix)))
         (argv (append prefix timeout-argv
                       (list shell-file-name shell-command-switch command)))
         (process (make-process :name "gptel-pi-bash"
                                :buffer stdout-buffer
                                :stderr stderr-buffer
                                :command argv
                                :connection-type 'pipe
                                :file-handler t
                                :noquery t)))
    (when overlay
      (overlay-put overlay 'gptel-pi-stdout-buffer stdout-buffer)
      (overlay-put overlay 'gptel-pi-stderr-buffer stderr-buffer))
    (process-put process 'callback callback)
    (process-put process 'origin origin)
    (process-put process 'command command)
    (process-put process 'directory directory)
    (process-put process 'timeout effective-timeout)
    (process-put process 'stderr-buffer stderr-buffer)
    (set-process-sentinel
     process
     (lambda (finished _event)
       (when (memq (process-status finished) '(exit signal))
         (let ((stdout-buffer (process-buffer finished))
               (stderr-buffer (process-get finished 'stderr-buffer)))
           (unwind-protect
               (funcall
                (process-get finished 'callback)
                (gptel-pi--format-bash-result
                 (process-get finished 'origin)
                 (process-get finished 'command)
                 (process-get finished 'directory)
                 (if (buffer-live-p stdout-buffer)
                     (with-current-buffer stdout-buffer (buffer-string)) "")
                 (if (buffer-live-p stderr-buffer)
                     (with-current-buffer stderr-buffer (buffer-string)) "")
                 (process-status finished)
                 (process-exit-status finished)
                 (process-get finished 'timeout)))
             (when (buffer-live-p stdout-buffer) (kill-buffer stdout-buffer))
             (when (buffer-live-p stderr-buffer) (kill-buffer stderr-buffer)))))))
    (process-send-eof process)
    process))

(defconst gptel-pi-tools
  (list
   (gptel-make-tool
    :name "eval"
    :function #'gptel-pi--tool-eval
    :description (concat "Evaluate one Elisp form in the live Emacs session and return its printed value. "
                         "Use this only to inspect relevant editor state such as buffers, the kill ring, "
                         "or active modes; use read and bash for ordinary file and project exploration.")
    :args (list '(:name "elisp" :type string :description "The elisp code to eval"))
    :async nil
    :category "pi"
    :confirm nil
    :include t)

   (gptel-make-tool
    :name "bash"
    :function #'gptel-pi--tool-bash
    :description "Execute a bash command in the current working directory. Returns stdout, stderr, and exit status."
    :args (list '(:name "command" :type string :description "The commandline to execute in bash.")
                '(:name "timeout" :type integer :description "Optional timeout in seconds. Defaults to 300; set to -1 or 0 to disable timeout for this command." :optional t))
    :async t
    :category "pi"
    :confirm nil
    :include t)

   (gptel-make-tool
    :name "read"
    :function #'gptel-pi--tool-read
    :description (concat "Read the contents of a text file. Output is limited to complete lines and "
                         "50KB or 2000 lines, whichever is reached first. Use offset/limit for large "
                         "files and continue with the reported next offset.")
    :args (list '(:name "path" :type string :description "Path to the file to read (relative or absolute)")
                '(:name "offset" :type integer :description "Line number to start reading from (1-indexed)" :optional t)
                '(:name "limit" :type integer :description "Maximum number of lines to read" :optional t))
    :async nil
    :category "pi"
    :confirm nil
    :include t)

   (gptel-make-tool
    :name "write"
    :function (lambda (path content)
                (let ((expanded-path (expand-file-name path)))
                  (make-directory (file-name-directory expanded-path) t)
                  (with-temp-file expanded-path
                    (insert content))
                  (format "Wrote %d bytes to %s"
                          (string-bytes content)
                          expanded-path)))
    :description (concat "Write content to a file. Creates the file if it doesn't exist, overwrites if it does. "
                         "Automatically creates parent directories.")
    :args (list '(:name "path" :type string :description "Path to the file to write (relative or absolute)")
                '(:name "content" :type string :description "Content to write to the file"))
    :async nil
    :category "pi"
    :confirm nil
    :include t)

   (gptel-make-tool
    :name "edit"
    :function #'gptel-pi--tool-edit
    :description (concat "Edit a file by replacing exact text. `old` must match exactly "
                         "(including whitespace). Use this for precise, surgical edits.")
    :args (list '(:name "path" :type string :description "Path to the file to edit (relative or absolute)")
                '(:name "old" :type string :description "Exact text to find and replace (must match exactly)")
                '(:name "new" :type string :description "New text to replace the old text with"))
    :async nil
    :category "pi"
    :confirm nil
    :include t)))

(gptel-make-preset
 'gptel-pi
 :description "gptel pi"
 :system gptel-pi-system-prompt
 :tools (mapcar #'gptel-tool-name gptel-pi-tools)
 :prompt-transform-functions (append
                              gptel-prompt-transform-functions
                              '(gptel-pi--restrict-tools))
 :confirm-tool-calls 'auto
 :use-tools t)

(gptel-make-tool
 :name "web_search"
 :function (lambda (cb query)
             (let ((mhook nil))
               (setq mhook
                     (lambda ()
                       (ignore-errors (eww-readable))
                       (let ((s (buffer-string)))
                         (remove-hook 'eww-after-render-hook mhook)
                         (kill-buffer (current-buffer))
                         (funcall cb s))))
               (add-hook 'eww-after-render-hook mhook)
               (eww-open-in-new-buffer t query)))
 :description "Search the web"
 :args (list '(:name "query" :type string :description "The query to search for"))
 :async t
 :category "pi"
 :confirm nil
 :include nil)

(defun gptel-pi--help-prompt ()
  (let ((buffer (seq-find
                 (lambda (b) (not (buffer-local-value 'buffer-read-only b)))
                 (buffer-list (selected-frame)))))
    (concat
     (format "I am working on %s, and I am on line %d. "
             (buffer-file-name buffer)
             (with-current-buffer buffer
               (line-number-at-pos nil t)))
     "Help me understand and point out further areas of investigation. "
     "Be very concise and technical. "
     "Use the provided tools for online and offline investigation.")))

(gptel-make-preset "help"
  :system #'gptel-pi--help-prompt
  :tools '("read" "web_search")
  :use-context nil
  :confirm-tool-calls nil
  :use-tools t
  :include-reasoning nil
  :temperature 0.1
  :stream t)

;;;###autoload
(defun gptel-pi (&optional prefix)
  "DWIM open or switch to gptel-mode buffer through the (gptel) command.

Jump to the last visited *gptel-pi* buffer in the current project root,
creating one if none exists.

With a single C-u prefix, start a new *gptel-pi* buffer in the current
project root.

With a C-u C-u prefix, ask for the *gptel-pi* buffer to switch to."
  (interactive "P")
  (let* ((root (file-name-as-directory (file-truename
                (or (when-let* ((project (project-current)))
                      (project-root project))
                    default-directory))))
         (buffers
          (let (result)
            (dolist (buffer (buffer-list) (nreverse result))
              (with-current-buffer buffer
                (when (and (bound-and-true-p gptel-mode)
                           (bound-and-true-p gptel-pi-session-p)
                           (equal (file-truename default-directory) root))
                  (push buffer result))))))
         (buffer
          (cond
           ((equal prefix '(4)) nil)
           ((equal prefix '(16))
            (when buffers
              (get-buffer
               (completing-read "Switch to gptel-pi buffer: "
                                (mapcar #'buffer-name buffers)
                                nil t nil nil 42))))
           (t (car buffers)))))

    ;; new buffer
    (unless buffer
      (setq buffer
            (let ((name (generate-new-buffer-name
                         (format "*gptel-pi:%s*" (abbreviate-file-name root))))
                  (default-directory root))
              (gptel name)))

      (with-current-buffer buffer
        (gptel-preset (gptel-get-preset 'gptel-pi))
        (gptel-pi--setup-buffer)
        (gptel-pi--insert-project-context root)
        (gptel-pi--initialize-org-session)))

    (pop-to-buffer buffer)))

(provide 'gptel-pi)
