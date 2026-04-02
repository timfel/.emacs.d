;;; timfel-markdown-overlays-extensions.el --- Extra markdown overlays -*- lexical-binding: t -*-

;;; Commentary:

;; Local extensions for `markdown-overlays', including Jira-aware links.

;;; Code:

(require 'map)
(require 'browse-url)
(require 'seq)
(require 'subr-x)

(declare-function jira-detail-show-issue "jira-detail" (key))
(declare-function jira-api-call "jira-api" (method endpoint &rest args))
(declare-function jira-detail--issue "jira-detail" (key data))
(defvar browse-url-button-regexp)

(defconst timfel/markdown-overlays-jira-issue-regexp
  "\\bGR-[0-9]+\\b"
  "Regexp matching Jira issue keys handled by the local Jira integration.")

(defun timfel/markdown-overlays--jira-issue-key (text)
  "Return a Jira issue key extracted from TEXT, or nil.

Plain issue keys such as \"GR-123\" are accepted directly.  URLs are only
treated as Jira links when they contain both \"jira\" and a Jira issue key."
  (when (stringp text)
    (let ((case-fold-search t))
      (cond
       ((and (string-match-p "\\`https?://" text)
             (string-match-p "jira" text)
             (string-match timfel/markdown-overlays-jira-issue-regexp text))
        (match-string 0 text))
       ((and (string-match-p (concat "\\`" timfel/markdown-overlays-jira-issue-regexp "\\'") text)
             text)
        text)))))

(defun timfel/markdown-overlays-open-jira-issue (text)
  "Open the Jira issue identified by TEXT with `jira-detail-show-issue'."
  (interactive "sJira issue or URL: ")
  (let ((issue-key (timfel/markdown-overlays--jira-issue-key text)))
    (unless issue-key
      (user-error "No Jira issue key found in %S" text))
    (unless (require 'jira-detail nil t)
      (user-error "jira-detail.el is not available"))
    (cond
     ((fboundp 'jira-detail-show-issue)
      (jira-detail-show-issue issue-key))
     ((and (fboundp 'jira-api-call)
           (fboundp 'jira-detail--issue))
      (jira-api-call
       "GET" (concat "issue/" issue-key)
       :callback (lambda (data _response)
                   (jira-detail--issue issue-key data))))
     (t
      (user-error "jira-detail is loaded, but no issue-opening entrypoint is available")))))

(defun timfel/markdown-overlays-ret-action-at-point ()
  "Return the command bound to `RET' by an overlay/text keymap at point."
  (let ((maps (delq nil
                    (list (get-char-property (point) 'keymap)
                          (get-text-property (point) 'keymap)
                          (and (> (point) (point-min))
                               (get-char-property (1- (point)) 'keymap))
                          (and (> (point) (point-min))
                               (get-text-property (1- (point)) 'keymap))))))
    (seq-some (lambda (map)
                (when (keymapp map)
                  (let ((binding (lookup-key map (kbd "RET"))))
                    (and binding
                         (not (integerp binding))
                         (commandp binding)
                         binding))))
              maps)))

(defun timfel/agent-shell-return-dwim ()
  "Activate an actionable item at point, otherwise insert a newline."
  (interactive)
  (cond
   ((button-at (point))
    (push-button (point)))
   ((and (> (point) (point-min))
         (button-at (1- (point))))
    (push-button (1- (point))))
   ((if-let ((action (timfel/markdown-overlays-ret-action-at-point)))
        (progn
          (call-interactively action)
          t)
      nil))
   (t
    (call-interactively #'newline))))

(defun timfel/markdown-overlays--open-link-around (original-fn url)
  "Open Jira-like URL via Jira package, else delegate to ORIGINAL-FN."
  (if-let ((issue-key (timfel/markdown-overlays--jira-issue-key url)))
      (timfel/markdown-overlays-open-jira-issue issue-key)
    (funcall original-fn url)))

(defun timfel/markdown-overlays--inside-ranges-p (start end ranges)
  "Return non-nil when START END overlap any range from RANGES."
  (seq-some (lambda (range)
              (< (max start (car range))
                 (min end (cdr range))))
            ranges))

(defun timfel/markdown-overlays--clickable-overlay-p (start end)
  "Return non-nil when a clickable overlay already covers START END."
  (seq-some (lambda (overlay)
              (and (overlay-get overlay 'keymap)
                   (<= (overlay-start overlay) start)
                   (>= (overlay-end overlay) end)))
            (overlays-in start end)))

(defun timfel/markdown-overlays--hidden-p (pos)
  "Return non-nil when POS is currently hidden by another overlay."
  (get-char-property pos 'invisible))

(defun timfel/markdown-overlays--jira-keymap (issue-key)
  "Return a keymap that opens ISSUE-KEY in Jira."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1]
                (lambda ()
                  (interactive)
                  (timfel/markdown-overlays-open-jira-issue issue-key)))
    (define-key map (kbd "RET")
                (lambda ()
                  (interactive)
                  (timfel/markdown-overlays-open-jira-issue issue-key)))
    map))

(defun timfel/markdown-overlays--jira-face-at (pos)
  "Return a face list for a Jira link at POS that preserves existing styling."
  (delete-dups
   (append (ensure-list (get-char-property pos 'face))
           '(link))))

(defun timfel/markdown-overlays--fontify-jira-link (start end issue-key)
  "Attach Jira link affordances for ISSUE-KEY covering START END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'category 'markdown-overlays)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'face (timfel/markdown-overlays--jira-face-at start))
    (overlay-put overlay 'mouse-face 'highlight)
    (overlay-put overlay 'help-echo (format "Open Jira issue %s" issue-key))
    (overlay-put overlay 'follow-link t)
    (overlay-put overlay 'priority 1001)
    (overlay-put overlay 'keymap (timfel/markdown-overlays--jira-keymap issue-key))
    (overlay-put overlay 'timfel-jira-issue issue-key)
    overlay))

(defun timfel/markdown-overlays--source-block-ranges (result)
  "Return source block ranges from markdown-overlays RESULT."
  (mapcar (lambda (block)
            (cons (car (map-elt block 'start))
                  (cdr (map-elt block 'end))))
          (alist-get 'source-blocks result)))

(defun timfel/markdown-overlays--put-jira-overlays (result)
  "Add Jira issue overlays after `markdown-overlays-put' returns RESULT."
  (let ((source-block-ranges (timfel/markdown-overlays--source-block-ranges result))
        url-ranges)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward browse-url-button-regexp nil t)
        (let* ((start (match-beginning 0))
               (end (match-end 0))
               (text (match-string-no-properties 0))
               (issue-key (timfel/markdown-overlays--jira-issue-key text)))
          (when issue-key
            (push (cons start end) url-ranges)
            (unless (or (timfel/markdown-overlays--hidden-p start)
                        (timfel/markdown-overlays--inside-ranges-p start end source-block-ranges)
                        (timfel/markdown-overlays--clickable-overlay-p start end))
              (timfel/markdown-overlays--fontify-jira-link start end issue-key))))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward timfel/markdown-overlays-jira-issue-regexp nil t)
        (let ((start (match-beginning 0))
              (end (match-end 0))
              (issue-key (match-string-no-properties 0)))
          (unless (or (timfel/markdown-overlays--inside-ranges-p start end source-block-ranges)
                      (timfel/markdown-overlays--hidden-p start)
                      (timfel/markdown-overlays--inside-ranges-p start end url-ranges)
                      (timfel/markdown-overlays--clickable-overlay-p start end))
            (timfel/markdown-overlays--fontify-jira-link start end issue-key)))))
    result))

(with-eval-after-load 'markdown-overlays
  (unless (advice-member-p #'timfel/markdown-overlays--open-link-around
                           'markdown-overlays--open-link)
    (advice-add 'markdown-overlays--open-link :around
                #'timfel/markdown-overlays--open-link-around))
  (unless (advice-member-p #'timfel/markdown-overlays--put-jira-overlays
                           'markdown-overlays-put)
    (advice-add 'markdown-overlays-put :filter-return
                #'timfel/markdown-overlays--put-jira-overlays)))

(provide 'timfel-markdown-overlays-extensions)

;;; timfel-markdown-overlays-extensions.el ends here
