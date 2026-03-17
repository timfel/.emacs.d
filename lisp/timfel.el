;;; timfel.el --- Utility functions from tim     -*- lexical-binding: t -*-

;; Copyright (C) 2025 Tim Felgentreff <timfelgentreff@gmail.com>
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

;; These are just various utility functions that I picked up over the
;; years and that I find useful in my Emacs usage.

(require 'cl-lib)
(require 'subr-x)

(declare-function term-keys/windows-terminal-json "term-keys-windows-terminal")

(defgroup timfel nil
  "Tim's local utility functions and commands."
  :group 'convenience)

(defun timfel/friendly-whitespace ()
  "Add to the buffer-local write-contents-functions remove tabs and delete
trailing whitespace from files"
  (interactive)
  (add-hook 'write-contents-functions
            #'(lambda()
               (save-excursion
                 (untabify (point-min) (point-max))
                 (delete-trailing-whitespace)))))

(defun timfel/ascii-table ()
  "Print the ascii table. Based on a defun by Alex Schroeder <asc@bsiag.com>"
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 0))
    (while (< i 254)
      (setq i (+ i 1))
      (insert (format "%4d %c\n" i i))))
  (goto-char (point-min)))

(defun timfel/kill-all-but-active-buffers ()
  "For each buffer, kill it silently if unmodified, not currently visible,
not read-only, and it's name does not start with '*'"
  (interactive)
  (let ((cnt 0))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (unless (or (buffer-modified-p buf)
                    (get-buffer-window buf 'visible)
                    buffer-read-only
                    (string-match-p "\\*" (buffer-name buf)))
          (kill-buffer buf)
          (setq cnt (1+ cnt)))))
    (message (format "Killed %d buffers." cnt))))

(defun timfel/uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.+\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun timfel/how-many-region (begin end regexp &optional interactive)
  "Print number of non-trivial matches for REGEXP in region.
Non-interactive arguments are Begin End Regexp"
  (interactive "r\nsHow many matches for (regexp): \np")
  (let ((count 0) opoint)
    (save-excursion
      (setq end (or end (point-max)))
      (goto-char (or begin (point)))
      (while (and (< (setq opoint (point)) end)
                  (re-search-forward regexp end t))
        (if (= opoint (point))
            (forward-char 1)
          (setq count (1+ count))))
      (if interactive (message "%d occurrences" count))
      count)))

(defun timfel/infer-indentation-style ()
  (interactive)
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (timfel/how-many-region (point-min) (point-max) "^  "))
        (tab-count (timfel/how-many-region (point-min) (point-max) "^\t")))
    (if (> space-count tab-count)
        (progn
          (setq indent-tabs-mode nil)
          (message "using spaces for indentation")))
    (if (> tab-count space-count)
        (progn
          (setq indent-tabs-mode t
                c-basic-offset 4
                tab-width 4)
          (message "using tabs for indentation")))))

(defun timfel/move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (let ((col (current-column))
        start
        end)
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (forward-char)
    (setq end (point))
    (let ((line-text (delete-and-extract-region start end)))
      (forward-line n)
      (insert line-text)
      ;; restore point to original column in moved line
      (forward-line -1)
      (forward-char col))))

(defun timfel/move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun timfel/move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(defun timfel/update-proxies-from-wpad (&optional force-enable)
  (interactive "P")
  (let* (wpad
         (no-proxy-env "localhost,127.0.0.1,*.oraclecorp.com,oraclecorp.com,*.oraclecloud.com,oraclecloud.com,*.us.oracle.com")
         (no-proxy
          (let ((s (regexp-quote no-proxy-env)))
            (setq s (string-replace "\\*" ".*" s))
            (setq s (string-replace "," "\\|" s))
            s)))
    (if (and (or force-enable (not url-proxy-services))
             (setq wpad (shell-command-to-string "curl -s wpad"))
             (string-match "PROXY\s\\([^; \n\t]+\\)" wpad))
        (progn
          (setq wpad (match-string-no-properties 1 wpad))
          (let ((wpad_with_protocol (if (not (string-match "^https?://" wpad))
                                        (concat "http://" wpad)
                                      wpad)))
            (setenv "http_proxy" wpad_with_protocol)
            (setenv "https_proxy" wpad_with_protocol)
            (setenv "HTTP_PROXY" wpad_with_protocol)
            (setenv "HTTPS_PROXY" wpad_with_protocol)
            (setenv "no_proxy" no-proxy-env)
            (setenv "NO_PROXY" no-proxy-env)
            )
          (let* ((m (string-match "\\([^:]+\\):\\([0-9]+\\)$" wpad))
                 (host (if m (match-string-no-properties 1 wpad) wpad))
                 (port (if m (match-string-no-properties 2 wpad) 443))
                 (http (if m wpad (concat wpad ":80")))
                 (https (if m wpad (concat wpad ":443"))))
            (setq
             url-proxy-services (list
                                 (cons "http" http)
                                 (cons "https" https)
                                 (cons "no_proxy" no-proxy)))
            (setenv "GRADLE_OPTS" (format "-Dhttp.proxyHost=%s -Dhttp.proxyPort=%s -Dhttps.proxyHost=%s -Dhttps.proxyPort=%s" host port host port))
            (setenv "MAVEN_OPTS" (format "-Dhttp.proxyHost=%s -Dhttp.proxyPort=%s -Dhttps.proxyHost=%s -Dhttps.proxyPort=%s" host port host port)))
          (message "Proxies set: %s" url-proxy-services))
      (progn
        (setq url-proxy-services nil)
        (setenv "http_proxy" nil)
        (setenv "https_proxy" nil)
        (setenv "no_proxy" nil)
        (setenv "HTTP_PROXY" nil)
        (setenv "HTTPS_PROXY" nil)
        (setenv "NO_PROXY" nil)
        (setenv "MAVEN_OPTS" nil)
        (setenv "GRADLE_OPTS" nil)
        (message "Proxies disabled"))))
  (cdar url-proxy-services))

(defun timfel/org-narrow-to-headings (regexp depth)
  "Narrow the current org buffer to the headings at given depth matching
the given regexp"
  (interactive
   (list (read-string
	  "Regex for headings to keep (lowercase): "
	  "\\(python\\|leads\\).*call")
	 (read-number "Heading depth: " 4)))
  (while (< (point) (point-max))
    (let ((heading-title (car (cddddr (org-heading-components))))
          (heading-depth (car (org-heading-components))))
      (if (= heading-depth depth)
          (if (string-match (downcase regexp) (downcase heading-title))
              (org-next-visible-heading 1)
            (org-cut-subtree))
        (org-next-visible-heading 1)))))

(defun timfel/determine-recent-project-root ()
  "Examine the recent buffers to find the most recent one that was under a
VC root"
  (if-let* ((buffers-root-dirs (seq-keep (lambda (buffer)
                                           (if-let* ((path (buffer-file-name buffer))
                                                     (backend (vc-backend path))
                                                     (dir (file-name-parent-directory path))
                                                     (root (vc-call-backend backend 'root dir)))
                                               root))
                                         (buffer-list)))
            (vc-root (seq-first buffers-root-dirs))
            (root (expand-file-name vc-root)))
      root))

(defun timfel/update-windows-terminal-settings-json ()
    "Find the Windows-side Windows terminal settings.json which is under
%LOCALAPPDATA%\\Packages\\Microsoft.WindowsTerminal_8wekyb3d8bbwe\\
LocalState\\settings.json
(finding that PATH on the Windows side if we are running in WSL), remove
the actions json key and value, and insert the actions json key and
value that is generated by term-keys/windows-terminal-json"
    (interactive)
    (require 'term-keys-windows-terminal)
    (with-temp-buffer
      (insert (term-keys/windows-terminal-json))
      (let* ((actions-json (buffer-string))
             (actions-obj (if (fboundp 'json-parse-string)
                              (json-parse-string actions-json :object-type 'hash-table :array-type 'list)
                            (with-temp-buffer
                              (insert actions-json)
                              (goto-char (point-min))
                              (json-read))))
             (actions (cond
                       ((hash-table-p actions-obj)
                        (or (gethash "actions" actions-obj) actions-obj))
                       ((listp actions-obj) actions-obj)
                       (t nil))))
        (cl-labels
            ((timfel/wsl-p ()
               (and (eq system-type 'gnu/linux)
                    (ignore-errors
                      (with-temp-buffer
                        (insert-file-contents "/proc/version")
                        (goto-char (point-min))
                        (re-search-forward "Microsoft" nil t)))))
             (timfel/trim-crlf (s)
               (when s
                 (replace-regexp-in-string "[\r\n]+\\'" "" s)))
             (timfel/shell-trim (cmd)
               (timfel/trim-crlf (shell-command-to-string cmd)))
             (timfel/wsl-localappdata ()
               (let* ((raw (timfel/shell-trim "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe -NoProfile -Command \"\\$env:LOCALAPPDATA\""))
                      (u (and raw (timfel/shell-trim (format "wslpath -u '%s'" raw)))))
                 (and (stringp u) (not (string-empty-p u)) u)))
             (timfel/windows-localappdata ()
               (cond
                ((eq system-type 'windows-nt)
                 (getenv "LOCALAPPDATA"))
                ((timfel/wsl-p)
                 (timfel/wsl-localappdata))
                (t nil)))
             (timfel/windows-terminal-settings-path ()
               (when-let* ((lad (timfel/windows-localappdata)))
                 (let ((p (expand-file-name
                           (concat (file-name-as-directory lad)
                                   "Packages/Microsoft.WindowsTerminal_8wekyb3d8bbwe/LocalState/settings.json"))))
                   p))))
          (when actions
            (when-let* ((settings-path (timfel/windows-terminal-settings-path)))
              (when (file-exists-p settings-path)
                (let* ((settings-obj
                        (with-temp-buffer
                          (insert-file-contents settings-path)
                          (goto-char (point-min))
                          (json-parse-buffer :object-type 'hash-table :array-type 'list))))
                  (when (hash-table-p settings-obj)
                    (puthash "actions" actions settings-obj)
                    (let ((json-encoding-pretty-print t))
                      (with-temp-file settings-path
                        (insert (json-encode settings-obj)))))))))))))

(defun timfel/fill-paragraph-sentence-wise ()
  (interactive)
  (save-excursion
    (let ((fill-column 12345678))
      (fill-paragraph)
      (let ((end (save-excursion
		   (forward-paragraph 1)
		   (backward-sentence)
		   (point-marker))))
	(beginning-of-line)
	(while (progn (forward-sentence)
		      (<= (point) (marker-position end)))
	  (just-one-space)
	  (delete-char -1)
	  (newline t))))))

(dolist (sym (apropos-internal "^timfel/" #'fboundp))
  (let* ((old-name (symbol-name sym))
         (new-name (intern (string-remove-prefix "timfel/" old-name))))
    (if (fboundp new-name)
        (warn "Function %s already exists" new-name)
      (defalias new-name sym))))

(defcustom timfel/cloud-storage
  (cl-case system-type
    (windows-nt "//drive.timfelgentreff.de@SSL/DavWWWRoot/remote.php/dav/files/timfelgentreff/")
    (android (file-name-directory
              (file-name-directory
               (or
                (nth 0 (file-expand-wildcards "/content/storage/org.nextcloud.documents/*/SyncFolder"))
                (expand-file-name "~")))))
    (t (if (file-exists-p (expand-file-name "~/CloudDrive"))
           (expand-file-name "~/CloudDrive")
         (expand-file-name "~/NextCloud"))))
  "Where the cloud storage lives, where I sync screenshots and notes"
  :type 'directory)

(defcustom timfel/gist-location
  (cl-case system-type
    (windows-nt "D:/gists")
    (t (expand-file-name "~/dev/gists/")))
  "The location of non-public extra lisp files"
  :type 'directory)


(provide 'timfel)
