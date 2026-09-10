;;; -*- lexical-binding: t -*-
(require 'use-package)

(use-package android
  :if (eq system-type 'android)
  :no-require t
  :defines (timfel/cloud-storage android-intercept-control-space)
  :functions (org-capture-kill org-capture-finalize org-capture with-auto-default
              timfel/android-mail-addresses timfel/android-send-it
              timfel/android-start-termux
              mail-fetch-field mail-strip-quoted-names
              mail-sendmail-undelimit-header expand-mail-aliases)
  :after (timfel)
  :custom
  (tool-bar-mode 1)
  (menu-bar-mode 1)
  (modifier-bar-mode 1)
  (tool-bar-position 'top)
  (tool-bar-always-show-default t)
  (tool-bar-button-margin 32)
  (touch-screen-display-keyboard t)
  :config

  ;; `mailclient-send-it' uses a `mailto:' URL, but Android Emacs opens
  ;; those with ACTION_VIEW.  Use ACTION_SEND with the standard email
  ;; extras instead.  This keeps the implementation in Lisp and lets the
  ;; configured mail app fill in the recipients, subject, and body.
  (defun timfel/android-mail-addresses (field)
    (when-let* ((value (mail-fetch-field field nil t)))
      (split-string (mail-strip-quoted-names value)
                    "[ \t\n]*,[ \t\n]*" t)))

  (defun timfel/android-send-it ()
    "Pass the current message to an Android mail client via `am'."
    (require 'mail-utils)
    (require 'sendmail)
    (let ((mailbuf (current-buffer)) to cc bcc subject body)
      (with-temp-buffer
        (insert-buffer-substring mailbuf)
        (mail-sendmail-undelimit-header)
        (let ((headers-end (point-marker)))
          (when mail-aliases
            (expand-mail-aliases (point-min) headers-end))
          (save-restriction
            (narrow-to-region (point-min) headers-end)
            (setq to (timfel/android-mail-addresses "To")
                  cc (timfel/android-mail-addresses "Cc")
                  bcc (timfel/android-mail-addresses "Bcc")
                  subject (mail-fetch-field "Subject" nil t)))
          (widen)
          (goto-char headers-end)
          (when (looking-at "\n")
            (forward-char 1))
          (setq body (buffer-substring-no-properties (point) (point-max)))))
      (let* ((am (or (executable-find "am")
                     (and (file-executable-p "/system/bin/am")
                          "/system/bin/am")))
             (output-buffer "*Android mail intent*")
             (args (append
                    '("start" "-a" "android.intent.action.SEND"
                      "-t" "text/plain")
                    (when to
                      (list "--esa" "android.intent.extra.EMAIL"
                            (string-join to ",")))
                    (when cc
                      (list "--esa" "android.intent.extra.CC"
                            (string-join cc ",")))
                    (when bcc
                      (list "--esa" "android.intent.extra.BCC"
                            (string-join bcc ",")))
                    (when subject
                      (list "--es" "android.intent.extra.SUBJECT" subject))
                    (list "--es" "android.intent.extra.TEXT" (or body "")))))
        (unless am
          (user-error "Cannot find Android am command"))
        (let ((status (let ((coding-system-for-write 'utf-8))
                        (apply #'call-process am nil output-buffer nil args))))
          (unless (and (integerp status) (zerop status))
            (error "Android mail intent failed (%s): %s"
                   status
                   (with-current-buffer output-buffer
                     (string-trim (buffer-string)))))))))
  (setq send-mail-function #'timfel/android-send-it)

  (defun timfel/android-start-termux ()
    "Switch to the Termux activity."
    (let* ((am (or (executable-find "am")
                   (and (file-executable-p "/system/bin/am")
                        "/system/bin/am")))
           (output-buffer "*Android Termux launch*"))
      (unless am
        (user-error "Cannot find Android am command"))
      (with-current-buffer (get-buffer-create output-buffer)
        (erase-buffer))
      (let ((status (call-process am nil output-buffer nil
                                  "start" "-n"
                                  "com.termux/.app.TermuxActivity")))
        (unless (and (integerp status) (zerop status))
          (error "Could not start Termux (%s): %s"
                 status
                 (with-current-buffer output-buffer
                   (string-trim (buffer-string))))))))

  ;; AltGr on the no-name phone keyboard I use sends KEYCODE_*, and there is no
  ;; Meta key, so let's make it usable
  (define-key key-translation-map (kbd "<KEYCODE_SPACE>") (kbd "ESC"))
  (define-key key-translation-map (kbd "<KEYCODE_S>") (kbd "ß"))
  (define-key key-translation-map (kbd "<KEYCODE_Q>") (kbd "ä"))
  (define-key key-translation-map (kbd "<KEYCODE_P>") (kbd "ö"))
  (define-key key-translation-map (kbd "<KEYCODE_Y>") (kbd "ü"))
  (define-key key-translation-map (kbd "S-<KEYCODE_Q>") (kbd "Ä"))
  (define-key key-translation-map (kbd "S-<KEYCODE_P>") (kbd "Ö"))
  (define-key key-translation-map (kbd "S-<KEYCODE_Y>") (kbd "Ü"))

  (setq android-intercept-control-space nil)
  (global-visual-line-mode t)
  (setq visual-line-fringe-indicators
        '(left-curly-arrow right-curly-arrow))
  ;; we do not have permissions above our own and some shared folders in
  ;; emacs on android
  (setq locate-dominating-stop-dir-regexp
        (concat locate-dominating-stop-dir-regexp
                "\\|\\`/data/data/org.gnu.emacs/\\'"
                "\\|\\`/data/data/com.termux/\\'"
                "\\|\\`/content/storage/\\'"))
  ;; fullscreen
  ;; (set-frame-parameter nil 'fullscreen 'fullboth)
  ;; useful menus
  (setq tool-bar-map '(keymap))
  (define-key-after tool-bar-map [separator-0] menu-bar-separator)
  (tool-bar-add-item "save" 'save-buffer 'save-buffer)
  (tool-bar-add-item "close" (lambda ()
                               (interactive)
                               (kill-buffer nil))
                     'kb)
  (define-key-after tool-bar-map [separator-1] menu-bar-separator)
  (tool-bar-add-item "back-arrow" 'undo 'undo)
  (define-key-after tool-bar-map [separator-2] menu-bar-separator)
  (tool-bar-add-item "home" 'delete-other-windows 'delete-other-windows)
  (tool-bar-add-item "refresh" 'revert-buffer 'ost)
  (tool-bar-add-item "new" (lambda ()
                             (interactive)
                             (if (derived-mode-p 'org-mode)
                                 (progn
                                   (org-capture nil "n")
                                   (delete-other-windows)
                                   (visual-line-mode t)
                                   (text-scale-set +2))
                               (find-file (expand-file-name
                                           "SyncFolder/notes.org"
                                           timfel/cloud-storage))
                               (goto-char (point-max))))
                     'oc)
  (tool-bar-add-item "info" 'gptel 'gptel)
  (tool-bar-add-item "zoom-in" (lambda () (interactive) (global-text-scale-adjust +4)) 'zoomin)
  (tool-bar-add-item "zoom-out" (lambda () (interactive) (global-text-scale-adjust -4)) 'zoomout)
  :bind
  (("<volume-down>" . (lambda ()
                        (interactive)
                        (let ((modes (cons major-mode local-minor-modes)))
                          (pcase modes
                            ((pred (memq 'gptel-mode))
                             (gptel-send))

                            ((pred (memq 'org-capture-mode)) ;; save org note
                             (org-capture-finalize)
                             (find-file (expand-file-name "SyncFolder/notes.org" timfel/cloud-storage))
                             (goto-char (point-max)))

                            ((pred (memq 'org-mode))
                             (progn
                                   (org-capture nil "n")
                                   (delete-other-windows)
                                   (visual-line-mode t)
                                   (text-scale-set +2)))

                            ((pred (memq 'vc-dir-mode))
                             (if (and (not (executable-find "git"))
                                      (file-equal-p default-directory "~/.emacs.d"))
                                 (let ((tgt (car (file-expand-wildcards "/content/storage/com.termux.documents/*/.emacs.d"))))
                                   (require 'em-unix)
                                   (declare-function eshell/cp "em-unix")
                                   (eshell/cp "-f" "-r" tgt "~/"))
                               (vc-pull)))

                            ((pred (memq 'org-social-ui-mode)) ;; org-social timeline
                             (with-auto-default (org-social-new-post))
                             (delete-other-windows)
                             (visual-line-mode t)
                             (text-scale-set +2))

                            ((pred (memq 'org-social-mode)) ;; send org-social post
                             (save-buffer)
                             (kill-buffer)
                             (org-social-timeline))

                            (_ nil)))))
   ("<volume-up>" . (lambda ()
                      (interactive)
                      (let ((modes (cons major-mode local-minor-modes)))
                        (pcase modes
                          ((pred (memq 'org-capture-mode)) ;; abort org note and show prev notes
                           (org-capture-kill)
                           (find-file (expand-file-name "SyncFolder/notes.org" timfel/cloud-storage))
                           (goto-char (point-max)))

                          ((pred (memq 'vc-dir-mode))
                           (if (and (not (executable-find "git"))
                                    (file-equal-p default-directory "~/.emacs.d"))
                               (let ((tgt (car (file-expand-wildcards "/content/storage/com.termux.documents/*/"))))
                                 (unless tgt
                                   (user-error "Cannot find the Termux shared folder"))
                                 (require 'em-unix)
                                 (declare-function eshell/rm "em-unix")
                                 (declare-function eshell/cp "em-unix")
                                 (message "Pushing to Termux (1/2): removing old Git metadata…")
                                 (redisplay)
                                 (eshell/rm "-r" "-f" "~/.emacs.d/.git")
                                 (message "Pushing to Termux (2/2): copying configuration…")
                                 (redisplay)
                                 (eshell/cp "-f" "-r" "~/.emacs.d" tgt)
                                 (message "Push complete; switching to Termux…")
                                 (redisplay)
                                 (timfel/android-start-termux))
                             (vc-push)))

                          ((pred (memq 'org-social-mode)) ;; cancel org social post
                           (kill-buffer))

                          (_ nil)))))))

(use-package zone
  :commands (zone-when-idle)
  :custom
  (zone-all-frames t)           ; EMACS-31
  (zone-all-windows-in-frame t) ; EMACS-31
  :config
  (zone-when-idle 300))

(use-package isearch
  :bind (("C-S-s" . isearch-forward-thing-at-point)
         :map isearch-mode-map
         ([backspace] . isearch-edit-string)))

(use-package hippie-exp
  :bind (([remap dabbrev-expand] . hippie-expand))
  :config
  (defun timfel/try-complete-abbrev (_old)
    (if (expand-abbrev) t nil))
  :custom
  (hippie-expand-try-functions-list '(timfel/try-complete-abbrev
                                      try-complete-file-name
                                      try-expand-dabbrev)))

(use-package org
  :after timfel
  :commands org-mode
  :functions org-agenda-files
  :mode (("\\.org$" . org-mode))
  :init
  :bind (("C-c c" . org-capture)
         ("C-c m" . (lambda () (interactive) (org-capture nil "m")))
         ("C-c t" . (lambda () (interactive) (org-capture nil "t")))
         ("C-c a" . (lambda () (interactive)
                      (ignore-errors
                        (require 'jira)
                        (require 'emacs-ci))
                      (call-interactively #'org-agenda)))
         ("C-c l" . org-store-link)
         ("C-c b" . (lambda ()
                      (interactive)
                      (org-store-link nil)
                      (when-let* ((link (caar org-stored-links))
                                  (files (org-agenda-files))
                                  (re (regexp-quote link))
                                  (hit 0))
                        (seq-find (lambda (f)
                                    (ignore-errors
                                      (with-current-buffer (find-file-noselect f)
                                        (save-excursion
                                          (goto-char (point-min))
                                          (when (re-search-forward re nil t)
                                            (setq hit (cons f (match-beginning 0)))
                                            t)))))
                                  files)
                        (unless (numberp hit)
                          (find-file (car hit))
                          (goto-char (cdr hit))
                          (recenter)))))
         :map org-mode-map
         ("C-c g" . org-dblock-update)
         ("C-c d" . org-dynamic-block-insert-dblock)
         ("C-c <right>" . org-shiftright)
         ("C-c <left>" . org-shiftleft)
         ("C-c M-RET" . org-insert-subheading))
  :config
  (defun timfel/org-buffers (&rest _)
    (ibuffer t "*Org Buffers*" '((used-mode . org-mode))))
  :custom
  (org-refile-use-outline-path 'file)
  (org-archive-mark-done t)
  (org-image-actual-width (list 600))
  (org-log-done 'time)
  (org-export-backends '(ascii md html latex))
  (org-hide-emphasis-markers t)
  (org-link-elisp-skip-confirm-regexp
   (concat
    "^(jira-detail-show-issue \"[^\"]+\")$"
    "\\|"
    "^(timfel/ci-dashboard-show-pr \"[^\"]+\" \"[^\"]+\" [0-9]+)$"
    "\\|"
    "^(browse-url-default-browser \"slack:[^\"]+\")$"
    "\\|"
    "^(jira-issues)$"
    "\\|"
    "^(ci-dashboard)$"
    "\\|"
    "^(let ((default-directory \"[^\"]+\")) (call-interactively #'agent-shell))$"))
  (org-return-follows-link t)
  (org-file-apps '((auto-mode . emacs)
                   ("\\.mm\\'" . default)
                   ("\\.x?html?\\'" . default)
                   ("\\.pdf\\'" . "evince %s")))
  (org-replace-disputed-keys t)
  (org-deadline-warning-days 7)
  (org-agenda-span 'fortnight)
  (org-agenda-start-on-weekday nil)
  (org-agenda-skip-scheduled-if-deadline-is-shown t)
  (org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
  (org-agenda-todo-ignore-deadlines 'all)
  (org-agenda-todo-ignore-scheduled 'all)
  (org-agenda-sorting-strategy '((agenda deadline-up priority-down)
                                (todo priority-down category-keep)
                                (tags priority-down category-keep)
                                (search category-keep)))
  (org-insert-mode-line-in-empty-file t)
  (org-refile-targets `(((,(expand-file-name "SyncFolder/todo.org" timfel/cloud-storage))
                         . (:regexp . ,(rx (= 4 num) "-" (= 2 num) "-" (= 2 num) (+ space) (+ word))))))
  (org-adapt-indentation nil "do not shift lower items")
  (org-hide-leading-stars t "i like this more")
  (org-priority-highest ?A)
  (org-priority-lowest ?C)
  (org-priority-default ?A)
  (org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                       (?B . (:foreground "LightSteelBlue"))
                       (?C . (:foreground "OliveDrab"))))
  (org-agenda-window-setup 'current-window)
  (org-agenda-skip-deadline-if-done t)
  (org-todo-keywords '((sequence "TODO(t)" "IN PROGRESS(i@/!)" "BLOCKED(b@)" "|" "DONE(d!)" "WONT DO(w@/!)")))
  (org-todo-keyword-faces
   '(("TODO" . (:foreground "Red" :weight bold))
     ("IN PROGRESS" . (:foreground "Cyan" :weight bold))
     ("BLOCKED" . (:foreground "Magenta" :weight bold))
     ("DONE" . (:foreground "LimeGreen" :weight bold))
     ("WONT DO" . (:foreground "LimeGreen" :weight bold))))
  (org-agenda-custom-commands
   '(("a" "Daily agenda and all TODOs"
      ((tags-todo "-DONE"
                  ((org-agenda-overriding-header "Active")
                   (org-agenda-skip-function
                    '(and
                      (org-agenda-skip-entry-if 'notregexp "Daily work items")
                      (org-agenda-skip-entry-if 'todo '("TODO" "DONE" "WONT DO"))))))
       (agenda ""
               ((org-agenda-span 7)
                (org-agenda-overriding-header "Agenda")
                (org-agenda-skip-function
                 '(or (org-agenda-skip-entry-if 'regexp "Daily work items")
                      (org-agenda-skip-entry-if 'todo '("IN PROGRESS" "BLOCKED" "DONE" "WONT DO"))
                      (org-agenda-skip-entry-if 'nottimestamp)))))
       (alltodo ""
                ((org-agenda-overriding-header "Unscheduled")
                 (org-agenda-skip-function
                  '(or (org-agenda-skip-entry-if 'nottodo '("TODO"))
                       (org-agenda-skip-entry-if 'scheduled 'deadline)))))
       (agenda ""
               ((org-agenda-start-day "-7d")
                (org-agenda-span 8)
                (org-agenda-overriding-header "Recently done")
                (org-agenda-show-log 'only)
                (org-agenda-use-time-grid nil)
                (org-agenda-log-mode-items '(closed))
                (org-agenda-skip-function
                 '(or (org-agenda-skip-entry-if 'nottodo '("DONE" "WONT DO"))))))))
     ("b" "All org buffers" timfel/org-buffers)))
  (org-clock-idle-time 15)
  (org-agenda-files (list (expand-file-name "SyncFolder/todo.org" timfel/cloud-storage)
                          (expand-file-name "SyncFolder/notes.org" timfel/cloud-storage)))
  (org-capture-templates
   `(("t" "todo"
      entry (file+olp+datetree ,(expand-file-name "SyncFolder/todo.org" timfel/cloud-storage))
      ,(string-join '("* TODO %i%?"
                      "DEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+7d\"))"
                      ":Created: %T"
                      "  %a")
                    "\n")
      :empty-lines 1
      :tree-type month)
     ("n" "note"
      entry (file+olp+datetree ,(expand-file-name "SyncFolder/notes.org" timfel/cloud-storage))
      "* %?\nEntered on %U\n")
     ("m" "meeting"
      entry (file+olp+datetree ,(expand-file-name "SyncFolder/notes.org" timfel/cloud-storage))
      ,(string-join '("* %? :meeting:"
                      ":Created: %T"
                      "** Notes"
                      "** Action Items"
                     "*** TODO [#A] ")
                    "\n")
      :clock-in t
      :clock-resume t
      :empty-lines 0))))

(use-package org-tempo
  :after org)

(use-package imenu
  :custom
  (imenu-auto-rescan t)
  (imenu-flatten 'group)
  :bind (("C-." . imenu)))

(use-package eldoc
  :custom
  (eldoc-documentation-strategy #'eldoc-documentation-compose)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-help-at-pt t) ;; EMACS-31
  :functions (eldoc-display-in-buffer-at-point)
  :bind (([remap display-local-help] . timfel/local-help-or-doc))
  :config
  (defun timfel/local-help-or-doc ()
    (interactive)
    (unless (display-local-help t)
      (call-interactively #'eldoc-print-current-symbol-info)))
  (setq eldoc-display-functions '(eldoc-display-in-echo-area eldoc-display-in-buffer))
  ;; (remove-hook 'eldoc-display-functions #'eldoc-display-in-buffer)
  (remove-hook 'eldoc-display-functions #'eldoc-display-in-buffer-at-point)
  (add-hook 'eldoc-display-functions #'eldoc-display-in-echo-area))

(use-package icomplete
  :functions (icomplete-fido-delete-char
              icomplete-fido-ret
              icomplete-fido-backward-updir
              icomplete-forward-completions
              icomplete-backward-completions
              icomplete-fido-exit
              icomplete-minibuffer-setup
              icomplete-ret)
  :bind (:map icomplete-minibuffer-map
              ("RET" . #'icomplete-fido-ret)
              ("M-j" . #'icomplete-fido-exit)
              ("C-<return>" . #'icomplete-ret)
              ("TAB" . #'minibuffer-complete)
              ("DEL" . #'icomplete-fido-backward-updir)
              ("C-d" . #'icomplete-fido-delete-char)
              ("<right>" . #'icomplete-forward-completions)
              ("<left>" . #'icomplete-backward-completions)
              ("<volume-down>" . #'icomplete-forward-completions)
              ("<volume-up>" . #'icomplete-backward-completions)
              ("C-c C-d" . (lambda ()
                             (interactive)
                             (message "category=%S"
                                      (completion-metadata-get
                                       (completion-metadata (minibuffer-contents)
                                                            minibuffer-completion-table
                                                            minibuffer-completion-predicate)
                                       'category)))))
  :custom
  (icomplete-in-buffer t)
  (icomplete-hide-common-prefix t)
  (icomplete-tidy-shadowed-file-names t)
  (icomplete-show-matches-on-no-input t)
  (completion-flex-nospace nil)
  (icomplete-vertical-in-buffer-adjust-list t) ;; EMACS-31
  (icomplete-vertical-render-prefix-indicator t) ;; EMACS-31
  :config
  (add-to-list 'completion-ignored-extensions
               ".lock")
  (setq completion-ignore-case t
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t)
  (icomplete-mode t)
  (icomplete-vertical-mode t)
  ;; If I were to use normal ido-mode, disable icomplete in the minibuffer
  ;; (remove-hook 'minibuffer-setup-hook #'icomplete-minibuffer-setup)
  ;; i like completion to be local
  (advice-add 'completion-at-point :after (lambda (&rest _args) (unless (minibuffer-window-active-p (get-buffer-window)) (minibuffer-hide-completions))))
  (setq completion-category-overrides nil)
  (mapc (lambda (override) (add-to-list 'completion-category-overrides override))
        '((project-file (styles substring))
          (imenu (styles flex))
          (buffer (styles initials flex basic))
          (command (styles partial-completion))
          (file (styles flex partial-completion)))))

(use-package completion-preview
  :disabled
  :hook (prog-mode . completion-preview-mode))

(use-package grep
  :defines (find-name-arg)
  :functions (grep-apply-setting)
  :defer t
  :custom
  (find-program (if (eq system-type 'windows-nt)
                    (shell-quote-argument
                     (or (if-let* ((git (executable-find "git.exe"))
                                   (gitdir (file-name-directory git)))
                             (catch 'found
                               (dolist (candidate '("../../usr/bin/find.exe"
                                                    "../../mingw64/bin/find.exe"
                                                    "../usr/bin/find.exe"
                                                    "../mingw64/bin/find.exe"))
                                 (let ((c (expand-file-name candidate gitdir)))
                                   (when (file-executable-p c)
                                     (throw 'found c))))))
                         "find"))
                  "find"))
  :config
  (when (and (eq system-type 'windows-nt)
             (equal find-program "find"))
    (grep-apply-setting 'grep-find-template
                        "findstr /S /N /D:. /C:<R> <F>")
    (setq find-name-arg nil))
  (add-to-list 'grep-find-ignored-files ".venv")
  (add-to-list 'grep-find-ignored-directories "mxbuild")
  (add-to-list 'grep-find-ignored-directories ".agent-shell")
  (add-to-list 'grep-find-ignored-directories ".cache")
  (add-to-list 'grep-find-ignored-directories ".venv")
  (add-to-list 'grep-find-ignored-directories "eln-cache")
  (add-to-list 'grep-find-ignored-directories "site-packages"))

(use-package project
  :bind (("C-t" . project-or-external-find-file))
  :functions (project-try-vc)
  :preface
  (defcustom project-markers-filenames
    '("Cargo.toml" "compile_commands.json" "compile_flags.txt"
      "pyproject.toml" ".venv" "setup.py" "pyrightconfig.json")
    "Files or directories that indicate the root of a project."
    :type '(repeat string)
    :group 'project)

  (defun project-markers--project-root-p (path)
    "Check if the current PATH has any of the project root markers."
    (catch 'found
      (dolist (marker project-markers-filenames)
        (when (file-exists-p (concat path marker))
          (throw 'found marker)))))

  (defun project-markers-find-root (path)
    "Search up the PATH for `project-markers-filenames'."
    (when-let* ((root (locate-dominating-file path #'project-markers--project-root-p))
                (root (file-name-as-directory (expand-file-name root))))
      (if-let* ((vc-project (project-try-vc path))
                (vc-root (file-name-as-directory
                          (expand-file-name (project-root vc-project)))))
          ;; Never let marker-based detection override a deeper VC root.
          (unless (and (string-prefix-p root vc-root)
                       (not (string= root vc-root)))
            (cons 'transient root))
        (cons 'transient root))))
  :config
  (add-hook 'project-find-functions #'project-markers-find-root)
  (add-to-list 'vc-directory-exclusion-list ".venv")
  (add-to-list 'vc-directory-exclusion-list "mxbuild")
  (add-to-list 'vc-directory-exclusion-list ".agent-shell")
  (add-to-list 'vc-directory-exclusion-list ".cache")
  (add-to-list 'vc-directory-exclusion-list "site-packages")
  (add-to-list 'vc-directory-exclusion-list "eln-cache"))

(use-package vc
  :if (memq system-type '(windows-nt android))
  :custom
  (vc-revert-show-diff nil)
  (vc-handled-backends '(Git))
  (vc-dir-auto-hide-up-to-date 'revert) ;; EMACS-31
  (vc-allow-rewriting-published-history t) ;; EMACS-31
  :bind (("C-x C-z" . project-vc-dir)))

(use-package diff
  :after vc
  :custom
  (diff-command (if (eq system-type 'windows-nt)
                    (or (executable-find "diff.exe")
                        (if-let* ((git (executable-find "git.exe"))
                                  (gitdir (file-name-directory git)))
                            (catch 'found
                              (dolist (candidate '("../../usr/bin/diff.exe"
                                                   "../../mingw64/bin/diff.exe"
                                                   "../usr/bin/diff.exe"
                                                   "../mingw64/bin/diff.exe"))
                                (let ((c (expand-file-name candidate gitdir)))
                                  (when (file-executable-p c)
                                    (throw 'found c)))))))
                  "diff")))

(use-package diff-mode
  :after diff
  :bind (:map diff-mode-map
         ("c" . vc-next-action)))

(use-package vc-dir
  :after vc
  :functions (log-view-current-entry
              vc-deduce-backend vc-dir-current-file vc-dir-hide-up-to-date
              vc-dir-mark-by-regexp vc-dir-marked-files vc-git-push
              vc-revert-file)
  :bind (:map vc-dir-mode-map
         ("!" . eshell)
         ("F" . vc-pull)
         ("P" . (lambda ()
                  (interactive)
                  (if (eq 'Git (vc-deduce-backend))
                      (vc-git-push t)
                    (vc-push))))
         ("k" . (lambda ()
                  (interactive)
                  (let* ((files (or (vc-dir-marked-files)
                                    (list (vc-dir-current-file))))
                         (tracked
                          (seq-filter (lambda (file)
                                        (not (eq (vc-call-backend vc-dir-backend 'state file)
                                                 'unregistered)))
                                      files)))
                    (map-y-or-n-p "Revert %s? " #'vc-revert-file tracked)
                    (map-y-or-n-p "Delete %s? " #'delete-file files)
                    (revert-buffer))))
         ("TAB" . (lambda ()
                    (interactive)
                    (vc-diff nil nil (list (vc-deduce-backend) (list (vc-dir-current-file)) nil nil))))
         ("c" . vc-next-action)
         ("i" . vc-dir-ignore)
         ("g" . (lambda ()
                  (interactive)
                  (vc-dir-hide-up-to-date)
                  (revert-buffer)
                  (run-with-idle-timer 4 nil #'vc-dir-hide-up-to-date)))
         ("U" . (lambda ()
                  (interactive)
                  (dolist (file (vc-dir-marked-files))
                    (vc-dir-mark-by-regexp (regexp-quote (file-relative-name file (vc-root-dir))) t))))
         ("s" . (lambda ()
                  (interactive)
                  (let* ((backend (vc-deduce-backend))
                         (file (vc-dir-current-file))
                         (fileset (list backend (list file) nil nil nil)))
                    (condition-case nil (vc-register fileset) (error nil))
                    (vc-dir-mark-by-regexp (regexp-quote (file-relative-name file (vc-root-dir))) nil))))
         ("u" . (lambda ()
                  (interactive)
                  (let ((file (vc-dir-current-file)))
                    (if (eq (vc-state file) 'added) (vc-revert-file file))
                    (vc-dir-mark-by-regexp (regexp-quote (file-relative-name file (vc-root-dir))) t))))))

(use-package vc-git
  :after vc
  :bind (:map vc-git-log-edit-mode-map
         ("C-c C-a" . vc-git-log-edit-toggle-amend)
         ("C-c C-l" . vc-print-log)
         :map vc-git-log-view-mode-map
         ("v" . (lambda ()
                  (interactive)
                  (let* ((rev (log-view-current-entry))
                         (default-directory (vc-root-dir))
                         (cmd (format "%s revert --no-commit %s" vc-git-program (cadr rev))))
                    (if (yes-or-no-p (concat "Run `" cmd "`?"))
                        (shell-command cmd)))))
         ("r" . (lambda ()
                  (interactive)
                  (let* ((rev (log-view-current-entry))
                         (default-directory (vc-root-dir))
                         (cmd (format "%s rebase --allow-empty --autostash --autosquash %s" vc-git-program (cadr rev))))
                    (if (yes-or-no-p (concat "Run `" cmd "`?"))
                        (shell-command cmd)))))))

(use-package desktop
  :if (eq system-type 'gnu/linux)
  :custom
  (history-length 10)
  (desktop-restore-eager 5)
  (desktop-auto-save-timeout 15)
  (desktop-restore-frameset nil)
  (desktop-buffers-not-to-save (concat "\\("
                                       "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                                       "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
                                       "\\)$"))
  :hook
  (desktop-save-mode . (lambda () (run-with-idle-timer 4 nil #'desktop-read)))
  :config
  ;; (desktop-save-mode)
  (add-to-list 'desktop-globals-to-save 'file-name-history)
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
  (add-to-list 'desktop-modes-not-to-save 'grep-mode)
  (add-to-list 'desktop-modes-not-to-save 'magit-mode)
  (add-to-list 'desktop-modes-not-to-save 'treemacs-mode)
  (add-to-list 'desktop-modes-not-to-save 'deadgrep-mode))

(use-package savehist
  :config
  (savehist-mode 1))

(use-package which-key
  :config
  (which-key-mode)
  (which-key-setup-side-window-right-bottom)
  :custom
  (which-key-idle-delay 1.0))

(use-package cc-mode
  :defines (c-syntactic-context)
  :functions (c-update-modeline my/c-update-modeline)
  :hook ((cc-mode . timfel/infer-indentation-style)
         (java-mode . timfel/friendly-whitespace)
         (java-ts-mode . timfel/friendly-whitespace)
         (java-mode . timfel/java-indentation-setup)
         (java-ts-mode . timfel/java-indentation-setup))
  :custom
  (c-basic-offset 4)
  :config
  (defun my/c-update-modeline (oldfun)
    ;; cc-mode assumes mode-line is a plain string at all times, see e.g.
    ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2018-07/msg00339.html
    ;;
    ;; The problem is that this is simply not always true with LSP and MMM in the
    ;; mix, so we get issues. I just advice the c-update-modeline function to
    ;; make mode-name a plain string
    (let ((mode-name (substring-no-properties (format-mode-line mode-name))))
      (funcall oldfun)))
  (advice-add #'c-update-modeline :around #'my/c-update-modeline))

(use-package tramp
  :defer 3
  :custom
  (tramp-use-scp-direct-remote-copying t)
  (remote-file-name-inhibit-cache nil)
  (remote-file-name-inhibit-locks t)
  (remote-file-name-inhibit-auto-save t)
  (remote-file-name-inhibit-auto-save-visited t)
  (auto-revert-remote-files nil)
  (enable-remote-dir-locals nil)
  (tramp-copy-size-limit (* 1024 1024))
  (tramp-verbose 2)
  :preface
  (defun memoize-remote (key cache orig-fn &rest args)
  "Memoize a value if the key is a remote path."
  (if (and key
           (file-remote-p key))
      (if-let* ((current (assoc key (symbol-value cache))))
          (cdr current)
        (let ((current (apply orig-fn args)))
          (set cache (cons (cons key current) (symbol-value cache)))
          current))
    (apply orig-fn args)))
  :config
  (connection-local-set-profile-variables
   'my-remote-profile
   '((dired-check-symlinks . nil)
     (shell-history-file-name . t)
     (tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp)
   'my-remote-profile)

  (connection-local-set-profile-variables
   'my-podman-no-direct-async-profile
   '((tramp-direct-async-process . nil)))

  (connection-local-set-profiles
   '(:application tramp :protocol "podman")
   'my-podman-no-direct-async-profile)

  (with-eval-after-load 'magit
    (connection-local-set-profile-variables
     'my-remote-magit-profile
     '((magit-commit-show-diff . nil)
       (magit-branch-direct-configure . nil)
       (magit-refresh-status-buffer . nil)))

    (connection-local-set-profiles
     '(:application tramp)
     'my-remote-magit-profile)

    ;; Memoize magit top level
    (defvar magit-toplevel-cache nil)
    (defun memoize-magit-toplevel (orig &optional directory)
      (memoize-remote (or directory default-directory)
                      'magit-toplevel-cache orig directory))
    (advice-add 'magit-toplevel :around 'memoize-magit-toplevel)

    (defvar magit-tramp-pipe-stty-settings)
    (setq magit-tramp-pipe-stty-settings 'pty))

  (with-eval-after-load 'vc
    ;; setting the below thing will disable VC support for any TRAMP path
    ;; (setq vc-ignore-dir-regexp
    ;;       (format "\\(%s\\)\\|\\(%s\\)"
    ;;               vc-ignore-dir-regexp
    ;;               tramp-file-name-regexp))
    ;; memoize vc-git-root
    (defvar vc-git-root-cache nil)
    (defun memoize-vc-git-root (orig file)
      (let ((value (memoize-remote (file-name-directory file) 'vc-git-root-cache orig file)))
        ;; sometimes vc-git-root returns nil even when there is a root there
        (when (null (cdr (car vc-git-root-cache)))
          (setq vc-git-root-cache (cdr vc-git-root-cache)))
        value))
    (advice-add 'vc-git-root :around 'memoize-vc-git-root))

  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook 'tramp-compile-disable-ssh-controlmaster-options))

  (with-eval-after-load 'project
    ;; Memoize current project
    (defvar project-current-cache nil)
    (defun memoize-project-current (orig &optional prompt directory)
      (memoize-remote (or directory
                          project-current-directory-override
                          default-directory)
                      'project-current-cache orig prompt directory))
    (advice-add 'project-current :around 'memoize-project-current))

  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package re-builder
  :commands re-builder
  :config (setq reb-re-syntax 'string))

(use-package xt-mouse
  :unless (or (daemonp) (display-graphic-p))
  :config (run-with-idle-timer 0.1 nil #'xterm-mouse-mode +1))

(use-package proced
  :bind (("<f8>". proced)
         :map proced-mode-map
         ("<f8>" . quit-window))
  :config
  (setq proced-enable-color-flag t))

(use-package repeat
  :hook
  (after-init . repeat-mode)
  :custom
  (repeat-exit-timeout 5))

(use-package flymake
  :bind (("C-c f" . flymake-show-buffer-diagnostics)
         ("C-c e" . flymake-show-project-diagnostics)))

(use-package eglot
  :functions (eglot-server-capable eglot-server-capable-or-lose
              eglot-execute
              eglot-current-server)
  :bind (("C-," . eglot-code-actions)
         ("C-S-t" . xref-find-apropos))
  :custom
  (eglot-connect-timeout 120)
  (eglot-documentation-renderer 'markdown-ts-view-mode)
  (eglot-code-action-indications nil)
  (eglot-extend-to-xref t)
  :config
  (add-to-list 'eglot-server-programs
               '((ruby-mode ruby-ts-mode) "ruby-lsp"))
  (advice-add
   'eglot-workspace-folders :filter-return
   (lambda (folders)
     (vconcat
      (mapcar
       (lambda (folder)
         (let* ((copy (copy-sequence folder))
                (name (plist-get copy :name)))
           (plist-put copy :name
                      (if (string-empty-p name)
                          name
                        (file-name-nondirectory
                         (directory-file-name name))))))
       folders))))
  (defun eglot-execute-custom (command &optional arguments)
    "Execute a custom COMMAND supported by the current Eglot server.

    Interactively, offer commands advertised by the server's
    `:executeCommandProvider'.  With a prefix argument, prompt for
    ARGUMENTS as an Elisp form evaluating to a list or vector.  Empty
    input means nil arguments."
    (interactive
     (let* ((commands (append (or (eglot-server-capable
                                   :executeCommandProvider :commands)
                                  '())
                              nil))
            (command
             (progn
               (unless commands
                 (user-error "Current server advertises no custom commands"))
               (completing-read "[eglot] Execute command: "
                                commands nil t nil nil
                                (car commands))))
            (raw-arguments
             (when current-prefix-arg
               (read-from-minibuffer
                "[eglot] Arguments (Elisp list/vector, empty for nil): "))))
       (list command
             (unless (or (null raw-arguments)
                         (equal raw-arguments ""))
               (car (read-from-string raw-arguments))))))
    (eglot-server-capable-or-lose :executeCommandProvider)
    (eglot-execute
     (eglot-current-server)
     (list :command command :arguments arguments))))

(use-package gud
  :defines (gdb-many-windows gdb-use-separate-io-buffer)
  :functions (gud-basic-call)
  :commands (gdb jdb pdb)
  :custom (gud-jdb-use-classpath t)
  :bind (:map gud-mode-map
         ("C-q" . (lambda () (interactive) (gud-basic-call "quit"))))
  :hook ((gud-mode . gud-tooltip-mode)
         (gud-mode . (lambda () (window-configuration-to-register 123456))))
  :config
  ;; restore window configuration after gud exits
  (advice-add 'gud-sentinel :after (lambda (proc _msg)
                                     (when (memq (process-status proc) '(signal exit))
                                       (jump-to-register 123456)
                                       (bury-buffer))))
  ;; make command window dedicated when gud starts up
  (advice-add 'gdb-setup-windows :after (lambda ()
                                          (set-window-dedicated-p (selected-window) t)))
  ;; use the debug view with many windows
  (setq gdb-many-windows t
        gdb-use-separate-io-buffer t))

(use-package newsticker
  :commands (newsticker-treeview)
  :custom
  (newsticker-url-list
   '(("xkcd" "https://xkcd.com/rss.xml")
     ("indieretronews" "https://www.indieretronews.com/feeds/posts/default?alt=rss")
     ("osnews" "https://www.osnews.com/feed/"))))

(use-package speedbar
  :if (>= emacs-major-version 31)
  :commands (speedbar-window)
  :custom
  (speedbar-window-default-width 25)
  (speedbar-window-max-width 40)
  :bind
  (("<f6>" . #'speedbar-window)))

(use-package ibuffer
  :defer t
  :custom
  (ibuffer-human-readable-size t))

(use-package tty-tip
  :if (and (>= emacs-major-version 31) (not (display-graphic-p)))
  :ensure nil
  :functions (tty-tip-mode)
  :config
  (tty-tip-mode))

(use-package apropos
  :defer t
  :config
  (defvar-keymap help-apropos-map
    :doc "Keymap for apropos subcommands."
    "a"   #'apropos
    "l"   #'apropos-library
    "f"   #'apropos-function
    "x"   #'apropos-command
    "v"   #'apropos-variable
    "V"   #'apropos-local-variable
    "u"   #'apropos-user-option
    "d"   #'apropos-documentation
    "C-f" #'customize-apropos-faces
    "g"   #'customize-apropos-groups
    "o"   #'customize-apropos-options
    "c"   #'customize-apropos
    "i"   #'info-apropos)
  (keymap-set help-map "a" help-apropos-map))
