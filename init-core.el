;;; -*- lexical-binding: t -*-
(require 'use-package)

(use-package android
  :if (eq system-type 'android)
  :no-require t
  :after (timfel)
  :hook
  (after-init . (lambda ()
                  (run-with-idle-timer
                   1 nil
                   (lambda ()
                     (global-text-scale-adjust +8)
                     (org-agenda nil "a")))))
  :custom
  (browse-url-browser-function #'browse-url-default-android-browser)
  :config
  (require 'org-agenda)
  (require 'org-capture)

  ;; Tap targets for finger use
  (tool-bar-mode 1)
  (menu-bar-mode 1)
  (modifier-bar-mode -1) ;; and extra bar with Meta/Ctrl/Super buttons if the android kbd doesn't have them
  (customize-set-variable 'tool-bar-position 'bottom)
  (customize-set-variable 'tool-bar-always-show-default nil)
  (customize-set-variable 'tool-bar-button-margin 48)
  (customize-set-variable 'touch-screen-display-keyboard t) ;; being able to get the keyboard anywhere is good
  (setq-default tool-bar-map (make-sparse-keymap))
  (setq tool-bar-map (default-value 'tool-bar-map))
  (tool-bar-add-item "save" 'save-buffer 'save-buffer)
  ;; try to dwim ...
  (tool-bar-add-item "close"
                     (lambda ()
                       (interactive)
                       (if (> (seq-length (window-list)) 1)
                           (progn
                             (quit-window)
                             (other-window 1)
                             (delete-other-windows))
                         (quit-window)
                         (unless (derived-mode-p '(org-agenda-mode org-mode))
                           (dolist (f org-agenda-files)
                             (when-let* ((b (find-buffer-visiting f))
                                         (_ (not (buffer-modified-p b))))
                               (with-current-buffer b
                                 (revert-buffer))))
                           (find-file (car (last org-agenda-files)))
                           (org-fold-show-all)
                           (goto-char (point-max)))))
                     'close)
  (define-key-after tool-bar-map [separator-0] menu-bar-separator)
  (tool-bar-add-item "undo" 'undo 'undo)
  (tool-bar-add-item "redo" 'redo 'redo)
  (tool-bar-add-item "describe"
                     (lambda ()
                       (interactive)
                       (let ((last-input-event nil))
                         (context-menu-open)))
                     'context-menu-open)

  (add-hook 'org-mode-hook
            (lambda ()
              (unless (or (bound-and-true-p org-capture-mode) (bound-and-true-p gptel-mode))
                (setq-local tool-bar-map (copy-tree (default-value 'tool-bar-map)))
                (define-key-after tool-bar-map [separator-0] menu-bar-separator)
                (tool-bar-local-item "mail/spam"
                                     (lambda () (interactive)
                                       (org-agenda nil "a"))
                                     'agenda
                                     tool-bar-map)
                (tool-bar-local-item "mail/inbox"
                                     (lambda () (interactive)
                                       (org-capture nil "t"))
                                     'todo
                                     tool-bar-map)
                (tool-bar-local-item "mail/compose"
                                     (lambda () (interactive)
                                       (org-capture nil "n"))
                                     'note
                                     tool-bar-map)
                (tool-bar-local-item "mail/reply-all"
                                     (lambda () (interactive)
                                       (org-capture nil "m"))
                                     'meeting
                                     tool-bar-map)
                (tool-bar-local-item "conceal" 'org-cycle 'cycle
                                     tool-bar-map)
                (tool-bar-local-item "symbols/chevron_up_16" 'org-previous-visible-heading
                                     'previous tool-bar-map)
                (tool-bar-local-item "symbols/chevron_down_16" 'org-next-visible-heading
                                     'next tool-bar-map))))

  (add-hook 'org-cycle-hook
            (lambda (new-state)
              (if (eq new-state 'folded)
                  (save-excursion
                    (or (org-at-heading-p) (org-back-to-heading nil))
                    (org-fold-hide-sublevels (org-outline-level))))))

  (add-hook 'org-capture-mode-hook
            (lambda ()
              (setq-local tool-bar-map (copy-tree (default-value 'tool-bar-map)))
              (define-key-after tool-bar-map [separator-0] menu-bar-separator)
              (tool-bar-local-item "symbols/check-mark_16"
                                   #'org-capture-finalize
                                   'org-capture-finalize
                                   tool-bar-map)
              (tool-bar-local-item "symbols/cross_16"
                     #'org-capture-kill
                     'org-capture-kill
                     tool-bar-map)))

  (add-hook 'org-agenda-finalize-hook
            (lambda ()
              (setq-local tool-bar-map (make-sparse-keymap))
              (tool-bar-local-item "save" 'org-save-all-org-buffers 'save tool-bar-map)
              (tool-bar-local-item "close" 'quit-window 'close tool-bar-map)
              (tool-bar-local-item "mail/inbox"
                                   (lambda () (interactive)
                                     (org-capture nil "t"))
                                   'todo
                                   tool-bar-map)
              (tool-bar-local-item "mail/compose"
                                   (lambda () (interactive)
                                     (org-capture nil "n"))
                                   'note
                                   tool-bar-map)
              (tool-bar-local-item "mail/reply-all"
                                   (lambda () (interactive)
                                     (org-capture nil "m"))
                                   'meeting
                                   tool-bar-map)
              (tool-bar-local-item "refresh"
                                   (lambda () (interactive)
                                     (dolist (f org-agenda-files)
                                       (when-let* ((b (find-buffer-visiting f))
                                                   (_ (not (buffer-modified-p b))))
                                         (with-current-buffer b
                                           (revert-buffer))))
                                     (org-agenda-redo-all))
                                   'refresh
                                   tool-bar-map)
              (tool-bar-local-item "right-arrow"
                                   #'org-agenda-switch-to
                                   'goto
                                   tool-bar-map)))

  (setopt display-buffer-alist
          (cons '("\\*Org Agenda\\*"
                  display-buffer-same-window)
                display-buffer-alist))
  (setopt display-buffer-alist
          (cons `(,(rx bos "CAPTURE-")
                  display-buffer-same-window)
                display-buffer-alist))
  (setopt display-buffer-alist
          (cons `(,(rx "notes.org" eos)
                  display-buffer-same-window)
                display-buffer-alist))
  (setopt display-buffer-alist
          (cons `(,(rx "todo.org" eos)
                  display-buffer-same-window)
                display-buffer-alist))

  ;; No auto-save and no backup files
  (customize-set-variable 'auto-save-default nil)
  (customize-set-variable 'auto-save-visited-mode nil)
  (customize-set-variable 'make-backup-files nil)
  (customize-set-variable 'recentf-auto-cleanup 300)

  ;; Set Coding System to plain utf-8
  (if (fboundp 'set-charset-priority)
      (set-charset-priority 'unicode))
  (prefer-coding-system 'utf-8)
  (setopt locale-coding-system 'utf-8)

  ;; Create symbolic link for fonts directory from emacs dotfiles
  ;; directory.  If ~/fonts exists and fonts from user emacs directory
  ;; doesn't exist then do nothing.
  (when-let* ((target (expand-file-name "~/fonts"))
              (link (expand-file-name "fonts" user-emacs-directory))
              ((not (file-exists-p target)))
              ((file-exists-p link))
              ((yes-or-no-p "Do you want to create `fonts' folder?")))
    (make-symbolic-link link target)
    (message "Symbolic link created: %s -> %s" link target))

  ;; Android's sfnt driver only sees TrueType fonts in ~/fonts.  The Android
  ;; setup in README.org puts the managed copies in ~/.emacs.d/fonts and the
  ;; block above exposes that directory as ~/fonts.
  (setq use-default-font-for-symbols nil)
  (add-to-list 'face-ignored-fonts "Noto Color Emoji")
  (let ((families (font-family-list)))
    (when-let* ((emoji-font
                 (seq-find (lambda (font) (member font families))
                           '("Noto Emoji" "Symbola"))))
      ;; Replace the default emoji mapping rather than prepending to it, so
      ;; Android's color emoji font cannot win the fallback lookup.
      (set-fontset-font t 'emoji emoji-font))
    (let ((symbol-fonts
           (seq-filter (lambda (font) (member font families))
                       '("Noto Sans Symbols2"
                         "Noto Sans Symbols 2"
                         "Noto Sans Symbols"
                         "Noto Sans Math"
                         "Symbola"))))
      (when symbol-fonts
        (set-fontset-font t 'symbol (car symbol-fonts))
        (dolist (font (cdr symbol-fonts))
          (set-fontset-font t 'symbol font nil 'append)))))
  (let ((font (if (member "Noto Sans Mono" (font-family-list))
                  "Noto Sans Mono"
                "Droid Sans Mono")))
    (set-face-attribute 'default nil :family font :height 120)
    (set-face-attribute 'fixed-pitch nil :family font)
    (set-fontset-font t nil font))

  ;; never make me type "yes"
  (customize-set-variable 'use-short-answers t)

  ;; Make text easier to read on a phone.
  (require 'visual-wrap)
  (customize-set-variable 'line-spacing '(0.1 . 0.1))
  (customize-set-variable 'visual-wrap-extra-indent 0)
  (modify-all-frames-parameters '((internal-border-width . 32)))
  (run-with-idle-timer
   1 nil
   (lambda () (set-face-background 'fringe (face-attribute 'default :background))))
  (global-visual-line-mode t)
  (global-visual-wrap-prefix-mode 1)
  (global-hide-mode-line-mode 1)
  (customize-set-variable 'visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

  ;; Make agenda easier to read on phone
  (customize-set-variable 'org-agenda-prefix-format
        '((agenda . " %i %?-12t% s\n    ")
          (todo   . " %i")
          (tags   . " %i")
          (search . " %i")))
  (customize-set-variable 'org-deadline-warning-days 0)
  
  ;; Keyboard setup for the the no-name bluetooth phone keyboard I use. AltGr
  ;; sends KEYCODE_*, and there is no Meta key, so let's make it usable
  (define-key key-translation-map (kbd "<KEYCODE_SPACE>") (kbd "ESC"))
  (define-key key-translation-map (kbd "<KEYCODE_S>") (kbd "ß"))
  (define-key key-translation-map (kbd "<KEYCODE_Q>") (kbd "ä"))
  (define-key key-translation-map (kbd "<KEYCODE_P>") (kbd "ö"))
  (define-key key-translation-map (kbd "<KEYCODE_Y>") (kbd "ü"))
  (define-key key-translation-map (kbd "S-<KEYCODE_Q>") (kbd "Ä"))
  (define-key key-translation-map (kbd "S-<KEYCODE_P>") (kbd "Ö"))
  (define-key key-translation-map (kbd "S-<KEYCODE_Y>") (kbd "Ü"))
  (customize-set-variable 'android-intercept-control-space nil)

  ;; A long press is also the start of drag selection. If the finger is
  ;; released without moving, turn that same gesture into a context menu. The
  ;; touch-screen translator consumes touchscreen-end internally, so do this
  ;; around its point-up handler rather than binding touchscreen-end.
  (require 'touch-screen)
  (require 'mouse)
  (advice-add #'touch-screen-handle-point-up
              :around
              (lambda (orig point prefix canceled)
                (if (and (eq (nth 3 (bound-and-true-p touch-screen-current-tool)) 'held)
                         (not canceled))
                    (let ((last-input-event (list 'mouse-3 (cdr point))))
                      (context-menu-open))
                  (funcall orig point prefix canceled))))

  ;; We do not have permissions above our own and some shared folders in emacs
  ;; on android
  (setq locate-dominating-stop-dir-regexp
        (concat locate-dominating-stop-dir-regexp
                "\\|\\`/data/data/org.gnu.emacs/\\'"
                "\\|\\`/data/data/com.termux/\\'"
                "\\|\\`/content/storage/\\'"))

  :bind
  (:map org-capture-mode-map
        ("<volume-down>" . #'org-capture-finalize)
        ("<volume-up>" . #'org-capture-kill)))

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
  :defines org-agenda-files
  :mode (("\\.org$" . org-mode))
  :custom-face
  (org-level-1 ((t (:inherit outline-1 :height 1.35))))
  (org-level-2 ((t (:inherit outline-2 :height 1.25))))
  (org-level-3 ((t (:inherit outline-3 :height 1.15))))
  (org-level-4 ((t (:inherit outline-4 :height 1.08))))
  (org-level-5 ((t (:inherit outline-5 :height 1.03))))
  (org-level-6 ((t (:inherit outline-6 :height 1.0))))
  (org-level-7 ((t (:inherit outline-7 :height 1.0))))
  (org-level-8 ((t (:inherit outline-8 :height 1.0))))
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
                                  (files org-agenda-files)
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
  (setf (alist-get 'agenda org-fold-show-context-detail) 'canonical)
  :custom
  (org-refile-use-outline-path 'file)
  (org-archive-mark-done t)
  (org-image-actual-width (list 600))
  (org-log-done 'time)
  (org-insert-heading-respect-content t)
  (org-special-ctrl-a/e t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-export-backends '(ascii md html latex))
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis "…")
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
  (eglot-send-changes-idle-time 2)
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
