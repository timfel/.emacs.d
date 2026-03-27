;;; -*- lexical-binding: t -*-
(require 'package)
(require 'warnings)
(setq warning-minimum-level :error
      ad-redefinition-action 'accept
      gc-cons-threshold most-positive-fixnum)

;; keep in sync with .githooks/compile-init.el
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("cselpa" . "https://elpa.thecybershadow.net/packages/"))
(setq package-archive-priorities
      '(("melpa-stable" . 10)
	("nongnu" . 5)
	("gnu" . 5)
	("melpa" . 1)
	("cselpa" . 0)))
(package-initialize)

(require 'use-package)
;; (setq use-package-compute-statistics t)
(setq use-package-verbose t)

(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(use-package emacs
  :bind (([f11] . toggle-frame-fullscreen)
         ("M-<up>" . timfel/move-line-up)
         ("M-<down>" . timfel/move-line-down)
         ("M-q" . fill-paragraph)
         ("C-M-q" . timfel/fill-paragraph-sentence-wise)
         ("C-x <left>" . (lambda ()
		           (interactive)
		           (push-mark)
		           (if (xref-marker-stack-empty-p)
		               (previous-buffer)
		             (xref-go-back))))
         ("C-x <right>" . pop-global-mark)
         ("C-<down>" . (lambda (n) (interactive "p") (forward-line n) (scroll-up n)))
         ("C-<up>" . (lambda (n) (interactive "p") (forward-line (- n)) (scroll-down n)))
         ("C-/" . (lambda ()
		    (interactive)
		    (if mark-active
		        (call-interactively #'comment-or-uncomment-region)
		      (save-excursion
		        (beginning-of-line)
		        (set-mark (point))
		        (end-of-line)
		        (call-interactively #'comment-or-uncomment-region)))))
         ("M-]" . forward-list)
         ("C-M-]" . backward-list)
         ("C-z" . (lambda () (interactive) (beep))))

  :custom
  (browse-url-generic-program (or (executable-find "wslview") "xdg-open"))
  (browse-url-browser-function (lambda (&rest args)
                                 (if (y-or-n-p "Browse with EWW? ")
                                     (apply #'eww-browse-url args)
                                   (if (eq system-type 'windows-nt)
                                       (apply #'browse-url-default-browser args)
                                     (apply #'browse-url-generic args)))))
  (custom-file (locate-user-emacs-file "emacs-custom.el"))
  (confirm-kill-emacs 'yes-or-no-p)
  (visible-bell nil)
  (ring-bell-function #'ignore)
  (delete-by-moving-to-trash (eq system-type 'android))
  (make-backup-files nil)
  (query-replace-highlight t)
  (search-highlight t)
  (font-lock-maximum-decoration t)
  (require-final-newline t)
  (show-paren-style 'mixed)
  (inhibit-startup-screen t)
  (default-directory "~/")
  (initial-scratch-message nil)
  (comment-multi-line t)
  (comment-style 'extra-line)
  (sentence-end-double-space nil)
  (use-short-answers t)
  (fill-column 79)
  (buffer-file-coding-system 'utf-8-unix)
  (tool-bar-position 'bottom)
  (tool-bar-always-show-default t)
  (tool-bar-button-margin 16)

  :config
  (if (file-exists-p custom-file)
      (load custom-file))
  (recentf-mode t)
  (show-paren-mode t)
  (blink-cursor-mode 0)
  (tool-bar-mode (if (eq system-type 'android) 1 0))
  (menu-bar-mode (if (eq system-type 'android) 1 0))
  (modifier-bar-mode (if (eq system-type 'android) 1 0))
  (scroll-bar-mode 0)
  (column-number-mode t)
  (windmove-default-keybindings)
  (put 'narrow-to-region 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil)
  (setq-default indent-tabs-mode nil))

(use-package request ;; has not had a release in ages, but bugfixes on master
  :ensure t
  :defer t
  :pin melpa)

(use-package timfel
  :config
  (add-to-list 'load-path (expand-file-name "lisp" timfel/gist-location)))

(use-package emacs-ci
  :after timfel
  :commands ci-dashboard)

(use-package timfel-ci-extensions
  :after (timfel emacs-ci))

(use-package oca
  :after timfel
  :commands (oca-key oca-update-codex-config oca-update-opencode-config oca-codex-login))

(use-package orcl
  :after timfel
  :commands (timfel/git-merges-jira-html timfel/install-ol-cli))

(use-package timfel-agent-shell-extensions
  :commands (timfel/agent-shell-fan-out-worktrees
             timfel/magit-diff-comment-region-with-agent-shell
             timfel/agent-shell-recovery-recover-live-set
             timfel/dired-agent-shell-marked-directories
             timfel/agent-shell-tile-buffers-grid)
  :hook ((agent-shell-mode . timfel/agent-shell-recovery-track-live-set)
         (agent-shell-mode . timfel/agent-shell-retry-on-hitting-rate-limit)
         (agent-shell-mode . hack-dir-local-variables-non-file-buffer)
         (dired-mode . (lambda ()
                         (keymap-set dired-mode-map "C-x a i" #'timfel/dired-agent-shell-marked-directories))))
  :bind (("C-x a t" . timfel/agent-shell-tile-buffers-grid))
  :after timfel)

(use-package timfel-jira-extensions
  :commands (timfel/jira-periodic-issues
             timfel/jira
             timfel/jira-issues-investigate-marked-with-agent)
  :after timfel)

(use-package timfel-lsp-java-extensions
  :commands (timfel/my/lsp/find-eclipse-projects-recursively
             timfel/my/lsp/reload-all-java-buffers
             timfel/my/lsp/kill-old-java-buffers
             timfel/my/lsp/kill-all-java-buffers
             timfel/my/lsp/clear-workspace
             timfel/my/lsp/import-eclipse-projects
             timfel/my/lsp/rebuild-java)
  :after timfel)

(use-package presentation
  :commands (presentation-mode)
  :custom
  (presentation-default-text-scale 4))

(use-package wsl-interop
  :commands (wsl-p
             wsl-powershell-command wsl-powershell-command-to-string
             wsl-cmd-command wsl-cmd-command-to-string
             wsl-powershell-async-command wsl-powershell-start-process
             wsl-cmd-async-command wsl-cmd-start-process))

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

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml$" "\\.yaml$" "Gemfile.lock$"))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md$")
  :config
  (setq markdown-command "cmark-gfm --extension table")
  (with-eval-after-load 'markdown-overlays
    (advice-add 'markdown-overlays--parse-local-link :around
                (lambda (original-fn url)
                  "Treat an existing plain local path URL as a local file link."
                  (or (funcall original-fn url)
                      (let ((filepath (expand-file-name url)))
                        (when (or (file-exists-p filepath)
                                  (file-directory-p filepath))
                          (list (cons :file filepath)
                                (cons :line nil)))))))))

(use-package lua-mode
  :ensure t
  :defines (eglot-server-programs)
  :config (let ((lsp-clients-emmy-lua-jar-path (locate-user-emacs-file "lsp-servers/emmylua.jar")))
            (when (not (file-exists-p lsp-clients-emmy-lua-jar-path))
              (mkdir (file-name-directory lsp-clients-emmy-lua-jar-path) t)
              (url-copy-file
               "https://github.com/EmmyLua/EmmyLua-LanguageServer/releases/download/0.5.16/EmmyLua-LS-all.jar"
               lsp-clients-emmy-lua-jar-path))
            (with-eval-after-load 'eglot
              (add-to-list 'eglot-server-programs `(lua-mode . ,`("java" "-cp" ,lsp-clients-emmy-lua-jar-path "com.tang.vscode.MainKt")))))
  :mode ("\\.lua$"))

(use-package json-mode
  :ensure t
  :mode ("\\.json$"))

(use-package ruby-mode
  :mode ("\\.rb$" "\\.rjs$" "\\.rake$" "Rakefile$" "Gemfile$" "Vagrantfile$")
  :hook ((ruby-mode . (lambda() (progn
                                  (set (make-local-variable 'indent-tabs-mode) 'nil)
                                  (set (make-local-variable 'tab-width) 2)))))
  :config (defun ruby-accurate-end-of-block (&optional end)
            "(tfel): Fixes an issue I had with ruby-mode."
            (let (state
                  (end (or end (point-max))))
              (while (and (setq state (apply 'ruby-parse-partial end state))
                          (nth 2 state) (>= (nth 2 state) 0) (< (point) end))))))

(use-package org
  :commands org-mode
  :defines (ctl-x-o-map)
  :mode (("\\.org$" . org-mode))
  :init
  (define-prefix-command 'ctl-x-o-map)
  :bind (:map ctl-x-map
	 ("o" . ctl-x-o-map)
	 :map ctl-x-o-map
         ("w" . other-window) ;; because I clobbered the other window command
	 ("a" . org-agenda)
	 ("c" . org-capture)
         :map org-mode-map
         ("C-c <right>" . org-shiftright)
         ("C-c <left>" . org-shiftleft)
         ("C-c M-RET" . org-insert-subheading))
  :custom
  (org-image-actual-width (list 600))
  (org-log-done 'time)
  (org-return-follows-link t)
  (org-file-apps '((auto-mode . emacs)
                   ("\\.mm\\'" . default)
                   ("\\.x?html?\\'" . default)
                   ("\\.pdf\\'" . "evince %s")))
  (org-replace-disputed-keys t)
  (org-deadline-warning-days 7)
  (org-agenda-span 'fortnight)
  (org-agenda-skip-scheduled-if-deadline-is-shown t)
  (org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
  (org-agenda-todo-ignore-deadlines 'all)
  (org-agenda-todo-ignore-scheduled 'all)
  (org-agenda-sorting-strategy '((agenda deadline-up priority-down)
                                (todo priority-down category-keep)
                                (tags priority-down category-keep)
                                (search category-keep)))
  (org-insert-mode-line-in-empty-file t)
  (org-adapt-indentation nil "do not shift lower items")
  (org-hide-leading-stars t "i like this more")
  (org-priority-highest ?A)
  (org-priority-lowest ?C)
  (org-priority-default ?A)
  (org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                       (?B . (:foreground "LightSteelBlue"))
                       (?C . (:foreground "OliveDrab"))))
  (org-agenda-window-setup 'current-window)
  (org-clock-idle-time 15)
  (org-agenda-files (list (expand-file-name "SyncFolder/todo.org" timfel/cloud-storage)
                          (expand-file-name "SyncFolder/notes.org" timfel/cloud-storage)))
  (org-capture-templates `(("n" "note" entry
                             (file+olp+datetree ,(expand-file-name "SyncFolder/notes.org" timfel/cloud-storage))
                             "* %?\nEntered on %U\n"))))

(use-package org-tempo
  :after org)

(use-package ox-gfm
  :ensure t
  :after org)

(use-package org-download
  :ensure t
  :after org
  :commands org-download-screenshot
  :custom
  (org-download-image-org-width 200)
  (org-download-screenshot-method (if (eq system-type 'windows-nt)
                                      (format "%s %s"
                                              (executable-find "python3")
                                              (expand-file-name "~/dotfiles/bin/wslscr.py %s"))
                                    (expand-file-name "~/bin/wslscr.py %s")))
  (org-download-image-dir (expand-file-name "Screenshots/" timfel/cloud-storage)))

(use-package imenu
  :custom
  (imenu-auto-rescan t)
  (imenu-flatten 'group)
  :bind (("C-." . imenu)))

(use-package eldoc
  :custom (eldoc-documentation-strategy #'eldoc-documentation-compose)
  :functions (eldoc-display-in-buffer-at-point)
  :bind (([remap display-local-help] . timfel/local-help-or-doc))
  :config
  (defun timfel/local-help-or-doc ()
    (interactive)
    (unless (display-local-help t)
      (call-interactively #'eldoc-print-current-symbol-info)))
  (remove-hook 'eldoc-display-functions #'eldoc-display-in-buffer)
  (remove-hook 'eldoc-display-functions #'eldoc-display-in-buffer-at-point)
  (add-hook 'eldoc-display-functions #'eldoc-display-in-echo-area))

(use-package icomplete
  :functions (icomplete-fido-delete-char
              icomplete-fido-ret
              icomplete-fido-backward-updir
              icomplete-forward-completions
              icomplete-backward-completions
              icomplete-minibuffer-setup
              icomplete-ret
              icomplete-force-complete)
  :bind (:map icomplete-minibuffer-map
              ("RET" . #'icomplete-fido-ret)
              ("C-<return>" . #'icomplete-ret)
              ("TAB" . #'icomplete-force-complete)
              ("DEL" . #'icomplete-fido-backward-updir)
              ("C-d" . #'icomplete-fido-delete-char)
              ("<right>" . #'icomplete-forward-completions)
              ("<left>" . #'icomplete-backward-completions)
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
  :config
  (setq completion-ignore-case t
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t)
  (icomplete-mode t)
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
  :ensure nil
  :hook (prog-mode . completion-preview-mode))

(use-package grep
  :defines (find-name-arg)
  :functions (grep-apply-setting)
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

(use-package vscode-project
  :after project)

(use-package company
  :ensure t
  :bind (("M-?" . company-complete))
  :config (global-company-mode t)
  :custom
  (company-dabbrev-code-everywhere t)
  (company-dabbrev-other-buffers 'all)
  (company-dabbrev-ignore-case 'keep-prefix)
  (company-dabbrev-downcase 0)
  (company-idle-delay (if (eq system-type 'windows-nt) 10 0.2)))

(use-package vc
  :if (eq system-type 'windows-nt)
  :custom
  (vc-revert-show-diff nil)
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
                  "diff"))
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

(use-package magit
  :unless (eq system-type 'windows-nt)
  :bind (("C-x C-z" . magit-status)
         :map magit-diff-mode-map
         ("C-c c" . timfel/magit-diff-comment-region-with-agent-shell))
  :ensure t
  :custom
  (magit-auto-revert-tracked-only t)
  :config
  (magit-auto-revert-mode))

(use-package all-the-icons
  :unless (eq system-type 'windows-nt)
  :ensure t)

(use-package all-the-icons-completion
  :unless (eq system-type 'windows-nt)
  :after all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :unless (eq system-type 'windows-nt)
  :after all-the-icons
  :ensure t)

(use-package doom-modeline
  ;; remember to run (all-the-icons-install-fonts) manually some time
  :unless (eq system-type 'windows-nt)
  :after all-the-icons
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-minor-modes t)
  (doom-modeline-bar-width 4)
  (doom-modeline-hud nil)
  (doom-modeline-vcs-max-length 28)
  (doom-modeline-lsp t)
  (doom-modeline-buffer-file-name-style 'truncate-all)
  :custom-face
  (doom-modeline-buffer-file ((t (:foreground "black" :weight bold))))
  (doom-modeline-buffer-modified ((t (:foreground "#444" :weight bold)))))

(use-package desktop
  :custom
  (history-length 10)
  (desktop-restore-eager 5)
  (desktop-auto-save-timeout 15)
  (desktop-restore-frameset nil)
  (desktop-buffers-not-to-save (concat "\\("
                                       "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                                       "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
                                       "\\)$"))
  :config
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

(use-package treemacs
  :ensure t
  :defer t
  :bind (("<f5>" . treemacs))
  :config
  (treemacs-project-follow-mode)
  :custom
  (treemacs-file-follow-delay 1.0)
  (treemacs-width 45)
  (treemacs-width-is-initially-locked t))

(use-package treemacs-nerd-icons
  :unless (display-graphic-p)
  :functions (treemacs-load-theme)
  :after treemacs
  :ensure t
  :config
  (treemacs-load-theme "nerd-icons"))

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
	 (java-mode . (lambda ()
			(set-fill-column 99)
			(c-set-offset 'substatement-open 0)
			(c-set-offset 'case-label '+)
			(c-set-offset 'arglist-close 0)
			(if (assoc 'inexpr-class c-offsets-alist)
			    (c-set-offset 'inexpr-class 0))
			(c-set-offset 'arglist-cont-nonempty
				      (lambda (_syntax)
					(save-excursion
					  (if (and (= (length c-syntactic-context) 2)
						   (eq (caar c-syntactic-context) 'arglist-cont-nonempty)
						   (or
						    (eq (caadr c-syntactic-context) 'statement-block-intro)
						    (eq (caadr c-syntactic-context) 'block-close)))
					      0
					    16)))))))
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

(use-package mmm-mode
  :ensure t
  :functions (mmm-add-mode-ext-class)
  :commands (mmm-parse-buffer)
  :hook (java-mode . (lambda () (mmm-parse-buffer)))
  :config (progn
            (require 'mmm-auto)
            (setq mmm-global-mode 'maybe)
            (defun polyglot-mode-match (front)
              (if (string-match "\"\\([[:word:]]+\\)\",[[:space:]]*\n?[[:space:]]*\"\"\"" front)
                  (let ((sym (intern-soft (concat (downcase (match-string-no-properties 1 front)) "-mode"))))
                    (if (fboundp sym) sym 'text-mode))
                (if (string-match "//[[:space:]]*language=\\([[:word:]]+\\)[[:space:]]*\n.+\"\"\"" front)
                    (let ((sym (intern-soft (concat (downcase (match-string-no-properties 1 front)) "-mode"))))
                      (if (fboundp sym) sym 'text-mode))
                'text-mode)))
            (mmm-add-classes
             '((java-text-block
                :match-submode polyglot-mode-match
                :front ".+\n.+\"\"\"$"
                :back ".*\"\"\".*"
                :face mmm-code-submode-face)))
            (mmm-add-classes
             '((md-javascript-block
                :submode javascript-mode
                :front "<script>"
                :back "</script>"
                :face mmm-code-submode-face)))
            (defun github-code-match (front)
              (if (string-match "```\\([[:word:]]+\\)" front)
                  (let ((sym (intern-soft (concat (downcase (match-string-no-properties 1 front)) "-mode"))))
                    (if (fboundp sym) sym 'fundamental-mode))
                'fundamental-mode))
            (mmm-add-classes
             '((md-github-code-block
                :match-submode github-code-match
                :front "```[a-z]+"
                :back "```"
                :face mmm-code-submode-face)))
            (mmm-add-mode-ext-class 'java-mode "\\.java$" 'java-text-block)
            (mmm-add-mode-ext-class 'markdown-mode "\\.md$" 'md-javascript-block)
            (mmm-add-mode-ext-class 'markdown-mode "\\.md$" 'md-github-code-block)))

(use-package eclipse-theme
  :ensure t
  :config
  (load-theme 'eclipse t))

(use-package tramp
  :defer 30
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package ido
  :disabled
  :functions (ido-everywhere)
  :config
  (ido-mode t)
  (ido-everywhere t))

(use-package dumb-jump
  :ensure t
  :defer 10
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package jsonnet-mode
  :ensure t
  :mode ("\\.jsonnet$"))

(use-package sudo-save
  :unless (eq system-type 'windows-nt))

(use-package term-keys
  :ensure t
  :unless (display-graphic-p)
  :config
  (global-set-key (kbd "M-[ 1 ; 2 a") (kbd "S-<up>"))
  (global-set-key (kbd "M-[ 1 ; 2 b") (kbd "S-<down>"))
  (global-set-key (kbd "M-[ 1 ; 5 a") (kbd "C-<up>"))
  (global-set-key (kbd "M-[ 1 ; 5 b") (kbd "C-<down>"))
  (global-set-key (kbd "M-[ 1 ; 5 d") (kbd "C-<left>"))
  (global-set-key (kbd "M-[ 1 ; 5 c") (kbd "C-<right>"))
  (term-keys-mode t))

(use-package sixel-graphics
  :ensure t
  :unless (display-graphic-p)
  :vc (:url "https://github.com/timfel/sixel-graphics.el" :branch "main" :rev :newest)
  :config
  (sixel-graphics-mode t))

(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt$" "\\.cmake$")
  :commands cmake-mode)

(use-package deadgrep
  :ensure t
  :commands (rg deadgrep)
  :config (defun rg (what where)
            (interactive (list
                          (read-string
                           "Search what? "
                           (if (use-region-p)
                               (buffer-substring-no-properties (region-beginning) (region-end))
                             (if (symbol-at-point) (prin1-to-string (symbol-at-point)))))
                          (read-directory-name "Search where? ")))
            (deadgrep what where)))

(use-package pypytrace-mode
  :commands pypytrace-mode)

(use-package javap-handler)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :defer 3
  :custom
  (exec-path-from-shell-shell-name
   (if (eq system-type 'windows-nt) "powershell.exe" nil))
  (exec-path-from-shell-variables
   '("PATH"
     "MANPATH"
     "JAVA_HOME"
     "JDT"
     "SDKMAN_DIR"
     "PIP_CACHE_DIR"
     "MAVEN_OPTS"
     "GRADLE_USER_HOME"
     "UV_INSTALL_DIR"
     "UV_CACHE_DIR"
     "UV_PYTHON_CACHE_DIR"
     "UV_PYTHON_INSTALL_DIR"
     "UV_TOOL_DIR"
     "UV_PYTHON_BIN_DIR"
     "UV_TOOL_BIN_DIR"
     "PYENV"
     "PYENV_HOME"
     "PYENV_ROOT"
     "PYENV_VERSION"
     "PYTHONIOENCODING"
     "MX_CACHE_DIR"
     "MX_ASYNC_DISTRIBUTIONS"
     "MX_PYTHON_VERSION"
     "MX_BUILD_SHALLOW_DEPENDENCY_CHECKS"
     "MX_OUTPUT_ROOT_INCLUDES_CONFIG"
     "MX_BUILD_EXPLODED"
     "MX_COMPDB"
     "LINKY_LAYOUT"))
  :config
  (if (eq system-type 'windows-nt)
      (require 'exec-path-from-powershell))
  (exec-path-from-shell-initialize))

(use-package rustic
  :ensure t
  :mode ("\\.rs$" . rustic-mode))

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("C-S-d" . mc/mark-more-like-this-extended)))

(use-package adaptive-wrap
  :ensure t
  :commands adaptive-wrap-prefix-mode)

(use-package emojify
  :ensure t
  :if (display-graphic-p)
  :commands emojify-insert-emoji
  :custom
  (emojify-display-style 'unicode)
  (emojify-emoji-styles '(unicode)))

(use-package re-builder
  :commands re-builder
  :config (setq reb-re-syntax 'string))

(use-package gptel
  :ensure t
  :defines (cashpw/gptel-mode-line--indicator-querying
            cashpw/gptel-mode-line--indicator-responding
            cashpw/gptel-show-progress-in-mode-line)
  :functions (cashpw/gptel-mode-line cashpw/gptel-mode-line--hide-all
              cashpw/gptel-mode-line--indicator gptel-abort)
  :commands (gptel gptel-request)
  :custom
  (gptel-model 'gemma3n:latest)
  (gptel-include-tool-results t)
  (gptel-include-reasoning t)
  :config
  (setq gptel-backend (gptel-make-ollama "Ollama"
                        :host "localhost:11434"
                        :stream t
                        :models '(gemma3n:latest gemma3n-tools)))
  (ignore-errors (oca-key))
  (setq
   cashpw/gptel-mode-line--indicator-querying "↑GPTEL↑ "
   cashpw/gptel-mode-line--indicator-responding "↓GPTEL↓ "
   cashpw/gptel-show-progress-in-mode-line t)
  (defun cashpw/gptel-mode-line--indicator (mode)
    "Return indicator string for MODE."
    (pcase mode
      ('querying
       cashpw/gptel-mode-line--indicator-querying)
      ('responding
       cashpw/gptel-mode-line--indicator-responding)
      (_
       "")))
  (defun cashpw/gptel-mode-line (command mode)
    "Update mode line to COMMAND (show|hide) indicator for MODE."
    (when cashpw/gptel-show-progress-in-mode-line
      (let ((indicator (list t (cashpw/gptel-mode-line--indicator mode))))
        (pcase command
          ('show
           (cl-pushnew indicator global-mode-string :test #'equal))
          ('hide
           (setf global-mode-string (remove indicator global-mode-string)))))
      (force-mode-line-update t)))
  (defun cashpw/gptel-mode-line--hide-all (&rest _)
    (cashpw/gptel-mode-line 'hide 'querying)
    (cashpw/gptel-mode-line 'hide 'responding))
  (defun cashpw/gptel-mode-line--show-querying ()
    (cashpw/gptel-mode-line--hide-all)
    (cashpw/gptel-mode-line 'show 'querying))
  (defun cashpw/gptel-mode-line--show-responding ()
    (cashpw/gptel-mode-line--hide-all)
    (cashpw/gptel-mode-line 'show 'responding))
  (add-hook 'gptel-post-request-hook 'cashpw/gptel-mode-line--show-querying)
  (add-hook 'gptel-pre-response-hook 'cashpw/gptel-mode-line--show-responding)
  (add-hook 'gptel-post-response-functions 'cashpw/gptel-mode-line--hide-all)

  (advice-add 'keyboard-quit :before (lambda (&rest _args) (ignore-errors (gptel-abort (current-buffer)))))

  (defun timfel/gptel-complete ()
    (interactive
     (let ((query (if (use-region-p)
                      (buffer-substring-no-properties (region-beginning)
                                                      (region-end))
                    (buffer-substring-no-properties (point-min)
                                                    (point))))
           (gptel-use-tools nil)
           (gptel-include-reasoning nil))
       (gptel-request query
                      :callback (lambda (response info)
                                  (let* ((start-marker (plist-get info :position))
                                         (tracking-marker (plist-get info :tracking-marker)))
                                    (if (stringp response)
                                        (save-excursion
                                          (with-current-buffer (marker-buffer start-marker)
                                            (goto-char (or tracking-marker start-marker))
                                            (insert response)
                                            (plist-put info :tracking-marker (setq tracking-marker (point-marker))))))))
                      :stream gptel-stream
                      :system "Continue writing until the current control flow is completed or the task described in the last comment is done. Only write code, no markup, no communication, no explanations, do not repeat parts of the request, just continue writing the code."))))

  (setq gptel-directives (let* ((promptdir (expand-file-name "prompts" user-emacs-directory))
                                (prompt-files (directory-files promptdir t "md$")))
                           (mapcar (lambda (prompt-file)
                                     (with-temp-buffer
                                       (insert-file-contents prompt-file)
                                       (let ((prompt-description "NO DESCRIPTION"))
                                         ;; nab the description - single-line descriptions only!
                                         (goto-char (point-min))
                                         (when (re-search-forward "#\\+description: \\(.*?\\) *--> *$" nil t)
                                           (setq prompt-description (match-string 1)))
                                         ;; remove all comments
                                         (delete-matching-lines "^ *<!--" (point-min) (point-max))
                                         (delete-matching-lines "^$" (point-min) (+ 1 (point-min))) ; remove first blank line if exists
                                         (goto-char (point-min)) ;; not necessary, point is in the midst of comments to start
                                         ;; return the megillah
                                         (list
                                          (intern (file-name-directory prompt-file)) ; gptel-directives key
                                          prompt-description
                                          (buffer-substring-no-properties (point-min) (point-max)) ))))
                                   prompt-files)))
  :bind (("C-x a i" . gptel-send)
         ("C-x a c" . timfel/gptel-complete)))

(use-package llm-tool-collection
  :ensure t
  :after gptel
  :vc (:url "https://github.com/skissue/llm-tool-collection" :branch "main" :rev :newest))

(use-package timfel-gptel-tools
  :after (gptel llm-tool-collection))

(use-package emacs-theme-detection
  :ensure t
  :defer t
  :functions (emacs-theme-detection-is-dark emacs-theme-detection-is-light)
  :init
  (autoload #'emacs-theme-detection-is-dark "emacs-theme-detection")
  (autoload #'emacs-theme-detection-is-light "emacs-theme-detection")
  :vc (:url "https://github.com/timfel/emacs-theme-detection.git" :branch "main" :rev :newest))

(use-package xt-mouse
  :if (eq window-system nil)
  :config (run-with-idle-timer 0.1 nil #'xterm-mouse-mode +1))

(use-package proced
  :ensure t
  :bind (("<f8>". proced)
         :map proced-mode-map
         ("<f8>" . quit-window))
  :config
  (setq proced-enable-color-flag t))

(use-package vterm
  :ensure t
  :defines (vterm-mode-map vterm--process)
  :commands (vterm)
  :unless (memq system-type '(windows-nt android))
  :bind (("<f12>" . (lambda ()
                      (interactive)
                      (if-let ((w (get-window-with-predicate (lambda (w) (string-prefix-p "*vterm" (buffer-name (window-buffer w)))))))
                          (select-window w)
                        (let ((w (split-window (selected-window) -18)))
                          (select-window w)
                          (let ((buf (seq-find (lambda (b) (string-prefix-p "*vterm" (or (buffer-name b) ""))) (buffer-list))))
                            (if buf
                                (switch-to-buffer buf)
                              (vterm t)
                              (add-hook 'kill-buffer-hook #'delete-window 0 t)))
                          (set-window-dedicated-p w t)))))
         :map vterm-mode-map
         ("C-x b" . #'switch-to-buffer-other-window)
         ("C-x C-f" . (lambda ()
                        (interactive)
                        (when vterm--process
                          (let* ((pid (process-id vterm--process))
                                 (dir (file-truename (format "/proc/%d/cwd/" pid))))
                            (setq default-directory dir)))
                        (call-interactively (keymap-lookup (current-global-map) "C-x C-f"))))
         ("C-x <left>" . (lambda () (interactive)
                           (let* ((bl (seq-sort (lambda (a b) (string-lessp (buffer-name a) (buffer-name b))) (buffer-list)))
                                  (before (seq-take-while (lambda (b) (not (eq b (current-buffer)))) bl))
                                  (after (seq-difference bl before)))
                             (set-window-dedicated-p (selected-window) nil)
                             (set-window-buffer (selected-window)
                              (seq-find (lambda (b) (string-prefix-p "*vterm" (or (buffer-name b) ""))) (seq-reverse before)
                                        (seq-find (lambda (b) (string-prefix-p "*vterm" (or (buffer-name b) ""))) (seq-reverse after))))
                             (set-window-dedicated-p (selected-window) t))))
         ("C-x <right>" . (lambda () (interactive)
                            (let* ((bl (seq-sort (lambda (a b) (string-lessp (buffer-name a) (buffer-name b))) (buffer-list)))
                                   (before (seq-concatenate 'list (seq-take-while (lambda (b) (not (eq b (current-buffer)))) bl) (list (current-buffer))))
                                   (after (seq-difference bl before)))
                              (set-window-dedicated-p (selected-window) nil)
                              (set-window-buffer (selected-window)
                               (seq-find (lambda (b) (string-prefix-p "*vterm" (or (buffer-name b) ""))) after
                                         (seq-find (lambda (b) (string-prefix-p "*vterm" (or (buffer-name b) ""))) before)))
                              (set-window-dedicated-p (selected-window) t))))
         ("C-x c" . (lambda () (interactive)
                      (set-window-dedicated-p (selected-window) nil)
                      (vterm t)
                      (set-window-dedicated-p (selected-window) t)))
         ("<f12>" . delete-window))
  :custom
  (vterm-max-scrollback 40000))

(use-package eshell
  :if (memq system-type '(windows-nt android))
  :after exec-path-from-shell
  :defer t
  :bind (("<f12>" . (lambda ()
                      (interactive)
                      (if-let ((w (get-window-with-predicate (lambda (w) (string-prefix-p "*eshell" (buffer-name (window-buffer w)))))))
                          (select-window w)
                        (let ((w (split-window (selected-window) -18)))
                          (select-window w)
                          (let ((buf (seq-find (lambda (b) (string-prefix-p "*eshell" (or (buffer-name b) ""))) (buffer-list))))
                            (if buf
                                (switch-to-buffer buf)
                              (eshell t)
                              (add-hook 'kill-buffer-hook #'delete-window 0 t)))
                          (set-window-dedicated-p w t)))))))

(use-package esh-mode
  :defer t
  :bind (:map eshell-mode-map
         ("C-x b" . #'switch-to-buffer-other-window)
         ("C-x <left>" . (lambda () (interactive)
                           (let* ((bl (seq-sort (lambda (a b) (string-lessp (buffer-name a) (buffer-name b))) (buffer-list)))
                                  (before (seq-take-while (lambda (b) (not (eq b (current-buffer)))) bl))
                                  (after (seq-difference bl before)))
                             (set-window-dedicated-p (selected-window) nil)
                             (set-window-buffer (selected-window)
                              (seq-find (lambda (b) (string-prefix-p "*eshell" (or (buffer-name b) ""))) (seq-reverse before)
                                        (seq-find (lambda (b) (string-prefix-p "*eshell" (or (buffer-name b) ""))) (seq-reverse after))))
                             (set-window-dedicated-p (selected-window) t))))
         ("C-x <right>" . (lambda () (interactive)
                            (let* ((bl (seq-sort (lambda (a b) (string-lessp (buffer-name a) (buffer-name b))) (buffer-list)))
                                   (before (seq-concatenate 'list (seq-take-while (lambda (b) (not (eq b (current-buffer)))) bl) (list (current-buffer))))
                                   (after (seq-difference bl before)))
                              (set-window-dedicated-p (selected-window) nil)
                              (set-window-buffer (selected-window)
                               (seq-find (lambda (b) (string-prefix-p "*eshell" (or (buffer-name b) ""))) after
                                         (seq-find (lambda (b) (string-prefix-p "*eshell" (or (buffer-name b) ""))) before)))
                              (set-window-dedicated-p (selected-window) t))))
         ("C-x c" . (lambda () (interactive)
                      (set-window-dedicated-p (selected-window) nil)
                      (eshell t)
                      (set-window-dedicated-p (selected-window) t)))
         ("<f12>" . delete-window)))

(use-package eglot-booster
  :after eglot
  :if (memq system-type '(windows-nt gnu/linux))
  :vc (:url "https://github.com/jdtsmith/eglot-booster" :branch "main" :rev :newest)
  :ensure t
  :custom (eglot-booster-io-only t)
  :commands (eglot-booster-mode)
  :config
  (unless (executable-find "emacs-lsp-booster")
    (let ((lsp-booster-exe (locate-user-emacs-file "lsp-servers/emacs-lsp-booster")))
      (when (not (file-exists-p lsp-booster-exe))
        (mkdir (file-name-directory lsp-booster-exe) t)
        (let ((zip (concat lsp-booster-exe ".zip")))
          (url-copy-file
           (pcase system-type
             ('windows-nt "https://github.com/blahgeek/emacs-lsp-booster/releases/download/v0.2.1/emacs-lsp-booster_v0.2.1_x86_64-pc-windows-gnu.zip")
             ('gnu/linux "https://github.com/blahgeek/emacs-lsp-booster/releases/download/v0.2.1/emacs-lsp-booster_v0.2.1_x86_64-unknown-linux-musl.zip"))
           zip)
          (let ((default-directory (file-name-directory zip)))
            (shell-command (concat "unzip " zip)))
          (delete-file zip)))
      (add-to-list 'exec-path (file-name-directory lsp-booster-exe)))))

(use-package redo+
  :bind (("C--" . redo)))

(use-package flymake
  :bind (("C-c f" . flymake-show-buffer-diagnostics)
         ("C-c e" . flymake-show-project-diagnostics)))

(use-package eglot
  :functions (eglot-server-capable eglot-server-capable-or-lose
              eglot-execute
              eglot-current-server)
  :bind (("C-," . eglot-code-actions)
         ("C-S-t" . xref-find-apropos))
  :config
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

(use-package timfel-eglot-java-extensions
  :after eglot
  :functions (timfel/eglot-jdtls)
  :config
  (add-to-list 'eglot-server-programs
               (cons '(java-mode java-ts-mode) #'timfel/eglot-jdtls)))


(use-package yasnippet
  :ensure t
  :after (lsp-mode)
  :hook ((lsp-mode . yas-minor-mode)))

(use-package lsp-mode
  :preface (setq lsp-use-plists t)
  :defines (lsp-headerline-arrow)
  :functions (c-clear-string-fences c-restore-string-fences
              dired-get-file-for-visit lsp--line-character-to-point
              lsp-booster--advice-final-command
              lsp-booster--advice-json-parse lsp-diagnostics
              lsp-find-definition lsp-find-references
              lsp-execute-code-action lsp-ido-workspace-symbol
              my/c-clear-string-fences)
  :ensure t
  :commands (lsp)
  :bind (:map lsp-mode-map
         ("C-," . lsp-execute-code-action)
         ("M-." . lsp-find-definition)
         ("C-M-." . lsp-find-references)
         ("C-S-t" . lsp-ido-workspace-symbol))
  :hook ((lsp-mode . flymake-mode))
  :custom
  (lsp-print-io nil)
  (lsp-lens-enable t)
  (lsp-completion-enable-additional-text-edit t)
  (lsp-enable-snippet t)
  (lsp-enable-indentation nil)
  (lsp-before-save-edits t)
  (lsp-enable-file-watchers nil)
  (lsp-print-performance nil)
  (lsp-report-if-no-buffer t)
  (lsp-enable-snippet t)
  (lsp-enable-xref t)
  (lsp-diagnostics-provider :flymake)
  (lsp-completion-enable t)
  (lsp-completion-filter-on-incomplete nil)
  (lsp-completion-show-detail t)
  (lsp-completion-show-kind nil)
  (lsp-completion-sort-initial-results nil)
  (lsp-response-timeout 30)
  (lsp-diagnostic-clean-after-change nil)
  (lsp-eldoc-render-all t)
  (lsp-eldoc-enable-hover t)
  (lsp-idle-delay 1.000)
  (lsp-tcp-connection-timeout 20)
  (lsp-modeline-diagnostics-enable t)
  (lsp-modeline-code-actions-enable nil)
  :config
  (require 'treemacs)
  (setq lsp-headerline-arrow ">")
  (defun lsp-goto-next-diagnostic ()
    "Get lsp-diagnostics, it returns a hash mapping file names to a list of
	 	hashes, each of which is a diagnostic. Search in the file names for the
	 	current buffer's file name. If found, search the list of diagnostics.
	 	Get the vallue for the :range key, and compare the :start of the
	 	resulting hash with the current point position until we find the next
	 	diagnostic that is after the current point. If found, set the point to
	 	that next diagnistic's start position. If there are no more diagnistics
	 	after the current point in the list, take the next file name from the
	 	outer hash. If the file name that got picked is not the current buffer,
	 	open the file and position the point at the start of the range of the
	 	first hash in the list of diagnistics."
    (interactive)
    (let* ((current-file (or (buffer-file-name) (dired-get-file-for-visit)))
           (diagnostics-table (lsp-diagnostics))
           (all-files (hash-table-keys diagnostics-table))
           (files-ordered (seq-sort #'string-lessp all-files))
           (point-pos (point))
           (found (gethash current-file diagnostics-table))
           (files-to-seek (seq-drop-while
                           (lambda (f) (and found (not (string-equal f current-file))))
                           files-ordered)))
      (catch 'done
        (dolist (file files-to-seek)
          (let* ((diagnostics (gethash file diagnostics-table))
                 (next-diag
                  (seq-find (lambda (diag)
                              (let ((range (plist-get diag :range))
                                    (severity (plist-get diag :severity)))
                                (when (and range (< severity 2))
                                  (let* ((start (plist-get range :start))
                                         (pos (lsp--line-character-to-point
                                               (plist-get start :line)
                                               (plist-get start :character))))
                                    (or
                                     (not (equal file current-file))
                                     (< point-pos pos))))))
                            diagnostics)))
            (when next-diag
              (let* ((range (plist-get next-diag :range))
                     (start (plist-get range :start)))
                (unless (equal file current-file)
                  (find-file file))
                (goto-char (point-min))
                (forward-line (plist-get start :line))
                (forward-char (plist-get start :character))
                (message "Jumped to diagnostic: %s (%d)" (plist-get next-diag :message) (plist-get next-diag :severity))
                (throw 'done t))))))))

  (defun lsp-goto-previous-diagnostic ()
    "Move to the previous diagnostic before point."
    (interactive)
    (let* ((current-file (or (buffer-file-name) (dired-get-file-for-visit)))
           (diagnostics-table (lsp-diagnostics))
           (all-files (hash-table-keys diagnostics-table))
           (files-ordered (seq-sort #'string-lessp all-files))
           (point-pos (point))
           (found (gethash current-file diagnostics-table))
           (files-to-seek (seq-reverse (append (seq-take-while
                                                (lambda (f) (and found (not (string-equal f current-file))))
                                                files-ordered)
                                               (list current-file)))))
      (catch 'done
        (dolist (file files-to-seek)
          (let* ((diagnostics (gethash file diagnostics-table))
                 (reversed-diags (reverse diagnostics))
                 (prev-diag
                  (seq-find (lambda (diag)
                              (let ((range (plist-get diag :range))
                                    (severity (plist-get diag :severity)))
                                (when (and range (< severity 2))
                                  (let* ((start (plist-get range :start))
                                         (pos (lsp--line-character-to-point
                                               (plist-get start :line)
                                               (plist-get start :character))))
                                    (or
                                     (not (equal file current-file))
                                     (> point-pos pos))))))
                            reversed-diags)))
            (when prev-diag
              (let* ((range (plist-get prev-diag :range))
                     (start (plist-get range :start))
                     (pos (lsp--line-character-to-point
                           (plist-get start :line)
                           (plist-get start :character))))
                (unless (equal file current-file)
                  (find-file file))
                (goto-char pos)
                (message "Jumped to diagnostic: %s" (plist-get prev-diag :message))
                (throw 'done t))))))))

  (defun my/c-clear-string-fences (orig-fun)
    (condition-case nil
        (funcall orig-fun)
      (error
       nil)))
  (advice-add #'c-clear-string-fences :around #'my/c-clear-string-fences)
  (advice-add #'c-restore-string-fences :around #'my/c-clear-string-fences)

  (if (executable-find "emacs-lsp-booster")
      (progn
        (defun lsp-booster--advice-json-parse (old-fn &rest args)
          "Try to parse bytecode instead of json."
          (or
           (when (equal (following-char) ?#)
             (let ((bytecode (read (current-buffer))))
               (when (byte-code-function-p bytecode)
                 (funcall bytecode))))
           (apply old-fn args)))
        (advice-add (if (progn (require 'json)
                               (fboundp 'json-parse-buffer))
                        'json-parse-buffer
                      'json-read)
                    :around
                    #'lsp-booster--advice-json-parse)

        (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
          "Prepend emacs-lsp-booster command to lsp CMD."
          (let ((orig-result (funcall old-fn cmd test?)))
            (if (and (not test?)                             ;; for check lsp-server-present?
                     (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
                     lsp-use-plists
                     (not (functionp 'json-rpc-connection))  ;; native json-rpc
                     (executable-find "emacs-lsp-booster"))
                (progn
                  (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
                    (setcar orig-result command-from-exec-path))
                  (message "Using emacs-lsp-booster for %s!" orig-result)
                  (cons "emacs-lsp-booster" orig-result))
              orig-result)))
        (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))))

(use-package lsp-treemacs
  :ensure t
  :functions (determine-recent-project-root lsp-treemacs--build-error-list)
  :commands lsp-treemacs-errors-list
  :bind (:map lsp-mode-map
         ("C-c e" . lsp-treemacs-errors-list))
  :custom
  (lsp-treemacs-error-list-current-project-only t)
  (lsp-treemacs-error-list-expand-depth nil)
  (lsp-treemacs-error-list-severity 1)
  :config
  (cl-flet*
      ((error-list-advice (oldfun &rest args)
         "Show only diagnostics for the current folder if there are any."

         (if-let* ((root (determine-recent-project-root))
                   (folder-arg (seq-elt args 0))
                   (folder (expand-file-name folder-arg)))
             (when (string-prefix-p root folder)
               (apply oldfun args))
           (apply oldfun args))))

    (advice-add #'lsp-treemacs--build-error-list
                :around
                #'error-list-advice)))

(use-package lsp-java
  :ensure t
  :functions (-flatten -map -uniq dap-register-debug-template
              lsp-find-session-folder lsp-find-workspace lsp-session
              lsp-workspace-shutdown my/lsp-find-session-folder-with-mx
              my/setup-java-workspace-dir)
  :after (lsp-mode treemacs)
  :demand t
  :mode ("\\.java.*\\.class" . java-mode)
  :custom
  (lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/snapshots/jdt-language-server-latest.tar.gz")
  (lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true"))
  (lsp-java-content-provider-preferred "fernflower")
  (lsp-java-save-actions-organize-imports t)
  (lsp-java-format-on-type-enabled nil)
  (lsp-java-format-comments-enabled t)
  (lsp-java-format-enabled t)
  (lsp-java-autobuild-enabled nil)
  (lsp-java-inhibit-message t)
  (lsp-java-import-gradle-enabled nil)
  (lsp-java-completion-import-order ["java" "javax" "org" "com"])
  (lsp-java-import-order ["java" "javax" "org" "com"])
  (dap-java-default-debug-port 8000)
  :config 
  (defun my/lsp-find-session-folder-with-mx (oldfun session file-name)
    (or (funcall oldfun session file-name)
        (funcall oldfun session
                 (replace-regexp-in-string
                  "/mxbuild/\\(jdk[0-9]+/\\)?" "/"
                  file-name))))
  (advice-add #'lsp-find-session-folder :around #'my/lsp-find-session-folder-with-mx)

  (dap-register-debug-template "Java Attach com.oracle.graal.python"
                               (list :type "java"
                                     :request "attach"
                                     :hostName "localhost"
                                     :projectName "com.oracle.graal.python"
                                     :port 8000))

  (defun my/setup-java-workspace-dir (&rest _args)
    (unless (lsp-find-workspace 'jdtls nil)
      (if-let* ((p (project-current))
                (r (project-root p))
                (wsuserdir (expand-file-name ".cache/.jdtls.workspace" r)))
          (unless (equal lsp-java-workspace-dir wsuserdir)
            (->> (lsp-session)
                 (lsp-session-folder->servers)
                 (hash-table-values)
                 (-flatten)
                 (-uniq)
                 (mapc #'lsp-workspace-shutdown))
            (setq lsp--session nil)
            (add-to-list 'desktop-globals-to-save 'lsp-java-workspace-dir)
            (add-to-list 'desktop-globals-to-save 'lsp-java-workspace-cache-dir)
            (add-to-list 'desktop-globals-to-save 'lsp-session-file)
            (setq
             lsp-java-workspace-dir wsuserdir
             lsp-java-workspace-cache-dir (expand-file-name "cache/" lsp-java-workspace-dir)
             lsp-session-file (expand-file-name ".lsp-session-v1" wsuserdir))
            (message (format "Setting Eclipse workspace to %s, session to %s" lsp-java-workspace-dir lsp-session-file))
            (message (format "You may have to adapt %s/.metadata/.plugins/org.eclipse.core.runtime/.settings/org.eclipse.jdt.launching.prefs to give the default VM the name that mx told you" lsp-java-workspace-dir))
            (find-file-noselect (format "%s/.metadata/.plugins/org.eclipse.core.runtime/.settings/org.eclipse.jdt.launching.prefs" lsp-java-workspace-dir))))))
  (advice-add #'lsp :before #'my/setup-java-workspace-dir)

  (add-hook 'java-mode-hook (lambda () (if (lsp-find-workspace 'jdtls nil) (lsp)))))

(use-package dap-mode
  :ensure t
  :commands (dap-debug)
  :config
  (dap-auto-configure-mode t)
  :custom
  (dap-stack-trace-limit 40)
  (dap-auto-configure-features '(sessions locals tooltip))
  (dap-ui-locals-expand-depth nil)
  (dap-print-io nil)
  (dap-auto-show-output t))

(use-package dap-node
  :after dap-mode
  :config (dap-register-debug-template
           "Node Attach 9229"
           (list :type "node"
                 :cwd nil
                 :request "attach"
                 :protocol "auto"
                 :address "127.0.0.1"
                 :stopOnEntry t
                 :port 9229
                 :program ""
                 :name "Node Attach 9229")))

(use-package agent-shell
  :ensure t
  :functions (agent-shell-make-environment-variables
              agent-shell-openai-make-authentication
              agent-shell-opencode-make-authentication
              oca-key
              oca-codex-login
              timfel/agent-shell-command-prefix-bwrap)
  :commands agent-shell
  :pin melpa
  :custom
  (agent-shell-busy-indicator-frames 'dots-round)
  (agent-shell-header-style 'text)
  (agent-shell-buffer-name-format (lambda (_agent-name project-name) (format "%s agent" project-name)))
  (agent-shell-session-strategy 'latest)
  (agent-shell-highlight-blocks nil)
  (agent-shell-prefer-viewport-interaction nil)
  (agent-shell-preferred-agent-config 'codex)
  (agent-shell-show-config-icons nil)
  (agent-shell-show-usage-at-turn-end t)
  (agent-shell-text-file-capabilities nil)
  (agent-shell-command-prefix #'timfel/agent-shell-command-prefix-bwrap)
  :config
  ;; If any .agents/skills from this repo do not exist in $HOME/.agents/skills/ (Unix) or $Env:USERPROFILE/.agents/skills (Windows)
  ;; then symlink them there
  (let* ((repo-root (locate-user-emacs-file ""))
         (skills-src-dir (expand-file-name ".agents/skills" repo-root))
         (skills-dst-dir (expand-file-name ".agents/skills" (or (getenv "USERPROFILE") "~"))))
    (when (file-directory-p skills-src-dir)
      (dolist (src (directory-files-recursively skills-src-dir ".*" t))
        (unless (file-directory-p src)
          (let* ((rel (file-relative-name src skills-src-dir))
                 (dst (expand-file-name rel skills-dst-dir)))
            (let ((src (expand-file-name src))
                  (dst (expand-file-name dst)))
              (when (and (file-exists-p src)
                         (not (file-exists-p dst)))
                (make-directory (file-name-directory dst) t)
                (condition-case _
                    (make-symbolic-link src dst t)
                  (error
                   (condition-case _
                       (copy-file src dst t t t)
                     (error nil)))))))))))

  (setq
   agent-shell-openai-codex-environment (agent-shell-make-environment-variables :inherit-env t)
   agent-shell-openai-authentication (agent-shell-openai-make-authentication :codex-api-key #'oca-codex-login)
   agent-shell-opencode-authentication (agent-shell-opencode-make-authentication :api-key #'oca-key)))

(use-package timfel-agent-shell-unstick
  :after agent-shell)

(use-package agent-shell-attention
  :vc (:url "https://github.com/ultronozm/agent-shell-attention.el" :rev :newest)
  :ensure t
  :after (agent-shell)
  :bind (("C-x a a" . #'agent-shell-attention-dashboard)
         :map agent-shell-attention-dashboard-mode-map
         ("k" . (lambda ()
                  (interactive)
                  (let ((buffer (tabulated-list-get-id)))
                    (if (buffer-live-p buffer)
                        (kill-buffer buffer))))))
  :config
  (agent-shell-attention-mode 1)
  :custom
  (agent-shell-attention-show-zeros t)
  (agent-shell-attention-render-function #'agent-shell-attention-render-active)
  (agent-shell-attention-notify-function
   (lambda (_buffer title body)
     (if (or (eq system-type 'windows-nt) (wsl-p))
         (wsl-powershell-start-process
          "say" nil
          (string-replace "\n" ""
                          (format "$sp = New-Object -ComObject SAPI.SpVoice;
                                   $sp.Volume = 100;
                                   $sp.Rate   = 4;
                                   $sp.Speak(\"
                                   <speak>
                                     <emph><pitch middle='+10'>Check Your Agent! %s %s</pitch></emph>
                                   </speak>
                                   \", 0)" (replace-regexp-in-string "\\<agent\\>" "" title t t) (replace-regexp-in-string "\\<agent\\>" "" body t t)))))
     (knockknock-notify
      :title title
      :message body
      :icon "nf-cod-bot"
      :duration 5))))

(use-package knockknock
  :vc (:url "https://github.com/konrad1977/knockknock" :rev :newest)
  :ensure t
  :custom
  (knockknock-darken-background-percent 30)
  :commands (knockknock-notify))

(use-package difftastic
  :ensure t
  :pin melpa
  :custom (difftastic-executable (locate-user-emacs-file (concat "bin/difft" (if (eq system-type 'windows-nt) ".exe" ""))))
  :commands (difftastic-dired-diff difftastic-magit-diff)
  :config (difftastic-bindings-mode)
  (unless (file-executable-p difftastic-executable)
    "Find, download, and extract the latest binary from https://github.com/Wilfred/difftastic/releases/latest for the current platform into ~/.emacs.d/bin/"
    (cl-flet*
        ((timfel/difftastic--asset-pattern ()
           (pcase system-type
             ('gnu/linux "x86_64-unknown-linux")
             ('windows-nt "x86_64-pc-windows")
             (_ nil)))

         (timfel/difftastic--install-dir ()
           (let ((dir (file-name-directory difftastic-executable)))
             (make-directory dir t)
             dir))

         (timfel/difftastic--download-url ()
           (let* ((api "https://api.github.com/repos/Wilfred/difftastic/releases/latest")
                  (json (with-temp-buffer
                          (url-insert-file-contents api)
                          (goto-char (point-min))
                          (when (re-search-forward "^\r?$" nil t)
                            (delete-region (point-min) (point)))
                          (json-parse-buffer :object-type 'alist :array-type 'list :null-object nil :false-object nil)))
                  (assets (alist-get 'assets json))
                  (pat (timfel/difftastic--asset-pattern)))
             (unless pat
               (error "Unsupported system-type: %s" system-type))
             (or
              (catch 'found
                (dolist (a assets)
                  (let* ((name (alist-get 'name a))
                         (url (alist-get 'browser_download_url a)))
                    (when (and (stringp name)
                               (stringp url)
                               (string-match-p "\\.tar\\.gz\\'\\|\\.zip\\'\\|\\.tgz\\'\\|\\.gz\\'" name)
                               (string-match-p pat name))
                      (throw 'found url)))))
              (error "Could not find difftastic release asset for %s" system-type))))

         (timfel/difftastic--extract (archive destdir)
           (let ((default-directory destdir))
             (cond
              ((string-match-p "\\.zip\\'" archive)
               (let ((unzip (or (executable-find "unzip")
                                (expand-file-name "../../usr/bin/unzip.exe" (executable-find "git.exe")))))
                 (unless (file-executable-p unzip)
                   (error "Need unzip to extract %s" archive))
                 (let ((cmd (format "%s -o %s" (shell-quote-argument unzip) (shell-quote-argument archive))))
                   (unless (= 0 (shell-command cmd))
                     (error "Failed: %s" cmd)))))
              ((string-match-p "\\.tar\\.gz\\'\\|\\.tgz\\'" archive)
               (unless (executable-find "tar")
                 (error "Need tar to extract %s" archive))
               (let ((cmd (format "tar -xzf %s" (shell-quote-argument archive))))
                 (unless (= 0 (shell-command cmd))
                   (error "Failed: %s" cmd))))
              ((string-match-p "\\.gz\\'" archive)
               (unless (executable-find "gzip")
                 (error "Need gzip to extract %s" archive))
               (let* ((cmd (format "gzip -dkf %s" (shell-quote-argument archive))))
                 (unless (= 0 (shell-command cmd))
                   (error "Failed: %s" cmd))))
              (t
               (error "Unknown archive type: %s" archive)))))

         (timfel/difftastic--find-downloaded-exe (dir)
           (or
            (car (seq-filter #'file-executable-p
                             (directory-files-recursively dir "\\`difft\\(\\.exe\\)?\\'" t)))
            (car (seq-filter #'file-executable-p
                             (directory-files-recursively dir "\\`difftastic\\(\\.exe\\)?\\'" t)))
            (car (seq-filter (lambda (f)
                               (and (file-regular-p f)
                                    (file-executable-p f)
                                    (string-match-p "difft\\(\\.exe\\)?\\'" f)))
                             (directory-files-recursively dir "difft\\|difftastic" t))))))

      (let* ((url (timfel/difftastic--download-url))
             (tmpdir (make-temp-file "difftastic" t))
             (archive (expand-file-name (file-name-nondirectory url) tmpdir)))
        (url-copy-file url archive t)
        (timfel/difftastic--extract archive tmpdir)
        (let ((downloaded (timfel/difftastic--find-downloaded-exe tmpdir)))
          (unless downloaded
            (error "Could not locate difftastic binary after extracting %s" archive))
          (make-directory (file-name-directory difftastic-executable) t)
          (copy-file downloaded difftastic-executable t t t)
          (set-file-modes difftastic-executable #o755))))))

(use-package jira
  :ensure t
  :commands (jira-api-get-basic-data jira-api-get-users jira-issues)
  :bind (:map jira-issues-mode-map
              ("C-x a i" . timfel/jira-issues-investigate-marked-with-agent))
  :config
  (add-to-list 'transient-values
               '(jira-issues-menu "--myself" "--resolution=Unresolved"))
  (with-eval-after-load 'jira-issues
    (transient-append-suffix 'jira-issues-actions-menu "W"
      '("a" "Investigate marked issues with agent"
         timfel/jira-issues-investigate-marked-with-agent)))
  :custom
  (jira-issues-max-results 70)
  (jira-token-is-personal-access-token t)
  (jira-users-max-results 50)
  (jira-api-version 2)
  (jira-debug nil))

(use-package org-social
  :ensure t
  :commands org-social-timeline
  :custom
  (org-social-relay "https://relay.org-social.org/"
   org-social-my-public-url "https://host.org-social.org/timfelgentreff/social.org"))

(use-package elfeed
  :ensure t
  :bind (:map elfeed-search-mode-map
         ("g" . elfeed-update)
         ("<mouse-1>" . elfeed-search-show-entry))
  :commands elfeed
  :custom
  (elfeed-feeds
   '("https://www.indieretronews.com/feeds/posts/default?alt=rss"
     "https://xkcd.com/rss.xml"
     "https://www.osnews.com/feed/")))

(use-package custom
  :defines (wl-copy-process)
  :config
  (set-fontset-font
   t
   'emoji
   (cond
    ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
    ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
    ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
    ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")  ; 🧗
    ((member "Symbola" (font-family-list)) "Symbola")))
  (set-fontset-font
   t
   'symbol
   (cond
    ((member "Segoe UI Symbol" (font-family-list)) "Segoe UI Symbol")
    ((member "Apple Symbols" (font-family-list)) "Apple Symbols")
    ((member "Symbola" (font-family-list)) "Symbola")))
  (if (eq system-type 'windows-nt)
      (set-fontset-font t '(#x1F300 . #x1F5FF) "Segoe UI Symbol"))  ; 🔁, Miscellaneous Symbols and Pictographs

  (if (display-graphic-p)
      (run-with-idle-timer 0 nil
			   (lambda ()
			     (if (memq window-system '(x pgtk))
			         (set-face-attribute 'default nil :font "DejaVu Sans Mono-10")
			       (if (eq window-system 'w32)
				   (set-face-attribute 'default nil :family "Consolas" :height 105)
                                 (if (eq system-type 'android)
                                     (set-face-attribute 'default nil :family "Droid Sans Mono" :height 120)))))))

  (when (eq system-type 'android)
    ;; we do not have permissions above our own and some shared folders in
    ;; emacs on android
    (setq locate-dominating-stop-dir-regexp
          (concat locate-dominating-stop-dir-regexp
                  "\\|\\`/data/data/org.gnu.emacs/\\'"
                  "\\|\\`/data/data/com.termux/\\'"
                  "\\|\\`/content/storage/\\'")))

  (when (eq system-type 'gnu/linux)
    (if (or (eq window-system 'pgtk)
	    (and (not window-system) (getenv "WAYLAND_DISPLAY")))
        (progn
	  (setq wl-copy-process nil)
	  (defun wl-copy (text)
	    (setq wl-copy-process (make-process :name "wl-copy"
					        :buffer nil
					        :command '("wl-copy" "-f" "-n")
					        :connection-type 'pipe))
	    (process-send-string wl-copy-process text)
	    (process-send-eof wl-copy-process))
	  (defun wl-paste ()
	    (if (and wl-copy-process (process-live-p wl-copy-process))
	        nil ; should return nil if we're the current paste owner
	      (shell-command-to-string "wl-paste -n | tr -d \r")))
	  (setq interprogram-cut-function 'wl-copy
	        interprogram-paste-function 'wl-paste))))

  (when-let* ((nvm "~/.nvm/versions/node/")
	      (_ (file-exists-p nvm)))
    (add-to-list 'exec-path (string-join (list nvm (car (sort (directory-files nvm) #'string-greaterp)) "bin") "/"))
    (setenv "PATH" (string-join exec-path path-separator)))

  (when-let* ((jdk21 (expand-file-name "~/.mx/jdks/labsjdk-ce-21/"))
	      (_ (and (file-exists-p jdk21) (not (getenv "JAVA_HOME")))))
    (setenv "JAVA_HOME" jdk21))

  (when-let* ((sdkman (getenv "SDKMAN_DIR"))
              (jdk21 (expand-file-name (concat sdkman "candidates/java/21.0.4-oracle")))
	      (_ (and (file-exists-p jdk21) (not (getenv "JAVA_HOME")))))
    (setenv "JAVA_HOME" jdk21))

  (when-let* ((eclipse (expand-file-name "~/dev/eclipse/eclipse"))
	      (_ (and (file-exists-p eclipse) (not (getenv "ECLIPSE_EXE")))))
    (setenv "ECLIPSE_EXE" eclipse))

  ;; 100 MiB
  (setq gc-cons-threshold (* 1024 1024 100)))
