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
      '(("melpa" . 10)
        ("nongnu" . 5)
        ("gnu" . 5)
        ("melpa-stable" . 1)
        ("cselpa" . 0)))
(setq package-install-upgrade-built-in nil)
(package-initialize)

(require 'use-package)

;; (setq use-package-compute-statistics t)
;; (require 'profiler)
;; (profiler-start 'cpu)
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (profiler-stop)
;;             (profiler-report)
;;             (use-package-report)))
;; (setq use-package-verbose t)

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
         ("C-x C-M-2" . (lambda ()
                          (interactive)
                          (set-face-attribute 'default nil
                                              :height (* 2 (face-attribute 'default :height)))))
         ("M-]" . forward-list)
         ("C-M-]" . backward-list)
         ("C-z" . (lambda () (interactive) (beep))))
  :custom
  (browse-url-browser-function
   (lambda (url &rest args)
     (if (and (not (string-match-p
                    (rx (or "mailto:"
                            "github.com"
                            "jira"
                            "bitbucket"
                            ".google.com"
                            ".office.com"
                            ".slack.com"))
                    url))
              (y-or-n-p "Browse with EWW? "))
         (apply #'eww-browse-url url args)
       (cond
        ((eq system-type 'windows-nt)
         (apply #'browse-url-default-browser url args))
        ((eq system-type 'android)
         (apply #'browse-url-default-android-browser url args))
        (t
         (setq browse-url-generic-program (or (executable-find "wslview") "xdg-open"))
         (apply #'browse-url-generic url args))))))
  (user-full-name "Tim Felgentreff")
  (user-mail-address "timfelgentreff@gmail.com")
  (send-mail-function 'mailclient-send-it)
  (custom-file (file-name-concat user-emacs-directory "emacs-custom.el"))
  (confirm-kill-emacs 'yes-or-no-p)
  (visible-bell nil)
  (ring-bell-function #'ignore)
  (delete-by-moving-to-trash (eq system-type 'android))
  (make-backup-files nil)
  (query-replace-highlight t)
  (search-highlight t)
  (select-active-regions nil)
  (select-enable-clipboard 't)
  (select-enable-primary nil)
  (font-lock-maximum-decoration t)
  (require-final-newline t)
  (show-paren-style 'mixed)
  (inhibit-startup-screen t)
  (default-directory "~/")
  (view-lossage-auto-refresh t) ; EMACS-31: live-updating C-h l, great for teaching/debugging
  (initial-scratch-message ";; Welcome to your emacs.
;; Some useful expressions:
;;   Get all fonts: (all-the-icons-install-fonts) (nerd-icons-install-fonts)
;;   Resume desktop
;;     (call-interactively (quote desktop-change-dir))
;;     (desktop-save-mode 1)
;;   Last bookmarks: `\\[bookmark-bmenu-list]`
;;   Agenda: 'C-c a a'
")
  (kill-ring-deindent-mode 1)
  (editorconfig-mode 1)
  (comment-multi-line t)
  (comment-style 'extra-line)
  (sentence-end-double-space nil)
  (use-short-answers t)
  (fill-column 79)
  (buffer-file-coding-system 'utf-8-unix)
  (treesit-auto-install-grammar (and (not (equal system-type 'windows-nt)) 'always))
  (treesit-enabled-modes (not (equal system-type 'windows-nt)))
  :config
  (if (file-exists-p custom-file)
      (load custom-file))
  (add-to-list 'save-some-buffers-action-alist
	       (list "d"
		     (lambda (buffer)
		       (diff-buffer-with-file (buffer-file-name buffer)))
		     "show diff between the buffer and its file"))
  (setq read-process-output-max (* 1024 1024))
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (modifier-bar-mode 0)
  (recentf-mode t)
  (show-paren-mode t)
  (blink-cursor-mode 0)
  (scroll-bar-mode 0)
  (column-number-mode t)
  (windmove-default-keybindings)
  (tab-bar-history-mode 1)
  (undelete-frame-mode 1)
  (winner-mode 1)
  (display-time-mode 1)
  (put 'narrow-to-region 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil)
  (setq-default indent-tabs-mode nil))

;; builtin packages i customize and use
(load (locate-user-emacs-file "./init-core.el"))
;; lisp files in my .emacs.d
(load (locate-user-emacs-file "./init-lisp.el"))
;; agent frontend. `agent-shell' uses ACP, `agent-tui' uses term
(unless (eq system-type 'android)
  (load (locate-user-emacs-file "./init-agent-shell.el")))
;; (load (locate-user-emacs-file "./init-agent-tui.el")))
;; packages that make things prettier, but not really more functional
(load (locate-user-emacs-file "./init-eyecandy.el"))
;; packages that make emacs more usable in the terminal
(load (locate-user-emacs-file "./init-terminal.el"))
;; extra packages for programming and working
(load (locate-user-emacs-file "./init-extras.el"))
;; gptel and my config files around it
(load (locate-user-emacs-file "./init-gptel.el"))
;; lsp mode and associated packages
;; (load (locate-user-emacs-file "./init-lsp.el"))
;; extra packages around eglot
(load (locate-user-emacs-file "./init-eglot.el"))

(use-package custom
  :functions (timfel/set-frame-faces)
  :config
  (load-theme 'modus-operandi-tinted)

  ;; Use dedicated fallback fonts for Unicode symbols and emoji.  In
  ;; particular, do not map the emoji pictograph range to Segoe UI Symbol:
  ;; that font only covers part of the range and can mask better fallbacks.
  (when-let* ((emoji-font
               (seq-find (lambda (font) (member font (font-family-list)))
                         '("Apple Color Emoji"
                           "Noto Color Emoji"
                           "Noto Emoji"
                           "Segoe UI Emoji"
                           "Symbola"))))
    (set-fontset-font t 'emoji emoji-font))
  (when-let* ((symbol-font
               (seq-find (lambda (font) (member font (font-family-list)))
                         '("Segoe UI Symbol"
                           "Apple Symbols"
                           "Symbola"))))
    (set-fontset-font t 'symbol symbol-font))

  (defun timfel/set-frame-faces ()
    (cond
     ((memq window-system '(x pgtk))
      (set-face-attribute 'default nil :font "DejaVu Sans Mono-10"))
     ((eq window-system 'w32)
      (set-face-attribute 'default nil :family "Consolas" :height 105))
     ((eq system-type 'android)
      (set-face-attribute 'default nil :family "Droid Sans Mono" :height 120))))

  (cond
   ((display-graphic-p) (run-with-idle-timer 0 nil #'timfel/set-frame-faces))
   ((daemonp) (add-hook 'server-after-make-frame-hook #'timfel/set-frame-faces)))

  (setq safe-local-variable-values
        (append safe-local-variable-values
                '((eval ignore-errors (require 'emacs-ci))
                  (eval ignore-errors (require 'agent-shell))
                  (eval ignore-errors (require 'jira))
                  (smie-indent-basic . 4)
                  (jsonnet-indent-level . 4)
                  (flycheck-disabled-checkers emacs-lisp-checkdoc))))

  (when (eq system-type 'gnu/linux)
    (when (getenv "WAYLAND_DISPLAY")
      ;; define a couple of methods to dispatch to when OSC-52 copy and/or
      ;; paste is not supported, but we have a wayland display
      (cl-defmethod gui-backend-get-selection
        (type data-type
              &context (window-system nil)
              ((terminal-parameter nil 'xterm--get-selection) (eql nil)))
	(if (and (eq type 'CLIPBOARD) (eq data-type 'STRING))
            (let* ((raw (shell-command-to-string "wl-paste -n 2>/dev/null | tr -d '\r'")))
              (when (and raw (not (string-empty-p raw))) raw))))
      (defvar wl-copy-process)
      (setq wl-copy-process nil)
      (cl-defmethod gui-backend-set-selection
        (type data
              &context (window-system nil)
              ((terminal-parameter nil 'xterm--set-selection) (eql nil)))
        (when wl-copy-process
          (ignore-errors
	    (kill-process wl-copy-process)))
	(if (eq type 'STRING)
            (setq wl-copy-process
		  (make-process :name "wl-copy"
				:buffer nil
				:command `("wl-copy" "-f" "-n" "-t" "text/plain" ,data)))))))

    (when-let* ((nvm "~/.nvm/versions/node/")
                (_ (file-exists-p nvm)))
      (add-to-list 'exec-path (string-join (list nvm (car (sort (directory-files nvm) #'string-greaterp)) "bin") "/"))
      (setenv "PATH" (string-join exec-path path-separator)))

    (when-let* ((rb "~/.rbenv/versions/3.3.1/bin/")
                (_ (file-exists-p rb)))
      (add-to-list 'exec-path rb)
      (setenv "PATH" (string-join exec-path path-separator)))

    (when-let* ((sdkman (getenv "SDKMAN_DIR"))
                (jdk21 (expand-file-name (concat sdkman "candidates/java/21.0.4-oracle")))
                (_ (and (file-exists-p jdk21) (not (getenv "JAVA_HOME")))))
      (setenv "JAVA_HOME" jdk21))

    (when-let* ((eclipse (expand-file-name "~/dev/eclipse/eclipse"))
                (_ (and (file-exists-p eclipse) (not (getenv "ECLIPSE_EXE")))))
      (setenv "ECLIPSE_EXE" eclipse))

  (server-start nil t)

  ;; 100 MiB
  (setq gc-cons-threshold (* 1024 1024 100)))
