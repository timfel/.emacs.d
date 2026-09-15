;;; -*- lexical-binding: t -*-
(require 'use-package)

(use-package all-the-icons
  :unless (memq system-type '(android windows-nt))
  :ensure t)

(use-package nerd-icons
  :unless (memq system-type '(android windows-nt))
  :ensure t)

(use-package all-the-icons-completion
  :unless (memq system-type '(android windows-nt))
  :after all-the-icons
  :ensure t
  :config (all-the-icons-completion-mode))

(use-package all-the-icons-dired
  :unless (memq system-type '(android windows-nt))
  :after all-the-icons
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package doom-modeline
  ;; remember to run (all-the-icons-install-fonts) manually some time
  :unless (memq system-type '(android windows-nt))
  :after all-the-icons
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-time t)
  (doom-modeline-time-icon nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-bar-width 4)
  (doom-modeline-hud nil)
  (doom-modeline-vcs-max-length 28)
  (doom-modeline-vcs-icon nil)
  (doom-modeline-lsp t)
  (doom-modeline-buffer-file-name-style 'truncate-all)
  :custom-face
  (doom-modeline-buffer-file ((t (:foreground "black" :weight bold))))
  (doom-modeline-buffer-modified ((t (:foreground "#444" :weight bold)))))

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
  :unless (or (daemonp) (display-graphic-p))
  :functions (treemacs-load-theme)
  :after treemacs
  :ensure t
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package emojify
  :ensure t
  :commands emojify-insert-emoji
  :custom
  (emojify-display-style 'unicode)
  (emojify-emoji-styles '(unicode)))

(use-package emacs-theme-detection
  :ensure t
  :defer t
  :unless (eq system-type 'android)
  :functions (emacs-theme-detection-is-dark emacs-theme-detection-is-light)
  :init
  (autoload #'emacs-theme-detection-is-dark "emacs-theme-detection")
  (autoload #'emacs-theme-detection-is-light "emacs-theme-detection")
  :vc (:url "https://github.com/timfel/emacs-theme-detection.git" :branch "main" :rev :newest))

(use-package hide-mode-line
  :ensure t
  :hook ((completion-list-mode . hide-mode-line-mode)
         (org-tree-slide-mode . hide-mode-line-mode)))

(use-package zone-rainbow
  :ensure t
  :after zone
  :config
  (setq zone-programs (vconcat [zone-rainbow] zone-programs)))

(use-package org-modern
  :ensure t
  :after org
  :hook
  (org-mode . (lambda ()
                (setq line-spacing '(0.1 . 0.1))))
  (org-agenda-mode . (lambda ()
                       (setq line-spacing '(0.1 . 0.1))))
  :config
  ;; Android ships the Noto symbol font as split subsets under one family
  ;; name.  The subset selected by Emacs does not contain all of
  ;; org-modern's folding glyphs (notably U+2BC6, U+25BF and U+25BE), so use
  ;; ASCII indicators there rather than displaying glyphless boxes.
  (when (eq system-type 'android)
    (setq org-modern-fold-stars
          '((">" . "v")
            ("+" . "-")
            (">" . "v")
            ("+" . "-")
            (">" . "v"))))
  (global-org-modern-mode 1))
