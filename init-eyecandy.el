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
  :hook ((completion-list-mode . hide-mode-line-mode)))

(use-package zone-rainbow
  :ensure t
  :after zone
  :config
  (setq zone-programs (vconcat [zone-rainbow] zone-programs)))

(use-package org-modern
  :ensure t
  :after org
  :demand t
  :custom
  (org-modern-hide-stars "")
  (org-modern-star 'replace)
  (org-modern-replace-stars "▶▷▹●◉○◌◆◈◇✳⋅")
  :hook
  (org-mode . (lambda ()
                (unless (eq system-type 'android)
                  (setq line-spacing '(0.1 . 0.1))
                  (setq-local left-margin-width 8)
                  (setq-local right-margin-width 12))))
  (org-agenda-mode . (lambda ()
                       (unless (eq system-type 'android)
                         (setq line-spacing '(0.1 . 0.1)))))
  :config
  (seq-do
   (lambda (i)
     (set-face-attribute (intern (format "org-level-%d" i))
                         nil :height (/ (- 28 i) 20.0)))
   (number-sequence 1 8))
  (global-org-modern-mode 1))

(use-package timeout
  :ensure t
  :vc (:url "https://github.com/karthink/timeout.git" :branch "master" :rev :newest))

(use-package dslide
  :after org-modern
  :commands (dslide-deck-start dslide-deck-present)
  :ensure t
  :defines (my-dslide-slide-width my-dslide-slide-height)
  :init
  (setq my-dslide-slide-width 80)
  (setq my-dslide-slide-height 30)
  :bind (:map org-mode-map
         ("<f5>" . dslide-deck-present)
         :map dslide-mode-map
         ("<volume-up>" . dslide-deck-start)
         ("<volume-down>" . dslide-deck-stop)
         ([touchscreen-scroll] . (lambda (event)
                                    (interactive "e")
                                    (let ((dx (nth 2 event)))
                                      (if (> dx 0)
                                          (dslide-deck-forward)
                                        (dslide-deck-backward))))))
  :custom
  (dslide-breadcrumb-separator " ▻ ")
  (dslide-present-frame-parameters '((fullscreen . fullboth)))
  (dslide-slide-in-effect nil)
  :config
  (customize-set-variable 'dslide-default-actions (seq-remove (lambda (e) (eq e 'dslide-action-babel)) dslide-default-actions))
  (when (eq system-type 'android)
    (timeout-throttle #'dslide-deck-forward 2)
    (timeout-throttle #'dslide-deck-backward 2))
  :hook
  (dslide-develop
   . (lambda ()
       (setq-local fill-column my-dslide-slide-width
                   display-fill-column-indicator-column my-dslide-slide-width)
       (display-fill-column-indicator-mode 1)
       (let ((horizontal-line
              (lambda ()
                (defvar-local timfel/dslide-develop--horizontal-lines nil)
                (mapc #'delete-overlay timfel/dslide-develop--horizontal-lines)
                (setq timfel/dslide-develop--horizontal-lines nil)
                (save-excursion
                  (goto-char (point-min))
                  (when (zerop (forward-line
                                (- my-dslide-slide-height
                                   6 ;; space, title/author/mail, breadcrumb, space
                                   )))
                    (let ((overlay (make-overlay (point) (point) nil t t)))
                      (overlay-put overlay 'before-string (make-separator-line my-dslide-slide-width))
                      (push overlay timfel/dslide-develop--horizontal-lines)))))))
         (funcall horizontal-line)
         (add-hook 'dslide-narrow-hook horizontal-line nil t))))
  (dslide-present
   . (lambda ()
       (when (display-graphic-p)
         (let* ((frame (selected-frame))
                (buffer (current-buffer))
                ;; Use the window actually displaying the slide buffer; the
                ;; selected window can still refer to the source window while
                ;; dslide is finishing frame setup.
                (window (get-buffer-window buffer frame))
                ;; Use the monitor containing the presentation frame rather
                ;; than the dimensions of the frame before it is fullscreen.
                (geometry (cdr (assq 'geometry
                                     (frame-monitor-attributes frame))))
                (display-width (or (nth 2 geometry)
                                   (display-pixel-width frame)))
                (display-height (or (nth 3 geometry)
                                    (display-pixel-height frame)))
                ;; Leave room for a two-face-height border on every side
                ;; and 0.3 face-heights of line spacing per line.
                (line-spacing-total 0.3)
                ;; DISPLAY-HEIGHT = my-dslide-slide-height * 1.3 line heights + 4 border heights.
                (target-line-height
                 (max 1
                      (floor
                       (/ display-height
                          (+ (* my-dslide-slide-height (+ 1.0 line-spacing-total)) 4.0)))))
                (old-line-height (max 1 (frame-char-height frame)))
                (scale (/ (float target-line-height) old-line-height)))
           (set-frame-parameter frame 'fullscreen 'fullboth)
           ;; refresh image sizing
           (add-hook 'dslide-narrow-hook #'org-link-preview-refresh nil t)
           (org-link-preview-refresh)
           ;; clean look with room to breathe
           (setq-local mode-line-format nil
                       header-line-format nil
                       line-spacing (cons (/ line-spacing-total 2) (/ line-spacing-total 2)))
           (let ((frame-inhibit-implied-resize t))
             (set-frame-parameter frame 'menu-bar-lines 0)
             (set-frame-parameter frame 'tool-bar-lines 0)
             (set-frame-parameter frame 'tab-bar-lines 0)
             ;; Resolve all face heights before changing any of them, so faces
             ;; inheriting from `default' do not get scaled twice.
             (dolist (face-height
                      (mapcar (lambda (face)
                                (cons face
                                      (face-attribute face :height frame
                                                      'default)))
                              ;; (face-list)
                              '(default)))
               (when (numberp (cdr face-height))
                 (set-face-attribute
                  (car face-height) frame :height
                  (max 1 (round (* (cdr face-height) scale))))))
             (set-frame-parameter
              frame 'internal-border-width (* 2 (frame-char-height frame))))
           (set-window-fringes window 0 0 t t)
           (internal-show-cursor window nil)
           (set-face-attribute 'org-verse frame :height 1.5)
           (set-face-attribute 'org-block-begin-line frame :foreground (face-attribute 'org-block :background))
           (set-face-attribute 'org-block-end-line frame :foreground (face-attribute 'org-block :background))
           ;; Margins are specified in character cells.  Calculate them from
           ;; the actual width of an `m' in the now-scaled default face.
           (let* ((fringes (window-fringes window))
                  (text-width
                   (- display-width
                      (* 2 (frame-parameter frame 'internal-border-width))
                      (or (nth 0 fringes) 0)
                      (or (nth 1 fringes) 0)))
                  (m-width (max 1 (string-pixel-width "m" (current-buffer))))
                  (margin-width
                   (floor (/ (max 0 (- text-width (* my-dslide-slide-width m-width)))
                             (* 2 m-width)))))
             (setq-local left-margin-width margin-width
                         right-margin-width margin-width)
             (set-window-margins window margin-width margin-width)
             (force-window-update window)
             ;; Reapply the margins when the fullscreen resize is processed.
             ;; This is the same point at which Emacs normally applies
             ;; buffer-local margin settings after switching buffers.
             (add-hook
              'window-size-change-functions
              (lambda (changed-window)
                (when (and (frame-live-p frame)
                           (window-live-p changed-window)
                           (eq (window-frame changed-window) frame)
                           (eq (window-buffer changed-window) buffer))
                  (set-window-margins changed-window
                                       margin-width margin-width)))
              nil t)
             ;; The fullscreen resize can arrive after this hook returns too.
             (run-at-time
              0 nil
              (lambda ()
                (when (and (frame-live-p frame)
                           (window-live-p window)
                           (eq (window-buffer window) buffer))
                  ;; Re-run the buffer-local margin initialization that a
                  ;; buffer switch would normally trigger.
                  (set-window-buffer window buffer)
                  (set-window-margins window margin-width margin-width)
                  (force-window-update window))))))))))
