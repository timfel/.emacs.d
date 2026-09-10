;;; -*- lexical-binding: t -*-
(require 'use-package)

(use-package request ;; has not had a release in ages, but bugfixes on master
  :ensure t
  :defer t
  :pin melpa)

(use-package wsl-interop
  :unless (eq system-type 'android)
  :vc (:url "https://github.com/timfel/wsl-interop.el" :branch "main" :rev :newest)
  :commands (wsl-p
             wsl-powershell-command wsl-powershell-command-to-string
             wsl-cmd-command wsl-cmd-command-to-string
             wsl-powershell-async-command wsl-powershell-start-process
             wsl-cmd-async-command wsl-cmd-start-process))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml$" "\\.yaml$" "Gemfile.lock$"))

(use-package markdown-mode
  :ensure t
  :unless (>= emacs-major-version 31)
  :mode ("\\.md$")
  :config
  (setq markdown-command "cmark-gfm --extension table"))

(use-package markdown-ts-mode
  :if (>= emacs-major-version 31)
  :ensure nil
  :mode ("\\.md$")
  :defer t)

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
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp")))
  ;; (defun ruby-accurate-end-of-block (&optional end)
  ;;   "(tfel): Fixes an issue I had with ruby-mode."
  ;;   (let (state
  ;;         (end (or end (point-max))))
  ;;     (while (and (setq state (apply 'ruby-parse-partial end state))
  ;;                 (nth 2 state) (>= (nth 2 state) 0) (< (point) end)))))
  )

(use-package robe
  :ensure t
  :hook ((ruby-mode . robe-mode)
         (ruby-ts-mode . robe-mode)))

(use-package rspec-mode
  :ensure t
  :hook ((ruby-mode . rspec-mode)
         (ruby-ts-mode . rspec-mode)))

(use-package inf-ruby
  :ensure t
  ;; ((ruby-mode . ((inf-ruby-wrapper-command . "docker exec -i container_name %s"))))
  :commands (inf-ruby inf-ruby-console-auto))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :ensure t
  :commands (dockerfile-mode))

(use-package docker-compose-mode
  :ensure t
  :disabled
  :unless (eq system-type 'android)
  :commands (docker-compose-mode))

(use-package web-mode
  :ensure t
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)))

(use-package org-tree-slide
  :after org
  :ensure t
  :functions (org-fold-show-all
              org-tree-slide-mode
              org-tree-slide-move-next-tree
              org-tree-slide-move-previous-tree
              org-tree-slide-content)
  :defines (org-tree-slide-mode-map)
  :commands org-tree-slide-mode
  :config
  (setq org-tree-slide-header t)
  (setq org-tree-slide-slide-in-effect (display-graphic-p))
  (setq org-tree-slide-heading-emphasis nil)
  (setq org-tree-slide-cursor-init nil)
  (setq org-tree-slide-modeline-display 'outside)
  (setq org-tree-slide-skip-done nil)
  (setq org-tree-slide-skip-comments t)
  :bind (:map org-mode-map
         ("<f5>" . org-tree-slide-mode)
         :map org-tree-slide-mode-map
         ("<f5>" . (lambda ()
                     (interactive)
                     (org-tree-slide-mode 0)
                     (org-fold-show-all)))
         ("C-<f5>" . org-tree-slide-content)
         ("C-<right>" . org-tree-slide-move-next-tree)
         ("C-<left>" . org-tree-slide-move-previous-tree)))

(use-package ox-gfm
  :ensure t
  :after org)

(use-package org-download
  :ensure t
  :after org
  :pin melpa
  :commands org-download-screenshot
  :custom
  (org-download-image-org-width 200)
  (org-download-screenshot-method (if (eq system-type 'windows-nt)
                                      (format "%s %s"
                                              (executable-find "python3")
                                              (expand-file-name "~/dotfiles/bin/wslscr.py %s"))
                                    (expand-file-name "~/bin/wslscr.py %s")))
  (org-download-image-dir "./Screenshots/"))

(use-package ox-pandoc
  :ensure t
  :after org
  :commands (org-pandoc-export-to-pptx
             org-pandoc-export-to-pptx-and-open)
  :custom
  ;; Not, pandoc picks an appropriate template slide from the slide master in the reference file
  ;; pandoc looks for the standard English name of the template slide
  ;;   Title Slide
  ;;   Title and Content
  ;;   Section Header
  ;;   Two Content
  ;;   Content with Caption
  ;;   Blank
  ;;   Comparison
  (org-pandoc-options-for-pptx
   `((reference-doc . ,(expand-file-name "Graal Template.pptx" timfel/gist-location))
     (slide-level . 2))))

(use-package company
  :ensure t
  :bind (("M-?" . company-complete))
  :config (global-company-mode t)
  :custom
  (company-dabbrev-code-everywhere t)
  ;; compat-31 defines all as a function; company-dabbrev checks functions
  ;; before the literal all sentinel.
  (company-dabbrev-other-buffers (lambda (_) (quote all)))
  (company-dabbrev-ignore-case 'keep-prefix)
  (company-dabbrev-downcase 0)
  (company-idle-delay (if (eq system-type 'windows-nt) 10 0.2)))

(use-package magit
  :unless (memq system-type '(windows-nt android))
  :bind (("C-x C-z" . magit-status)
         :map magit-mode-map
         ("C-x a s" . timfel-agent-start))
  :ensure t
  :preface
  (setq magit-diff-specify-hunk-foreground nil)
  :custom
  (magit-diff-use-indicator-faces t)
  (magit-auto-revert-tracked-only t)
  (magit-process-apply-ansi-colors t)
  (magit-diff-fontify-hunk t)
  (magit-diff-use-indicator-faces t)
  :config
  (defvar-keymap timfel/magit-ctl-x-a-map
    :doc "Prefix map for `C-x a' in Magit diff sections."
    "s" #'timfel-agent-start
    "a" #'magit-add-change-log-entry
    "4 a" #'magit-add-change-log-entry-other-window)
  (keymap-set magit-diff-section-map "C-x a" timfel/magit-ctl-x-a-map)
  (magit-auto-revert-mode))

(use-package mmm-mode
  :ensure t
  :demand t
  :after (java-ts-mode typescript-ts-mode js)
  :functions (mmm-add-mode-ext-class)
  :commands (mmm-parse-buffer)
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
            (mmm-add-mode-ext-class 'java-ts-mode "\\.java$" 'java-text-block)
            (mmm-add-mode-ext-class 'markdown-mode "\\.md$" 'md-javascript-block)
            (mmm-add-mode-ext-class 'markdown-ts-mode "\\.md$" 'md-javascript-block)
            (mmm-add-mode-ext-class 'markdown-mode "\\.md$" 'md-github-code-block)))

(use-package tramp-rpc
  :disabled
  :after tramp
  :vc (:url "https://github.com/ArthurHeymans/emacs-tramp-rpc"
       :rev :newest
       :lisp-dir "lisp"))

(use-package dumb-jump
  :ensure t
  :defer 10
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package jsonnet-mode
  :ensure t
  :mode ("\\.jsonnet$"))

(use-package cmake-mode
  :if (< emacs-major-version 31)
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


(use-package typescript-mode
  :mode "\\.ts\\'"
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :defer 5
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

(use-package exec-path-from-powershell
  :vc (:url "https://github.com/timfel/exec-path-from-powershell"
            :rev :newest
            :branch "main"
            :ignored-files ("scripts/*" "test/*"))
  :if (eq system-type 'windows-nt)
  :defer t)

(use-package rustic
  :ensure t
  :mode ("\\.rs$" . rustic-mode))

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("C-S-d" . mc/mark-more-like-this-extended)))


(defun timfel/side-shell-toggle (&optional arg)
  (interactive "P")
  (require 'ghostel)
  (defvar ghostel-buffer-name)
  (cl-flet ((hide ()
              (let ((parent (window-parent)))
                (if parent
                    (delete-window)
                  (set-window-dedicated-p (selected-window) nil)
                  (pop-to-buffer-same-window nil)))))
    (if (and (not arg)
             (derived-mode-p '(ghostel-mode)))
        ;; if we are inside a shell, hide it
        (hide)
      ;; find our prefix, a new name if we need it later, and get the side window
      (let* ((prefix "*side-term")
             (side-buffer-name (generate-new-buffer-name (format "%s*" prefix)))
             (w (or (get-window-with-predicate (lambda (w) (string-prefix-p prefix (buffer-name (window-buffer w)))))
                    (split-window (selected-window) -18))))
        ;; activate the side window
        (select-window w)
        (set-window-dedicated-p w nil)
        ;; get or create a new shell buffer
        (let ((buf
               (or
                (when (not arg)
                  (seq-find (lambda (b) (string-prefix-p prefix (or (buffer-name b) ""))) (buffer-list)))
                (let ((ghostel-buffer-name side-buffer-name))
                  (ghostel (not (null arg)))))))
          ;; put it in the side window, make that window now dedicated to that buffer
          (pop-to-buffer-same-window buf)
          (add-hook 'kill-buffer-hook (lambda () (hide)) 0 t)
          (set-window-dedicated-p w t))))))

(defun timfel/side-shell-select ()
  (interactive)
  (let* ((keys (this-command-keys-vector))
         (last-key (aref keys (1- (length keys))))
         (sorter (if (eq last-key 'left) #'string-greaterp #'string-lessp))
         (blfiltered (seq-filter (lambda (b) (string-prefix-p "*side-term" (or (buffer-name b) ""))) (buffer-list)))
         (bl (seq-sort (lambda (a b) (funcall sorter (buffer-name a) (buffer-name b))) blfiltered))
         (before (seq-take-while (lambda (b) (not (eq b (current-buffer)))) bl))
         (after (seq-difference (seq-difference bl before) (list (current-buffer)))))
    (when (or before after)
      (set-window-dedicated-p (selected-window) nil)
      (set-window-buffer (selected-window) (or (seq-first after) (seq-first before)))
      (set-window-dedicated-p (selected-window) t))))

(use-package ghostel
  :ensure t
  :defines (ghostel-mode-map ghostel-semi-char-mode-map ghostel-char-mode-map)
  :commands (ghostel)
  :bind (("<f12>" . #'timfel/side-shell-toggle)
         :map ghostel-mode-map
         ("C-c C-<left>" . #'timfel/side-shell-select)
         ("C-c C-<right>" . #'timfel/side-shell-select)
         ("<f12>" . #'timfel/side-shell-toggle)
         :map ghostel-semi-char-mode-map
         ("<f12>" . #'timfel/side-shell-toggle)
         :map ghostel-char-mode-map
         ("<f12>" . #'timfel/side-shell-toggle))
  :config
  (when (and (eq system-type 'windows-nt)
             (eq (type-of ghostel-shell) 'string)
             (string-suffix-p "cmdproxy.exe" ghostel-shell))
    (setq ghostel-shell (list ghostel-shell "/C" "powershell")))
  :custom
  (ghostel-max-scrollback (* 50 1024 1024)))

(use-package dape
  :ensure t
  :commands dape
  :config
  ;; put require "debug"; binding.break for the starting point in a test block,
  ;; then run M-x dape and select rdbg-rails-test
  (cl-flet* ((merge-plists (&rest plists)
               (let ((rtn (copy-sequence (pop plists)))
                     p v ls)
                 (while plists
                   (setq ls (pop plists))
                   (while ls
                     (setq p (pop ls) v (pop ls))
                     (setq rtn (plist-put rtn p v))))
                 rtn))
             (dape-override-config (configs provider override)
               (let* ((provider-config (assoc provider configs))
                      (config-plist (cdr provider-config)))
                 (if provider-config
                     (merge-plists config-plist override)
                   (error "Provider %s not found in configs" provider)))))
    (let* ((config (dape-override-config
                    dape-configs
                    'rdbg
                    '(-c (format "RAILS_ENV=test bundle exec bin/rails test %s:%d"
                                 (dape-buffer-default)
                                 (line-number-at-pos))))))
      (cl-pushnew `(rdbg-rails-test . ,config) dape-configs))))

(use-package yasnippet
  :ensure t
  :after (lsp-mode)
  :hook ((lsp-mode . yas-minor-mode)))

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
  :functions (jira-actions-copy-issues-id-to-clipboard
              jira-api--get-current-url
              jira-detail-show-issue
              jira-utils-marked-item
              org-link-set-parameters
              org-link-store-props
              timfel/org-store-jira-link)
  :defines (jira-detail--current-key)
  :commands (jira-api-get-basic-data jira-api-get-users jira-issues)
  :bind (:map jira-detail-mode-map
              ("c" . (lambda (&optional prefix)
                       (interactive "P")
                       (when-let* ((key jira-detail--current-key))
                         (jira-actions-copy-issues-id-to-clipboard
                          (if prefix
                              key
                            (concat (jira-api--get-current-url) "/browse/" key))))))
         :map jira-detail-mode-map
              ("C-x a i" . agent-shell-jira-issues-investigate-marked-with-agent)
         :map jira-issues-mode-map
              ("c" . (lambda (&optional prefix)
                       (interactive "P")
                       (when-let* ((key (jira-utils-marked-item)))
                         (jira-actions-copy-issues-id-to-clipboard
                          (if prefix
                              key
                            (concat (jira-api--get-current-url) "/browse/" key))))))
              ("C-x a i" . agent-shell-jira-issues-investigate-marked-with-agent))
  :config
  (add-to-list 'transient-values
               '(jira-issues-menu "--myself" "--resolution=Unresolved"))
  (with-eval-after-load 'jira-issues
    (transient-append-suffix 'jira-issues-actions-menu "W"
      '("a" "Investigate marked issues with agent"
        agent-shell-jira-issues-investigate-marked-with-agent)))

  (with-eval-after-load 'org
    (defun timfel/org-store-jira-link (&optional _interactive?)
      (when-let* ((_ (or (derived-mode-p 'jira-detail-mode)
                        (derived-mode-p 'jira-issues-mode)))
                 (key (concat "jira:" (jira-utils-marked-item))))
        (org-link-store-props :type "jira" :link key :description key)
        key))
    (org-link-set-parameters
     "jira"
     :follow #'jira-detail-show-issue
     :store #'timfel/org-store-jira-link))

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
  (org-social-relay "https://relay.org-social.org/")
  (org-social-my-public-url "https://host.org-social.org/timfelgentreff/social.org"))

(use-package codespaces
  :ensure t
  :commands (codespaces-setup))

(use-package mise
  :ensure t
  :unless (eq system-type 'android)
  :hook
  (after-init . global-mise-mode)
  :custom
  (mise-update-on-eshell-directory-change t))

(use-package verb
  :ensure t
  :commands (verb-mode))
