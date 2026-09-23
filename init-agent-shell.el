;;; -*- lexical-binding: t -*-

(require 'use-package)

(use-package agent-shell-bookmark
  :vc (:url "https://github.com/dcluna/agent-shell-bookmark" :branch "main" :rev :newest)
  :after agent-shell
  :config
  (require 'agent-shell-ol))

(use-package agent-shell-utils
  :after agent-shell
  :demand t
  :vc (:url "https://github.com/timfel/agent-shell-utils.el.git" :branch "master" :rev :newest))

(use-package agent-shell-slack
  :after agent-shell
  :commands agent-shell-slack
  :vc (:url "https://github.com/timfel/agent-shell-slack.git"
            :branch "master"
            :rev :newest
            :ignored-files ("tests/*")))

(use-package agent-shell-bwrap
  :after agent-shell-utils
  :if (not (eq system-type 'windows-nt))
  :config
  (agent-shell-bwrap-mode 1))

(use-package agent-shell-context
  :after agent-shell-utils
  :if (not (eq system-type 'windows-nt))
  :config
  (agent-shell-context-mode 1))

(use-package agent-shell-fanout
  :functions (agent-shell-fanout-default-repositories
              timfel/agent-shell-fanout-graal-repositories)
  :custom
  (agent-shell-fanout-adjacent-repository-names
   '("graal" "graal-enterprise"))
  :config
  (defun timfel/agent-shell-fanout-graal-repositories (repo-root)
    "Return fan-out repositories for REPO-ROOT, preserving Graal suite siblings."
    (let* ((repo-root (file-name-as-directory (expand-file-name repo-root)))
           (repo-name (file-name-nondirectory (directory-file-name repo-root)))
           (mx-dir (expand-file-name (format "mx.%s" repo-name) repo-root))
           (agent-shell-fanout-adjacent-repository-names
            (when (file-directory-p mx-dir)
              agent-shell-fanout-adjacent-repository-names)))
      (agent-shell-fanout-default-repositories repo-root)))
  (setq agent-shell-fanout-repositories-function
        #'timfel/agent-shell-fanout-graal-repositories)
  :hook
  (agent-shell-mode . hack-dir-local-variables-non-file-buffer))

(use-package agent-shell-ralph
  :hook
  (agent-shell-mode . agent-shell-ralph-rate-limit-retry-mode))

(use-package agent-shell-jira
  :defines (jira-detail-mode-map jira-issues-mode-map)
  :functions agent-shell-jira-return-dwim
  :after agent-shell
  :bind (:map agent-shell-mode-map
         ("RET" . agent-shell-jira-return-dwim)
         :map jira-detail-mode-map
         ("C-x a i" . agent-shell-jira-issues-investigate-marked-with-agent)
         :map jira-issues-mode-map
         ("C-x a i" . agent-shell-jira-issues-investigate-marked-with-agent)))

(use-package agent-shell-desktop
  :vc (:url "https://github.com/timfel/agent-shell-desktop.el" :branch "main" :rev :newest)
  :after (agent-shell)
  :hook
  (agent-shell-mode . (lambda ()
                        (when (and desktop-save-mode desktop-dirname desktop-file-modtime)
                          ;; this is so we wait roughly until we have a session id to save
                          (run-with-idle-timer 5 nil #'desktop-save desktop-dirname nil t))))
  (desktop-save-mode . agent-shell-desktop-mode))

(use-package agent-shell
  :ensure t
  :after exec-path-from-shell
  :functions (agent-shell-make-environment-variables
              agent-shell-openai-make-authentication
              agent-shell-make-goose-authentication
              agent-shell-opencode-make-authentication
              agent-shell-rename-buffer
              agent-shell-subscribe-to
              org-link-set-parameters
              org-link-store-props
              shell-maker-submit)
  :commands agent-shell
  :pin melpa
  :bind (("C-x a s" . agent-shell)
         ("C-x a b" . (lambda () (interactive) (ibuffer t "*agent buffers*" '((used-mode . agent-shell-mode)))))
         :map agent-shell-mode-map
         ("C-c RET" . shell-maker-submit)
         ("C-x a R" . agent-shell-restart)
         ("C-x a r" . agent-shell-reload))
  :hook
  (agent-shell-mode . (lambda ()
                        (agent-shell-subscribe-to
                         :shell-buffer (current-buffer)
                         :event 'turn-complete
                         :on-event (lambda (event)
                                     (when (and (equal (map-elt event :event) 'turn-complete)
                                                (equal (map-nested-elt event '(:data :stop-reason)) "end_turn")
                                                (bound-and-true-p show-help-function))
                                       (funcall show-help-function
                                                (format "Agent shell %s finished in %s."
                                                        (buffer-name)
                                                        default-directory)))))))
  :custom
  (agent-shell-inhibit-system-sleep nil)
  (agent-shell-thought-process-expand-by-default t)
  (agent-shell-activity-group-expand-by-default t)
  (agent-shell-busy-indicator-frames 'dots-round)
  (agent-shell-header-style 'text)
  (agent-shell-buffer-name-format (lambda (_agent-name project-name) (format "%s agent" project-name)))
  (agent-shell-session-strategy 'prompt)
  (agent-shell-session-restore-verbosity 'full)
  (agent-shell-prefer-viewport-interaction nil)
  (agent-shell-preferred-agent-config 'pi)
  (agent-shell-show-config-icons nil)
  (agent-shell-show-usage-at-turn-end t)
  (agent-shell-text-file-capabilities t)
  :config
  (fset 'timfel-agent-start #'agent-shell)
  (keymap-unset agent-shell-mode-map "p")
  (keymap-unset agent-shell-mode-map "n")

  ;; If any .agents/skills from this repo do not exist in $HOME/.agents/skills/ (Unix) or $Env:USERPROFILE/.agents/skills (Windows)
  ;; then copy them there
  (let* ((repo-root (locate-user-emacs-file ""))
         (skills-src-dir (expand-file-name ".agents/skills" repo-root))
         (skills-dst-dir (expand-file-name ".agents/skills" (or (getenv "USERPROFILE") "~"))))
    (when (file-directory-p skills-src-dir)
      (dolist (src (directory-files-recursively skills-src-dir ".*" t))
        (unless (file-directory-p src)
          (let* ((rel (file-relative-name src skills-src-dir))
                 (dst (expand-file-name rel skills-dst-dir)))
            (make-directory (file-name-directory dst) t)
            (when (file-newer-than-file-p src dst)
              (copy-file src dst t t)))))))

  (setq
   ;; pi-acp 0.0.33 defaults to pi.cmd on Windows.  The mise Pi package
   ;; provides pi.exe instead, so spawning the default command only starts a
   ;; short-lived cmd.exe and session/new fails when pi-acp writes to it.
   agent-shell-pi-environment
   (when (eq system-type 'windows-nt)
     (agent-shell-make-environment-variables
      "PI_ACP_PI_COMMAND" (or (executable-find "pi.exe") "pi.exe")
      :inherit-env t))
   agent-shell-openai-authentication (agent-shell-openai-make-authentication :login t)
   agent-shell-cline-environment (agent-shell-make-environment-variables :inherit-env t)
   agent-shell-openai-codex-environment (agent-shell-make-environment-variables :inherit-env t)
   agent-shell-goose-environment (agent-shell-make-environment-variables :inherit-env t)
   agent-shell-opencode-authentication (agent-shell-opencode-make-authentication :none t)))

(use-package agent-shell-tramp
  :vc (:url "https://github.com/junyi-hou/agent-shell-tramp" :branch "main" :rev :newest)
  :after agent-shell
  :demand t
  :config
  (agent-shell-tramp-mode 1))

(use-package agent-shell-dashboard
  :custom
  (agent-shell-dashboard-worktree-search-directories
   (list user-emacs-directory "~/dev/graalpython" "~/dev/graal" "~/dev/graalos" "~/tmp" "~/tmp/workloads" "~/tmp/graalos-apps/"))
  :bind (("C-x a a" . agent-shell-dashboard)))
