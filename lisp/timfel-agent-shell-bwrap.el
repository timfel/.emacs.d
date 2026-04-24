;;; timfel-agent-shell-bwrap.el --- Agent-shell bubblewrap prefix -*- lexical-binding: t -*-

;;; Commentary:

;; Bubblewrap command prefix for agent-shell sessions.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'timfel)

;;;###autoload
(defun timfel/agent-shell-command-prefix-bwrap (_buffer)
  "Return a `bwrap' command prefix for `agent-shell', or nil when unavailable."
  (when (executable-find "bwrap")
    (cl-flet ((git-common-root (directory)
                (let ((default-directory
                       (file-name-as-directory (expand-file-name directory))))
                  (with-temp-buffer
                    (if (zerop (process-file "git" nil t nil "rev-parse" "--git-common-dir"))
                        (let ((gitdir (string-trim (buffer-string))))
                          (if (string-empty-p gitdir)
                              directory
                            (file-name-directory
                             (directory-file-name
                              (expand-file-name gitdir default-directory)))))
                      directory)))))
      ;; delete older tmp dirs
      (let ((cutoff (time-subtract (current-time) (days-to-time 6))))
        (dolist (path (directory-files "/tmp" t "\\`bcodex-session"))
          (when (and (file-directory-p path)
                     (time-less-p
                      (file-attribute-modification-time
                       (file-attributes path))
                      cutoff))
            (ignore-errors
              (delete-directory path t)))))
      ;; figure out all the worktree dirs we may need
      (let* ((tmpdir (make-temp-file "/tmp/bcodex-session" t (replace-regexp-in-string "[^[:alnum:]]" "" default-directory)))
             (common-root (git-common-root default-directory))
             (graal-dir (expand-file-name "../graal"))
             (extra-dir-to-bind (if (file-directory-p graal-dir) graal-dir default-directory))
             (graal-common-root (if (file-directory-p graal-dir) (git-common-root graal-dir) extra-dir-to-bind))
             (real-config-toml (file-truename "~/.codex/config.toml")))
        (append
         `("bwrap" "--die-with-parent" "--new-session"
           "--ro-bind" "/" "/"
           "--tmpfs" "/tmp"
           "--tmpfs" ,(getenv "HOME"))
         ;; expose select folders as writable
         (thread-last
           (seq-map #'expand-file-name
                    `(,default-directory
                      ,common-root
                      ,extra-dir-to-bind
                      ,graal-common-root
                      ,tmpdir
                      "~/.cache"
                      "~/.codex"
                      "~/.eclipse"
                      "~/.gradle"
                      "~/.m2"
                      "~/.mx"
                      "~/.npm"
                      "~/.opencode"
                      "~/dev/.metadata"
                      "~/dev/graal"
                      "~/dev/graal-enterprise"
                      "~/dev/graalpython"
                      "~/dev/eclipse"
                      "~/dev/mx"
                      ))
           (seq-filter #'file-exists-p)
           (seq-mapcat (lambda (p) `("--bind" ,p ,p))))
         ;; expose some others as read-only
         (thread-last
           (seq-map #'expand-file-name
                    `(,real-config-toml
                      "~/.agents"
                      "~/.bun"
                      "~/.bundle"
                      "~/.config"
                      "~/.docker"
                      "~/.emacs.d"
                      "~/.gitconfig"
                      "~/.gitignore"
                      "~/.local"
                      "~/.npmrc"
                      "~/.nvm"
                      "~/.ol"
                      "~/.pyenv"
                      "~/.rustup"
                      "~/.sdkman"
                      ))
           (seq-filter #'file-exists-p)
           (seq-mapcat (lambda (p) `("--ro-bind" ,p ,p))))
         ;; some hide completely and make tmpfs
         (thread-last
           (seq-map #'expand-file-name
                    `(,timfel/cloud-storage
                      "~/.config/mc"
                      "~/.config/onedrive"
                      "~/.config/pulse"
                      "~/.config/rclone"
                      ))
           (seq-filter #'file-exists-p)
           (seq-mapcat (lambda (p) `("--tmpfs" ,p))))
         `("--proc" "/proc"
           "--dev" "/dev"
           "--chdir" ,default-directory
           "--setenv" "HTTP_PROXY" ,(or (getenv "HTTP_PROXY") "")
           "--setenv" "HTTPS_PROXY" ,(or (getenv "HTTPS_PROXY") "")
           "--setenv" "NO_PROXY" ,(or (getenv "NO_PROXY") "")
           "--setenv" "HOME" ,(getenv "HOME")
           "--setenv" "TMPDIR" ,tmpdir
           "--setenv" "XDG_CACHE_INNER" ,(expand-file-name ".agent-shell/xdgcache")
           "--setenv" "XDG_STATE_INNER" ,(expand-file-name ".agent-shell/xdgstate")
           "--setenv" "XDG_RUNTIME_INNER" ,(expand-file-name ".agent-shell/xdgruntime")
           "--"))))))

(provide 'timfel-agent-shell-bwrap)

;;; timfel-agent-shell-bwrap.el ends here
