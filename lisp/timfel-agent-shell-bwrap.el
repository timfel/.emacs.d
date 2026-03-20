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
                    (when (zerop (process-file "git" nil t nil
                                               "rev-parse" "--git-common-dir"))
                      (let ((gitdir (string-trim (buffer-string))))
                        (if (string-empty-p gitdir)
                            directory
                          (file-name-directory
                           (directory-file-name
                            (expand-file-name gitdir default-directory))))))))))
      (let* ((tmpdir (make-temp-file "/tmp/bcodex-session/" t))
             (common-root (git-common-root default-directory))
             (graal-dir (expand-file-name "../graal"))
             (extra-dir-to-bind (if (file-directory-p graal-dir)
                                    graal-dir
                                  default-directory))
             (graal-common-root (if (file-directory-p graal-dir)
                                    (git-common-root graal-dir)
                                  extra-dir-to-bind)))
        (append
         `("bwrap" "--die-with-parent" "--new-session"
           "--ro-bind" "/" "/"
           "--bind" ,default-directory ,default-directory
           "--bind" ,common-root ,common-root
           "--bind" ,extra-dir-to-bind ,extra-dir-to-bind
           "--bind" ,graal-common-root ,graal-common-root)
         (apply
          #'append
          (delq nil
                (mapcar
                 (lambda (path)
                   (when (file-exists-p path)
                     (list "--bind" path path)))
                 (list (expand-file-name "~/dev/mx")
                       (expand-file-name "~/dev/graal")
                       (expand-file-name "~/dev/graalpython")
                       (expand-file-name "~/dev/graal-enterprise")
                       (expand-file-name "~/.cache")
                       (expand-file-name "~/.mx")
                       (expand-file-name "~/dev/.metadata")
                       (expand-file-name "~/.eclipse")
                       (expand-file-name "~/.codex")
                       (expand-file-name "~/.opencode")
                       (expand-file-name "~/.config/opencode")))))
         `("--proc" "/proc"
           "--dev" "/dev"
           "--tmpfs" ,timfel/cloud-storage
           "--bind" ,tmpdir "/tmp"
           "--chdir" ,default-directory
           "--setenv" "HTTP_PROXY" ,(or (getenv "HTTP_PROXY") "")
           "--setenv" "HTTPS_PROXY" ,(or (getenv "HTTPS_PROXY") "")
           "--setenv" "NO_PROXY" ,(or (getenv "NO_PROXY") "")
           "--setenv" "HOME" ,(getenv "HOME")
           "--setenv" "TMPDIR" "/tmp"
           "--setenv" "XDG_CACHE_INNER" ,(expand-file-name ".agent-shell/xdgcache")
           "--setenv" "XDG_STATE_INNER" ,(expand-file-name ".agent-shell/xdgstate")
           "--setenv" "XDG_RUNTIME_INNER" ,(expand-file-name ".agent-shell/xdgruntime")
           "--"))))))

(provide 'timfel-agent-shell-bwrap)

;;; timfel-agent-shell-bwrap.el ends here
