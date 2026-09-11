;;; eglot-jdtls.el --- Eglot Java helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Minimal Eglot/JDTLS integration.  JDTLS is expected to be available as
;; `eglot-jdtls-command' in PATH; this file only supplies per-project
;; workspace/cache directories and initialization options.

;;; Code:

(require 'project)
(require 'subr-x)
(require 'eglot)

(defgroup eglot-jdtls nil
  "Small Java helpers for Eglot."
  :group 'eglot)

(defcustom eglot-jdtls-command "jdtls"
  "JDTLS executable to run.
The default uses `jdtls' from PATH.  For TRAMP projects, Eglot starts the
process through TRAMP, so this should also be available in the remote PATH."
  :type 'string
  :group 'eglot-jdtls)

(defcustom eglot-jdtls-initialization-options
  '(:extendedClientCapabilities
    (:classFileContentsSupport t)
    :settings
    (:java (:format (:onType (:enabled nil)
                    :comments (:enabled nil)
                    :enabled nil)
            :hover (:javadoc (:enabled t))
            :contentProvider (:preferred ["fernflowerContentProvider"])
            :references (:includeDecompiledSources t)
            :completion (:guessMethodArguments t
                         :overwrite t
                         :enabled t)
            :import (:maven (:enabled t)
                     :gradle (:enabled t))
            :autobuild (:enabled nil)
            :saveActions (:organizeImports nil)
            :jdt (:ls (:javac (:enabled t)))
            :project (:importOnFirstTimeStartup t))))
  "Initialization options passed to JDTLS."
  :type 'sexp
  :group 'eglot-jdtls)

;;; JDTLS workspace, cache directory, and temporary file location handling

(defun eglot-jdtls--project-id (project-root)
  "Return a stable directory name for PROJECT-ROOT."
  (let* ((root (directory-file-name (expand-file-name project-root)))
         (name (file-name-nondirectory (file-local-name root)))
         (safe-name (string-trim
                     (replace-regexp-in-string "[^[:alnum:]._-]+" "-" name)
                     "-+"
                     "-+"))
         (hash (substring (secure-hash 'sha1 root) 0 12)))
    (format "%s-%s" (if (string-empty-p safe-name) "project" safe-name) hash)))

(defun eglot-jdtls--state-base (project-root)
  "Return the JDTLS state base directory for PROJECT-ROOT."
  (if-let* ((remote (file-remote-p project-root)))
      (let ((remote-dir (format "/tmp/emacs-jdtls-%s/"
                                (replace-regexp-in-string "[^[:alnum:]._-]+" "-"
                                                          (user-login-name)))))
        (concat remote (file-name-as-directory remote-dir)))
    (file-name-as-directory (expand-file-name (locate-user-emacs-file ".cache/eglot.jdtls")))))

(defun eglot-jdtls--project-dir (project-root)
  "Return the JDTLS state directory for PROJECT-ROOT."
  (expand-file-name (eglot-jdtls--project-id project-root)
                    (eglot-jdtls--state-base project-root)))

(defun eglot-jdtls--jdt-cache-dir ()
  "Return cache directory for JDTLS virtual classfile sources."
  (let* ((project (project-current t))
         (root (project-root project))
         (project-dir (eglot-jdtls--project-dir root))
         (cache-dir (expand-file-name "jdt-contents/" project-dir)))
    (make-directory cache-dir t)
    cache-dir))

;;; JDTLS subprocess initialization and arguments preparation

(defun eglot-jdtls--local-argument (file-name)
  "Return FILE-NAME as a path suitable for the JDTLS process."
  (file-local-name (expand-file-name file-name)))

(defun eglot-jdtls--workspace-roots (project)
  "Return root directories to advertise as workspace folders for PROJECT."
  (let (roots)
    (dolist (root (cons (project-root project)
                        (project-external-roots project))
                  (nreverse roots))
      (when (stringp root)
        (let ((root (file-name-as-directory (expand-file-name root))))
          (unless (member root roots)
            (push root roots)))))))

(defun eglot-jdtls--initialization-options (project)
  "Return JDTLS initialization options for PROJECT."
  (let ((options (copy-tree eglot-jdtls-initialization-options)))
    (plist-put options
               :workspaceFolders
               (vconcat
                (mapcar #'eglot-path-to-uri
                        (eglot-jdtls--workspace-roots project))))
    options))

;;; jdt:// URI handling for decompiled sources

(defun eglot-jdtls--jdt-uri-file-name (uri)
  "Return stable cache file name for JDTLS URI."
  (let* ((decoded (url-unhex-string uri))
         (base (if (string-match
                    "\\`jdt://contents/[^/]+/\\(.+?\\)\\.\\(?:class\\|java\\)\\?"
                    decoded)
                   (replace-regexp-in-string "/" "." (match-string 1 decoded))
                 (secure-hash 'sha1 decoded))))
    (expand-file-name (concat base ".java") (eglot-jdtls--jdt-cache-dir))))

(defun eglot-jdtls--resolve-jdt-uri (uri)
  "Resolve JDTLS URI to a local cached Java source file."
  (let ((file (eglot-jdtls--jdt-uri-file-name uri)))
    (unless (file-readable-p file)
      (let ((content (jsonrpc-request
                      (eglot-current-server)
                      :java/classFileContents
                      `(:uri ,uri))))
        (make-directory (file-name-directory file) t)
        (with-temp-file file
          (insert content))))
    file))

(defun eglot-jdtls--file-handler (operation &rest args)
  "Handle JDTLS virtual classfile paths."
  (let ((inhibit-file-name-handlers
         (cons 'eglot-jdtls--file-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (pcase operation
      ('file-exists-p t)
      ('file-readable-p t)
      ('file-regular-p t)
      ('expand-file-name (car args))
      ('file-local-copy (eglot-jdtls--resolve-jdt-uri (car args)))
      ('insert-file-contents
       (let ((file (eglot-jdtls--resolve-jdt-uri (car args))))
         (apply #'insert-file-contents file (cdr args))))
      (_
       (let ((resolved (eglot-jdtls--resolve-jdt-uri (car args))))
         (apply operation resolved (cdr args)))))))

(add-to-list 'file-name-handler-alist '("\\`jdt://contents/" . eglot-jdtls--file-handler))

;;;###autoload
(defun eglot-jdtls-clear-workspace-and-cache ()
  "Delete the workspace directory for the current project after confirming."
  (interactive)
  (when-let* ((project (project-current t))
              (root (project-root project))
              (directory (eglot-jdtls--project-dir root)))
    (when (yes-or-no-p
           (format "Delete JDTLS workspace and cache directory %s? " directory))
      (ignore-errors (call-interactively #'eglot-shutdown))
      (when (file-directory-p directory)
        (delete-directory directory t))
      (message "Deleted JDTLS workspace and cache for %s" root)
      (when (yes-or-no-p "Delete also the Eclipse project files?")
        ;; no recursively delete all eclipse config files under the project
        ;; root, so they can be regenerated
        (dolist (file (directory-files-recursively
                       root
                       "\\(?:\\.classpath\\|\\.project\\|\\.settings\\|\\.factorypath\\)\\'"
                       t))
          (if (file-directory-p file)
              (delete-directory file t)
            (delete-file file)))
        (message "Deleted Eclipse project files for %s" root)))))

;;;###autoload
(defun eglot-jdtls (_interactive project)
  "Return an Eglot contact for JDTLS in PROJECT."
  (let* ((project-root (project-root project))
         (project-dir (eglot-jdtls--project-dir project-root))
         (cache-dir (expand-file-name "cache/" project-dir))
         (workspace-dir (expand-file-name "workspace/" project-dir)))
    (make-directory cache-dir t)
    (make-directory workspace-dir t)
    (append
     (list eglot-jdtls-command
           "-configuration" (eglot-jdtls--local-argument cache-dir)
           "-data" (eglot-jdtls--local-argument workspace-dir))
     (list :initializationOptions
           (eglot-jdtls--initialization-options project)))))

(provide 'eglot-jdtls)

;;; eglot-jdtls.el ends here
