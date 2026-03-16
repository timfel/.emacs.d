;;; timfel-eglot-java-extensions.el --- Eglot Java helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Local helpers for Eglot/JDTLS installation and Java debugging via GUD.

;;; Code:

(require 'project)
(require 'url-handlers)
(require 'url-util)

(declare-function eglot-current-server "eglot")
(declare-function eglot-execute "eglot")
(declare-function eglot-server-capable "eglot")
(declare-function jdb "gud")

(defvar eglot-server-programs)
(defvar gud-comint-buffer)
(defvar gud-jdb-classpath)
(defvar gud-jdb-command-name)
(defvar gud-jdb-sourcepath)
(defvar gud-jdb-use-classpath)

(defconst timfel/eglot-jdtls-install-dir
  (expand-file-name "lsp-servers/jdtls/" user-emacs-directory)
  "Directory where the JDTLS launcher is installed.")

(defconst timfel/eglot-jdtls-download-url
  "https://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz"
  "URL for the latest JDTLS archive.")

(defun timfel/eglot-jdtls--launcher ()
  "Return the absolute path to the JDTLS launcher script."
  (expand-file-name "bin/jdtls" timfel/eglot-jdtls-install-dir))

(defun timfel/eglot-jdtls--ensure-installed ()
  "Download and extract JDTLS unless it is already installed."
  (unless (file-executable-p (timfel/eglot-jdtls--launcher))
    (let ((archive (make-temp-file "jdtls-" nil ".tar.gz")))
      (make-directory timfel/eglot-jdtls-install-dir t)
      (message "Downloading JDTLS from %s" timfel/eglot-jdtls-download-url)
      (url-copy-file timfel/eglot-jdtls-download-url archive t)
      (unwind-protect
          (unless (zerop (call-process "tar" nil nil nil
                                       "-xzf" archive
                                       "-C" timfel/eglot-jdtls-install-dir))
            (error "Failed to extract JDTLS archive into %s"
                   timfel/eglot-jdtls-install-dir))
        (when (file-exists-p archive)
          (delete-file archive)))))
  (unless (file-executable-p (timfel/eglot-jdtls--launcher))
    (error "JDTLS launcher not found at %s"
           (timfel/eglot-jdtls--launcher))))

(defun timfel/eglot-jdtls ()
  "Return the Jdtls command for the current Java project."
  (interactive)
  (timfel/eglot-jdtls--ensure-installed)
  (let* ((project-root (expand-file-name
                        (project-root (project-current t))))
         (workspace-dir (expand-file-name ".cache/.jdtls.workspace/"
                                          project-root))
         (cache-dir (expand-file-name "cache/" workspace-dir)))
    (make-directory cache-dir t)
    (list (timfel/eglot-jdtls--launcher)
          "-configuration" workspace-dir
          "-data" cache-dir)))

(defun timfel/jdb--file-uri (file-name)
  "Return FILE-NAME as a file URI."
  (url-encode-url (concat "file://" (expand-file-name file-name))))

(defun timfel/jdb--normalize-path (path)
  "Normalize a PATH returned by JDTLS."
  (cond
   ((not (stringp path)) nil)
   ((string-prefix-p "file://" path)
    (url-unhex-string (substring path 7)))
   (t path)))

(defun timfel/jdb--normalize-path-list (paths)
  "Normalize PATHS returned by JDTLS into a deduplicated list."
  (delete-dups
   (delq nil
         (mapcar #'timfel/jdb--normalize-path (append paths nil)))))

(defun timfel/jdb--execute-command (command arguments)
  "Execute JDTLS COMMAND with ARGUMENTS in the current Eglot session."
  (let ((commands (append (or (eglot-server-capable :executeCommandProvider :commands)
                              '())
                          nil)))
    (unless (member command commands)
      (user-error "Current Eglot server does not advertise `%s'" command))
    (eglot-execute
     (eglot-current-server)
     `(:command ,command
       :arguments ,(if (vectorp arguments)
                       arguments
                     (vconcat arguments))))))

(defun timfel/jdb--project-paths ()
  "Return classpath and sourcepath data for the current Java project."
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (unless (eglot-current-server)
    (user-error "Current buffer is not managed by Eglot"))
  (let* ((uri (timfel/jdb--file-uri buffer-file-name))
         (classpath-reply
          (timfel/jdb--execute-command
           "java.project.getClasspaths"
           (vector uri "{\"scope\":\"runtime\"}")))
         (classpath-list
          (timfel/jdb--normalize-path-list
           (append (plist-get classpath-reply :classpaths)
                   (plist-get classpath-reply :modulepaths))))
         (sourcepath-list
          (condition-case err
              (timfel/jdb--normalize-path-list
               (timfel/jdb--execute-command
                "java.project.listSourcePaths"
                (vector uri)))
            (error
             (message "Could not fetch JDTLS source paths: %s"
                      (error-message-string err))
             nil))))
    (list :classpath-list classpath-list
          :sourcepath-list sourcepath-list
          :classpath (mapconcat #'identity classpath-list path-separator)
          :sourcepath (mapconcat #'identity sourcepath-list path-separator))))

(defun timfel/jdb--java-debug-address (args)
  "Extract a JDWP attach address from Java process ARGS."
  (when (and (stringp args)
             (or (string-match "-agentlib:jdwp=[^[:space:]]*address=\\([^,[:space:]]+\\)" args)
                 (string-match "-Xrunjdwp:[^[:space:]]*address=\\([^,[:space:]]+\\)" args)))
    (let ((address (match-string 1 args)))
      (cond
       ((string-prefix-p "*:" address)
        (concat "localhost:" (substring address 2)))
       ((string-prefix-p "0.0.0.0:" address)
        (concat "localhost:" (substring address 8)))
       (t address)))))

(defun timfel/jdb--java-process-candidates ()
  "Return running Java processes that expose a JDWP attach address."
  (let (candidates)
    (dolist (pid (list-system-processes))
      (let* ((attrs (process-attributes pid))
             (comm (alist-get 'comm attrs))
             (args (alist-get 'args attrs))
             (address (timfel/jdb--java-debug-address args)))
        (when (and address
                   (or (and (stringp comm)
                            (string-prefix-p "java" comm))
                       (and (stringp args)
                            (string-match-p "\\bjava\\b" args))))
          (push (cons (format "%s [%s] %s"
                              address pid
                              (truncate-string-to-width
                               (or args comm "java") 120 nil nil t))
                      address)
                candidates))))
    (nreverse candidates)))

(defun timfel/jdb--read-attach-address ()
  "Prompt for a JDWP attach address from running Java processes."
  (let* ((candidates (timfel/jdb--java-process-candidates))
         (launch-choice "[Start jdb without attach]")
         (choice
          (completing-read
           "JDB target: "
           (cons launch-choice (mapcar #'car candidates))
           nil t nil nil
           (if candidates (caar candidates) launch-choice))))
    (unless (equal choice launch-choice)
      (cdr (assoc choice candidates)))))

(defun timfel/jdb--command-line (paths attach-address extra-args)
  "Build a jdb command line from PATHS, ATTACH-ADDRESS and EXTRA-ARGS."
  (mapconcat
   #'identity
   (delq nil
         (list (or (and (boundp 'gud-jdb-command-name) gud-jdb-command-name)
                   "jdb")
               (when attach-address
                 (format "-attach %s" attach-address))
               (let ((classpath (plist-get paths :classpath)))
                 (when (> (length classpath) 0)
                   (concat "-classpath" classpath)))
               (let ((sourcepath (plist-get paths :sourcepath)))
                 (when (> (length sourcepath) 0)
                   (concat "-sourcepath" sourcepath)))
               (unless (equal extra-args "")
                 extra-args)))
   " "))

(defun timfel/jdb ()
  "Run GUD jdb using classpaths and sourcepaths from the current Eglot project.

If a running Java process exposes a JDWP address, offer to attach to it.
Otherwise start jdb with the current project's classpath configuration."
  (interactive)
  (require 'gud)
  (unless (derived-mode-p 'java-mode 'java-ts-mode)
    (user-error "Run `timfel/jdb' from a Java buffer"))
  (let* ((paths (timfel/jdb--project-paths))
         (attach-address (timfel/jdb--read-attach-address))
         (extra-args (read-from-minibuffer
                      (if attach-address
                          "Additional jdb arguments (optional): "
                        "Class or additional jdb arguments (optional): ")))
         (command-line (timfel/jdb--command-line
                        paths attach-address extra-args)))
    (jdb command-line)
    (when (buffer-live-p gud-comint-buffer)
      (with-current-buffer gud-comint-buffer
        (setq-local gud-jdb-use-classpath t
                    gud-jdb-classpath (plist-get paths :classpath-list)
                    gud-jdb-sourcepath (plist-get paths :sourcepath-list))))))

(provide 'timfel-eglot-java-extensions)

;;; timfel-eglot-java-extensions.el ends here
