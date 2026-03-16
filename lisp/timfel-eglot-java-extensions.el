;;; timfel-eglot-java-extensions.el --- Eglot Java helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Local helpers for Eglot/JDTLS installation and Java debugging via GUD.

;;; Code:

(require 'project)
(require 'subr-x)
(require 'url-http)
(require 'url-handlers)
(require 'url-util)

(declare-function eglot-current-server "eglot")
(declare-function eglot-execute "eglot")
(declare-function eglot-ensure "eglot")
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

(defconst timfel/eglot-jdtls-install-buffer-name
  "*JDTLS Install*"
  "Name of the buffer used for JDTLS installation logs.")

(defvar timfel/eglot-jdtls--install-state nil
  "Plist describing an in-flight JDTLS installation.")

(defun timfel/eglot-jdtls--launcher ()
  "Return the absolute path to the JDTLS launcher script."
  (expand-file-name "bin/jdtls" timfel/eglot-jdtls-install-dir))

(defun timfel/eglot-jdtls--install-buffer ()
  "Return the JDTLS installation log buffer."
  (let ((buffer (get-buffer-create timfel/eglot-jdtls-install-buffer-name)))
    (with-current-buffer buffer
      (setq-local buffer-read-only nil)
      (setq-local default-directory user-emacs-directory))
    buffer))

(defun timfel/eglot-jdtls--display-install-buffer ()
  "Pop to the JDTLS installation log buffer."
  (pop-to-buffer (timfel/eglot-jdtls--install-buffer)))

(defun timfel/eglot-jdtls--log (format-string &rest args)
  "Append a formatted log line to the JDTLS installation buffer."
  (with-current-buffer (timfel/eglot-jdtls--install-buffer)
    (let ((inhibit-read-only t)
          (moving (= (point) (point-max))))
      (goto-char (point-max))
      (insert (apply #'format format-string args))
      (unless (bolp)
        (insert "\n"))
      (when moving
        (goto-char (point-max))))))

(defun timfel/eglot-jdtls--install-in-progress-p ()
  "Return non-nil when a JDTLS installation is already running."
  (let ((download-buffer (plist-get timfel/eglot-jdtls--install-state :download-buffer))
        (process (plist-get timfel/eglot-jdtls--install-state :process)))
    (or (and (bufferp download-buffer)
             (buffer-live-p download-buffer))
        (and process
             (process-live-p process)))))

(defun timfel/eglot-jdtls--clear-install-state ()
  "Clear the current JDTLS installation state."
  (setq timfel/eglot-jdtls--install-state nil))

(defun timfel/eglot-jdtls--cleanup-archive ()
  "Delete the temporary archive associated with the active installation."
  (when-let ((archive (plist-get timfel/eglot-jdtls--install-state :archive)))
    (when (file-exists-p archive)
      (delete-file archive))))

(defun timfel/eglot-jdtls--queue-retry (&optional buffer)
  "Queue BUFFER for an automatic Eglot retry after JDTLS finishes installing."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (setf (plist-get timfel/eglot-jdtls--install-state :retry-buffers)
            (delete-dups
             (cons buffer
                   (plist-get timfel/eglot-jdtls--install-state :retry-buffers)))))))

(defun timfel/eglot-jdtls--retry-buffer (buffer)
  "Retry `eglot-ensure' in BUFFER when it is still a live Java buffer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (derived-mode-p 'java-mode 'java-ts-mode)
                 (not (eglot-current-server)))
        (condition-case err
            (eglot-ensure)
          (error
           (timfel/eglot-jdtls--log
            "Automatic Eglot retry failed for %s: %s"
            (buffer-name buffer)
            (error-message-string err))))))))

(defun timfel/eglot-jdtls--retry-pending-buffers (buffers)
  "Retry Eglot startup for BUFFERS after JDTLS installation succeeds."
  (dolist (buffer (delete-dups (delq nil buffers)))
    (run-at-time 0 nil #'timfel/eglot-jdtls--retry-buffer buffer)))

(defun timfel/eglot-jdtls--finish-install (success message-text)
  "Finish the active JDTLS installation.

When SUCCESS is non-nil, log and message MESSAGE-TEXT as a success.
Otherwise treat MESSAGE-TEXT as an error."
  (let ((retry-buffers (plist-get timfel/eglot-jdtls--install-state :retry-buffers)))
    (unless success
      (setq message-text
            (format "%s; inspect %s for details"
                    message-text
                    timfel/eglot-jdtls-install-buffer-name)))
    (timfel/eglot-jdtls--log "%s" message-text)
    (timfel/eglot-jdtls--display-install-buffer)
    (timfel/eglot-jdtls--cleanup-archive)
    (timfel/eglot-jdtls--clear-install-state)
    (when success
      (timfel/eglot-jdtls--log
       "Retrying Eglot in %d pending buffer(s)"
       (length (delete-dups (delq nil retry-buffers))))
      (timfel/eglot-jdtls--retry-pending-buffers retry-buffers))
    (message "%s" message-text)))

(defun timfel/eglot-jdtls--extract (archive)
  "Extract ARCHIVE asynchronously into `timfel/eglot-jdtls-install-dir'."
  (let* ((buffer (timfel/eglot-jdtls--install-buffer))
         (command (mapconcat #'shell-quote-argument
                             (list "tar" "-xzvf" archive
                                   "-C" timfel/eglot-jdtls-install-dir)
                             " "))
         (process
          (make-process
           :name "jdtls-install"
           :buffer buffer
           :command (list shell-file-name shell-command-switch command)
           :connection-type 'pipe
           :noquery t
           :sentinel
           (lambda (proc event)
             (when (memq (process-status proc) '(exit signal))
               (if (and (zerop (process-exit-status proc))
                        (file-executable-p (timfel/eglot-jdtls--launcher)))
                   (timfel/eglot-jdtls--finish-install
                    t
                    (format "JDTLS installation finished: %s"
                            (timfel/eglot-jdtls--launcher)))
                 (timfel/eglot-jdtls--finish-install
                  nil
                  (format "JDTLS extraction failed (%s)"
                          (string-trim event)))))))))
    (plist-put timfel/eglot-jdtls--install-state :process process)
    (timfel/eglot-jdtls--log "Extracting JDTLS into %s" timfel/eglot-jdtls-install-dir)))

(defun timfel/eglot-jdtls--download-callback (status archive)
  "Handle the asynchronous JDTLS download STATUS and write it to ARCHIVE."
  (let ((response-buffer (current-buffer)))
    (unwind-protect
        (cond
         ((plist-get status :error)
          (timfel/eglot-jdtls--finish-install
           nil
           (format "JDTLS download failed: %s"
                   (error-message-string (plist-get status :error)))))
         ((and (boundp 'url-http-response-status)
               (>= url-http-response-status 400))
          (timfel/eglot-jdtls--finish-install
           nil
           (format "JDTLS download failed with HTTP %s"
                   url-http-response-status)))
         (t
          (plist-put timfel/eglot-jdtls--install-state :download-buffer nil)
          (goto-char (point-min))
          (re-search-forward "^$" nil 'move)
          (forward-line 1)
          (let ((coding-system-for-write 'no-conversion))
            (write-region (point) (point-max) archive nil 'silent))
          (timfel/eglot-jdtls--log "Downloaded archive to %s" archive)
          (timfel/eglot-jdtls--extract archive)))
      (when (buffer-live-p response-buffer)
        (kill-buffer response-buffer)))))

(defun timfel/eglot-jdtls--install-async ()
  "Start installing JDTLS asynchronously and show progress in a buffer."
  (let ((archive (make-temp-file "jdtls-" nil ".tar.gz")))
    (make-directory timfel/eglot-jdtls-install-dir t)
    (setq timfel/eglot-jdtls--install-state (list :archive archive))
    (with-current-buffer (timfel/eglot-jdtls--install-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (timfel/eglot-jdtls--log "Downloading JDTLS from %s" timfel/eglot-jdtls-download-url)
    (timfel/eglot-jdtls--display-install-buffer)
    (plist-put timfel/eglot-jdtls--install-state
               :download-buffer
               (url-retrieve timfel/eglot-jdtls-download-url
                             #'timfel/eglot-jdtls--download-callback
                             (list archive)
                             t t))))

(defun timfel/eglot-jdtls--ensure-installed ()
  "Ensure JDTLS is installed, starting an async install when needed.

Return non-nil when the launcher is ready to execute."
  (cond
   ((file-executable-p (timfel/eglot-jdtls--launcher)) t)
   ((timfel/eglot-jdtls--install-in-progress-p)
    (timfel/eglot-jdtls--display-install-buffer)
    nil)
   (t
    (timfel/eglot-jdtls--install-async)
    nil)))

(defun timfel/eglot-jdtls (is-interactive project)
  "Return the Jdtls command for the current Java project."
  (interactive)
  (ignore is-interactive)
  (unless (timfel/eglot-jdtls--ensure-installed)
    (timfel/eglot-jdtls--queue-retry (current-buffer))
    (user-error "JDTLS installation is running; Eglot will retry automatically after %s completes"
                timfel/eglot-jdtls-install-buffer-name))
  (let* ((project-root (expand-file-name
                        (project-root project)))
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
