;;; timfel-gptel-tools.el --- GPTel tool configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Local GPTel tool registrations and tool selection.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'llm-tool-collection)
(require 'timfel)
(require 'subr-x)
(require 'url-util)

(defun timfel/gptel-tool--ensure-executable (program)
  "Ensure PROGRAM is available in `exec-path'."
  (unless (executable-find program)
    (user-error "Required executable not found: %s" program)))

(defun timfel/gptel-tool--valid-directory (directory)
  "Return DIRECTORY as an absolute directory name, or signal `user-error'."
  (let ((expanded (expand-file-name (or directory default-directory))))
    (unless (file-directory-p expanded)
      (user-error "Directory does not exist: %s" expanded))
    (file-name-as-directory expanded)))

(defun timfel/gptel-tool--run-process (program args &optional directory)
  "Run PROGRAM with ARGS in DIRECTORY and return a plist with results."
  (let ((default-directory (timfel/gptel-tool--valid-directory directory)))
    (with-temp-buffer
      (let ((exit-code (apply #'process-file program nil (current-buffer) nil args)))
        (list :ok (zerop exit-code)
              :program program
              :args args
              :directory default-directory
              :exit-code exit-code
              :output (buffer-string))))))

(defun timfel/gptel-tool--run-shell-command (command &optional directory)
  "Run shell COMMAND in DIRECTORY and return a plist with results."
  (let ((default-directory (timfel/gptel-tool--valid-directory directory)))
    (with-temp-buffer
      (let ((exit-code (call-process shell-file-name nil (current-buffer) nil
                                     shell-command-switch command)))
        (list :ok (zerop exit-code)
              :command command
              :directory default-directory
              :exit-code exit-code
              :output (buffer-string))))))

(defun timfel/gptel-tool-execute-command (command &optional working-dir)
  "Execute COMMAND, optionally in WORKING-DIR, and return structured output."
  (with-temp-message (format "Executing command: `%s`" command)
    (timfel/gptel-tool--run-shell-command command working-dir)))

(defun timfel/gptel-tool-change-directory (dir)
  "Change the current buffer's `default-directory' to DIR."
  (let ((directory (timfel/gptel-tool--valid-directory dir)))
    (setq-local default-directory directory)
    (list :ok t :directory default-directory)))

(defun timfel/gptel-tool-get-current-directory ()
  "Return the current default directory."
  (list :directory default-directory))

(defun timfel/gptel-tool-get-recently-edited-filenames ()
  "Return up to 5 recently visited filenames from open buffers."
  (cl-loop for buffer in (buffer-list)
           for filename = (buffer-file-name buffer)
           unless (or (null filename)
                      (string-prefix-p " " (buffer-name buffer)))
           collect filename into files
           finally return (seq-take (delete-dups files) 5)))

(defun timfel/gptel-tool-search-in-project (pattern)
  "Search for PATTERN in the recent project using ripgrep."
  (timfel/gptel-tool--ensure-executable "rg")
  (let ((project-root (determine-recent-project-root)))
    (unless project-root
      (user-error "No recent project root found"))
    (let ((result (timfel/gptel-tool--run-process
                   "rg"
                   (list "--max-count" "20"
                         "--no-heading"
                         "--color" "never"
                         "-e" pattern
                         (expand-file-name project-root)))))
      (plist-put result :project-root (expand-file-name project-root))
      result)))

(defun timfel/gptel-tool-set-file-content (filename content)
  "Overwrite FILENAME with CONTENT and return a structured confirmation."
  (let* ((expanded (expand-file-name filename))
         (parent (file-name-directory expanded)))
    (when parent
      (make-directory parent t))
    (with-temp-file expanded
      (insert content))
    (list :ok t
          :file expanded
          :bytes (string-bytes content))))

(defun timfel/gptel-tool-read-webpage (url)
  "Read URL via `w3m -dump' and return structured output."
  (timfel/gptel-tool--ensure-executable "w3m")
  (let ((result (timfel/gptel-tool--run-process "w3m" (list "-dump" url))))
    (plist-put result :url url)
    result))

(defun timfel/gptel-tool-search-web (phrase)
  "Search the web for PHRASE, or dump it directly when it is a URL."
  (timfel/gptel-tool--ensure-executable "w3m")
  (let* ((url (if (string-match-p "^http" phrase)
                  phrase
                (format "https://duckduckgo.com/?q=%s"
                        (url-hexify-string phrase))))
         (result (timfel/gptel-tool--run-process "w3m" (list "-dump" url))))
    (plist-put result :query phrase)
    (plist-put result :url url)
    result))

(defun timfel/gptel-tool-list-buffers ()
  "Return the list of file-backed buffers currently open."
  (string-join (remove nil (mapcar #'buffer-file-name (buffer-list))) "\n"))

(defun timfel/gptel-tool--make-selected-category-tools (category names)
  "Return GPTel tools from CATEGORY whose names are listed in NAMES."
  (cl-loop for spec in (llm-tool-collection-get-category category)
           for name = (plist-get spec :name)
           when (member name names)
           collect (apply #'gptel-make-tool spec)))

(defvar timfel/gptel-tool--custom-tools nil
  "Custom GPTel tool objects defined by Tim's config.")

(setq timfel/gptel-tool--custom-tools
      (list
       (gptel-make-tool
        :function #'timfel/gptel-tool-execute-command
        :name "execute_command"
        :description "Execute a shell command and return a result plist with exit code, directory, and output. IMPORTANT: This tool allows execution of arbitrary code; user confirmation will be required before any command is run."
        :args (list
               '(:name "command" :type string :description "The complete shell command to execute.")
               '(:name "working_dir" :type string :description "Optional directory in which to run the command."))
        :category "command"
        :confirm t
        :include t)
       (gptel-make-tool
        :function #'timfel/gptel-tool-change-directory
        :name "change_directory"
        :description "Change the current buffer's working directory and return the new directory."
        :args (list '(:name "dir" :type string :description "The directory to cd into."))
        :category "command"
        :confirm t
        :include nil)
       (gptel-make-tool
        :function #'timfel/gptel-tool-get-current-directory
        :name "get_current_directory"
        :description "Return the current working directory as a plist."
        :args (list)
        :confirm nil
        :include nil
        :category "command")
       (gptel-make-tool
        :name "get_recently_edited_filenames"
        :description "Return up to 5 recently visited file-backed buffers in this Emacs session."
        :function #'timfel/gptel-tool-get-recently-edited-filenames
        :args (list)
        :confirm nil
        :include nil
        :category "buffers")
       (gptel-make-tool
        :name "search_in_project"
        :description "Search for a string within the recent project using ripgrep, returning a result plist."
        :function #'timfel/gptel-tool-search-in-project
        :args (list '(:name "pattern" :type string :description "Pattern to search for."))
        :confirm t
        :include nil
        :category "search")
       (gptel-make-tool
        :name "set_file_content"
        :description "Set the content of a file to the given string, creating parent directories when needed."
        :function #'timfel/gptel-tool-set-file-content
        :args (list
               '(:name "filename" :type string :description "The file to overwrite.")
               '(:name "content" :type string :description "The new content for the file."))
        :confirm t
        :include t
        :category "files")
       (gptel-make-tool
        :function #'timfel/gptel-tool-read-webpage
        :name "read_webpage"
        :description "Read the contents of a URL with w3m, returning a result plist."
        :args (list '(:name "url" :type string :description "The URL to read."))
        :category "web")
       (gptel-make-tool
        :function #'timfel/gptel-tool-search-web
        :name "search_web"
        :description "Search the web for a string, or read it directly when it is already a URL. Returns a result plist."
        :args (list '(:name "phrase" :type string :description "The keywords to search for on the web, or a URL."))
        :category "web")
       (gptel-make-tool
        :function #'timfel/gptel-tool-list-buffers
        :name "list_buffers"
        :description "Get the list of files the user has open in buffers."
        :args (list)
        :category "buffers")))

(defvar timfel/gptel-tool--collection-tools nil
  "Selected GPTel tool objects imported from `llm-tool-collection'.")

(setq timfel/gptel-tool--collection-tools
      (append (timfel/gptel-tool--make-selected-category-tools
               "filesystem"
               '("read_file" "list_directory" "create_file" "create_directory"))
              (timfel/gptel-tool--make-selected-category-tools
               "buffers"
               '("view_buffer"))))

(setq gptel-tools
      (append timfel/gptel-tool--custom-tools
              timfel/gptel-tool--collection-tools))

(setq gptel-use-tools t
      gptel-confirm-tool-calls 'auto)

(provide 'timfel-gptel-tools)

;;; timfel-gptel-tools.el ends here
