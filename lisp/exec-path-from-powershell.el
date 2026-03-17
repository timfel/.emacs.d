;;; exec-path-from-powershell.el --- Get env vars from PowerShell -*- lexical-binding: t; -*-

;; Author: Tim Felgentreff <timfelgentreff@gmail.com>
;; Keywords: windows, environment
;; Package-Version: 1.0
;; Package-Requires: ((emacs "24.4") (exec-path-from-shell "2.2"))

;;; Commentary:

;; Provide a PowerShell-backed variant of the exec-path-from-shell API. This
;; annotates exec-path-from-shell.el and if exec-path-from-shell-shell-name is
;; either powershell.exe or pwsh.exe redirects to the functions here.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'exec-path-from-shell)

(defgroup exec-path-from-powershell nil
  "Make Emacs use PowerShell-defined values for $PATH etc."
  :prefix "exec-path-from-powershell-"
  :group 'environment)

(defcustom exec-path-from-powershell-load-profile t
  "If non-nil, load the PowerShell profile (do not pass -NoProfile)."
  :type 'boolean
  :group 'exec-path-from-powershell)

(defcustom exec-path-from-powershell-includes-visual-studio-environment t
  "If non-nil, also import the Visual Studio developer environment."
  :type 'boolean
  :group 'exec-path-from-powershell)

(defun exec-path-from-powershell--default-visual-studio-arch ()
  "Return the default Visual Studio architecture for the current machine."
  (let ((arch (downcase
               (or (getenv "PROCESSOR_ARCHITEW6432")
                   (getenv "PROCESSOR_ARCHITECTURE")
                   (car (split-string system-configuration "-"))
                   ""))))
    (cond
     ((member arch '("amd64" "x86_64" "x64")) "x64")
     ((member arch '("x86" "i386" "i486" "i586" "i686")) "x86")
     ((member arch '("arm64" "aarch64")) "arm64")
     ((member arch '("arm")) "arm")
     (t "x64"))))

(defcustom exec-path-from-powershell-visual-studio-arch
  (exec-path-from-powershell--default-visual-studio-arch)
  "Architecture argument for VsDevCmd/vcvarsall (e.g. x64, x86, arm64)."
  :type 'string
  :group 'exec-path-from-powershell)

(defcustom exec-path-from-powershell-visual-studio-host-arch
  (exec-path-from-powershell--default-visual-studio-arch)
  "Host architecture argument for VsDevCmd (e.g. x64, x86, arm64)."
  :type 'string
  :group 'exec-path-from-powershell)

(defvar eshell-path-env)

(defun exec-path-from-powershell--debug (msg &rest args)
  "Print MSG and ARGS like `message', but only if debug output is enabled."
  (when exec-path-from-shell-debug
    (apply #'message msg args)))

(defmacro exec-path-from-powershell--warn-duration (&rest body)
  "Evaluate BODY and warn if execution duration exceeds a time limit.
The limit is given by `exec-path-from-shell-warn-duration-millis'."
  (declare (indent 0))
  (let ((start-time (cl-gensym))
        (duration-millis (cl-gensym)))
    `(let ((,start-time (current-time)))
       (prog1
           (progn ,@body)
         (let ((,duration-millis (* 1000.0 (float-time (time-subtract (current-time) ,start-time)))))
           (if (> ,duration-millis exec-path-from-shell-warn-duration-millis)
               (message "Warning: exec-path-from-powershell execution took %dms" ,duration-millis)
             (exec-path-from-powershell--debug "PowerShell execution took %dms" ,duration-millis)))))))

(defun exec-path-from-powershell--ps-quote (s)
  "Quote S as a PowerShell single-quoted string."
  (concat "'" (replace-regexp-in-string "'" "''" s t t) "'"))

(defun exec-path-from-powershell--call-powershell (ps-script)
  "Run PowerShell with PS-SCRIPT and return stdout as a string."
  (let* ((exe exec-path-from-shell-shell-name)
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         (temp-ps1 (let ((inhibit-message t))
                     (make-temp-file "exec-path-from-powershell-" nil ".ps1" ps-script)))
         (args (append
                (list "-ExecutionPolicy" "Bypass" "-NonInteractive")
                (unless exec-path-from-powershell-load-profile
                  (list "-NoProfile"))
                (list "-File" temp-ps1))))
    (unwind-protect
        (with-temp-buffer
          (exec-path-from-powershell--debug "Invoking %s with args %S" exe args)
          (exec-path-from-powershell--debug "Powershell script contents:\n%s" ps-script)
          (let ((exit (exec-path-from-powershell--warn-duration
                      (apply #'call-process exe nil t nil args))))
            (unless (eq exit 0)
              (error "PowerShell failed (exit %s): %s" exit (buffer-string))))
          (buffer-string))
      (ignore-errors (delete-file temp-ps1)))))

(defvar exec-path-from-powershell--visual-studio-variable-cache
  (make-hash-table :test #'equal)
  "Cache of discovered Visual Studio environment variable names.")

(defun exec-path-from-powershell--build-vs-block (env-table-var)
  "Return a PowerShell snippet that merges a Visual Studio environment.
ENV-TABLE-VAR names the PowerShell dictionary variable to update."
  (let ((arch (exec-path-from-powershell--ps-quote exec-path-from-powershell-visual-studio-arch))
        (host (exec-path-from-powershell--ps-quote exec-path-from-powershell-visual-studio-host-arch)))
    (concat
     "$vsOutput = @();"
     "try {"
     "  $progFiles = ${env:ProgramFiles(x86)};"
     "  if (-not $progFiles) { $progFiles = ${env:ProgramFiles} };"
     "  $vswhere = Join-Path $progFiles 'Microsoft Visual Studio\\Installer\\vswhere.exe';"
     "  if (-not (Test-Path $vswhere)) { throw \"vswhere not found: $vswhere\" };"
     "  $install = & $vswhere -latest -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath;"
     "  if (-not $install) { throw 'No suitable Visual Studio/BuildTools installation found.' };"
     "  $vsdev = Join-Path $install 'Common7\\Tools\\VsDevCmd.bat';"
     "  $vcvars = Join-Path $install 'VC\\Auxiliary\\Build\\vcvarsall.bat';"
     "  $cmdLine = $null;"
     "  if (Test-Path $vsdev) {"
     "    $cmdLine = ('call \"' + $vsdev + '\" -no_logo -arch=' + " arch " + ' -host_arch=' + " host ");"
     "  } elseif (Test-Path $vcvars) {"
     "    $cmdLine = ('call \"' + $vcvars + '\" ' + " arch ");"
     "  } else { throw \"Neither VsDevCmd.bat nor vcvarsall.bat found under: $install\" };"
     "  $cmd = 'chcp 65001>nul & ' + $cmdLine + ' & set';"
     "  $vsOutput = & cmd.exe /d /s /c $cmd;"
     "} catch {"
     "  throw $_"
     "};"
     "foreach ($line in $vsOutput) {"
     "  if ($line -match '^(.*?)=(.*)$') {"
     "    " env-table-var "[$matches[1]] = $matches[2]"
     "  }"
     "};")))

(defun exec-path-from-powershell--visual-studio-variable-cache-key ()
  "Return the cache key for the current Visual Studio environment settings."
  (list exec-path-from-shell-shell-name
        exec-path-from-powershell-load-profile
        exec-path-from-powershell-visual-studio-arch
        exec-path-from-powershell-visual-studio-host-arch))

(defun exec-path-from-powershell--visual-studio-variable-names ()
  "Return the Visual Studio env variable names that differ from the base shell."
  (let* ((cache-key (exec-path-from-powershell--visual-studio-variable-cache-key))
         (cached (gethash cache-key exec-path-from-powershell--visual-studio-variable-cache)))
    (or cached
        (let* ((script
                (concat
                 "$ErrorActionPreference='Stop';"
                 "[Console]::OutputEncoding=[System.Text.Encoding]::UTF8;"
                 "$baseEnv = New-Object 'System.Collections.Generic.Dictionary[string,string]' ([System.StringComparer]::OrdinalIgnoreCase);"
                 "[System.Environment]::GetEnvironmentVariables().GetEnumerator() | ForEach-Object { $baseEnv[$_.Key] = [string]$_.Value };"
                 "$vsEnv = New-Object 'System.Collections.Generic.Dictionary[string,string]' ([System.StringComparer]::OrdinalIgnoreCase);"
                 "$baseEnv.GetEnumerator() | ForEach-Object { $vsEnv[$_.Key] = $_.Value };"
                 (exec-path-from-powershell--build-vs-block "$vsEnv")
                 "$allNames = New-Object 'System.Collections.Generic.HashSet[string]' ([System.StringComparer]::OrdinalIgnoreCase);"
                 "$baseEnv.Keys | ForEach-Object { [void]$allNames.Add($_) };"
                 "$vsEnv.Keys | ForEach-Object { [void]$allNames.Add($_) };"
                 "$result = foreach ($name in $allNames) {"
                 "  $baseValue = $baseEnv[$name];"
                 "  $vsValue = $vsEnv[$name];"
                 "  if ($baseValue -ne $vsValue) { $name }"
                 "};"
                 "$result | Sort-Object | ConvertTo-Json -Compress;"))
               (raw (exec-path-from-powershell--call-powershell script))
               (json-array-type 'list)
               (names (json-read-from-string (substring raw (or (string-match "\\[\\s-*\"" raw) 0)))))
          (puthash cache-key names exec-path-from-powershell--visual-studio-variable-cache)
          names))))

(defun exec-path-from-powershell--maybe-extend-names-with-visual-studio-env (names)
  "Return NAMES, augmented with Visual Studio variables when appropriate."
  (if (and exec-path-from-powershell-includes-visual-studio-environment
           (equal names exec-path-from-shell-variables))
      (cl-remove-duplicates
       (append names (exec-path-from-powershell--visual-studio-variable-names))
       :test #'string-equal)
    names))

(defun exec-path-from-powershell--build-env-script (names include-vs)
  "Build a PowerShell script that returns JSON for NAMES.
When INCLUDE-VS is non-nil, merge the Visual Studio developer environment."
  (let* ((names-array (concat "@(" (mapconcat #'exec-path-from-powershell--ps-quote names ", ") ")"))
         (vs-block (and include-vs (exec-path-from-powershell--build-vs-block "$envTable"))))
    (concat
     "$ErrorActionPreference='Stop';"
     "[Console]::OutputEncoding=[System.Text.Encoding]::UTF8;"
     "$names=" names-array ";"
     "$envTable = New-Object 'System.Collections.Generic.Dictionary[string,string]' ([System.StringComparer]::OrdinalIgnoreCase);"
     "[System.Environment]::GetEnvironmentVariables().GetEnumerator() | ForEach-Object { $envTable[$_.Key] = [string]$_.Value };"
     "$result = @{};"
     "foreach ($name in $names) {"
     "  $result[$name] = $envTable[$name]"
     "};"
     (if vs-block
         (concat
          vs-block
          ;; if $result already contains $name, either the new or the old value
          ;; is a list of paths separated by ';', and the new and old value are
          ;; not identical, then prepend the new value to the old value. this
          ;; mimics visual studio updating path variables in the environment on
          ;; top of the user's powershell environment
          "foreach ($name in $names) {"
          "  $newValue = $envTable[$name];"
          "  $oldValue = $result[$name];"
          "  if (($null -ne $oldValue) -and ($null -ne $newValue) -and ($oldValue -ne $newValue) -and (($oldValue -match '.;.') -or ($newValue -match '.;.'))) {"
          "    $result[$name] = ($newValue + ';' + $oldValue)"
          "  } else {"
          "    $result[$name] = $newValue"
          "  }"
          "};")
       "")
     "$result | ConvertTo-Json -Compress;")))

(defun exec-path-from-powershell--evaluate-expressions (expressions)
  "Return list of PowerShell evaluations for EXPRESSIONS."
  (if (null expressions)
      '()
    (let* ((array-literal (concat "@(" (mapconcat #'exec-path-from-powershell--ps-quote expressions ", ") ")"))
           (script (concat
                    "$ErrorActionPreference='Stop';"
                    "[Console]::OutputEncoding=[System.Text.Encoding]::UTF8;"
                    "$exprs=" array-literal ";"
                    "$results = foreach ($e in $exprs) {"
                    "  $value = Invoke-Expression $e;"
                    "  if ($value -is [Array]) {"
                    "    $value = ($value -join [System.IO.Path]::PathSeparator)"
                    "  };"
                    "  if ($null -eq $value) {"
                    "    $null"
                    "  } else {"
                    "    [string]$value"
                    "  }"
                    "};"
                    "$results | ConvertTo-Json -Compress;"))
           (raw (exec-path-from-powershell--call-powershell script)))
      (let ((json-null :null)
            (json-array-type 'list))
        (mapcar (lambda (value) (unless (eq value :null) value))
                (json-read-from-string (substring raw (or (string-match "\\[\\s-*\"" raw) 0))))))))

(defun exec-path-from-powershell--decode-escapes (template)
  "Decode printf-style escapes in TEMPLATE."
  (let ((len (length template))
        (idx 0)
        (result (get-buffer-create " *exec-path-from-powershell-tmp*")))
    (unwind-protect
        (with-current-buffer result
          (erase-buffer)
          (while (< idx len)
            (let ((ch (aref template idx)))
              (setq idx (1+ idx))
              (cond
               ((/= ch ?\\)
                (insert-char ch 1))
               ((>= idx len)
                (insert-char ?\\ 1))
               (t
                (let ((esc (aref template idx)))
                  (setq idx (1+ idx))
                  (pcase esc
                    (?\\ (insert-char ?\\ 1))
                    (?\" (insert-char ?\" 1))
                    (?\' (insert-char ?\' 1))
                    (?a (insert-char ?\a 1))
                    (?b (insert-char ?\b 1))
                    (?e (insert-char ?\e 1))
                    (?f (insert-char ?\f 1))
                    (?n (insert-char ?\n 1))
                    (?r (insert-char ?\r 1))
                    (?t (insert-char ?\t 1))
                    (?v (insert-char ?\v 1))
                    (?0
                     (let ((start idx)
                           (digits ""))
                       (while (and (< idx len)
                                   (<= ?0 (aref template idx))
                                   (<= (aref template idx) ?7)
                                   (< (- idx start) 3))
                         (setq digits (concat digits (string (aref template idx))))
                         (setq idx (1+ idx)))
                       (insert-char (string-to-number (if (string-empty-p digits) "0" digits) 8) 1)))
                    (?x
                     (let ((start idx)
                           (digits ""))
                       (while (and (< idx len)
                                   (or (and (<= ?0 (aref template idx))
                                            (<= (aref template idx) ?9))
                                       (and (<= ?A (aref template idx))
                                            (<= (aref template idx) ?F))
                                       (and (<= ?a (aref template idx))
                                            (<= (aref template idx) ?f)))
                                   (< (- idx start) 2))
                         (setq digits (concat digits (string (aref template idx))))
                         (setq idx (1+ idx)))
                       (insert-char (string-to-number (if (string-empty-p digits) "0" digits) 16) 1)))
                    (_ (insert-char esc 1))))))
              ))
          (buffer-string))
      (kill-buffer result))))

(defun exec-path-from-powershell--printf (oldfunc str &optional args)
  "Return the PowerShell evaluation of STR formatted with ARGS.
STR follows the same conventions as `exec-path-from-shell-printf'."
  (if (not (member (file-name-nondirectory exec-path-from-shell-shell-name) '("pwsh.exe" "powershell.exe")))
      (funcall oldfunc str args)
    (let* ((decoded (exec-path-from-powershell--decode-escapes str))
           (values (exec-path-from-powershell--evaluate-expressions args))
           (idx 0)
           (len (length decoded))
           (pieces (get-buffer-create " *exec-path-from-powershell-printf*")))
      (unwind-protect
          (with-current-buffer pieces
            (erase-buffer)
            (while (< idx len)
              (let ((ch (aref decoded idx)))
                (setq idx (1+ idx))
                (if (/= ch ?%)
                    (insert-char ch 1)
                  (progn
                    (when (>= idx len)
                      (error "Incomplete format specifier in %S" str))
                    (let ((next (aref decoded idx)))
                      (setq idx (1+ idx))
                      (pcase next
                        (?% (insert-char ?% 1))
                        (?s
                         (if (null values)
                             (error "Not enough arguments for format string")
                           (insert (or (pop values) ""))))
                        (_
                         (error "Unsupported format specifier %%%c" next))))))))
            (when values
              (dolist (extra values)
                (insert (or extra ""))))
            (buffer-string))
        (kill-buffer pieces)))))

(defun exec-path-from-powershell--getenvs (oldfunc names)
  "Get the environment variables with NAMES from PowerShell.

If exec-path-from-shell-shell-name is not powershell.exe or pwsh.exe,
just delegate to exec-path-from-shell-getenvs.

The result is a list of (NAME . VALUE) pairs."
  (if (not (member (file-name-nondirectory exec-path-from-shell-shell-name) '("pwsh.exe" "powershell.exe")))
      (funcall oldfunc names)
    (when (file-remote-p default-directory)
      (error "You cannot run exec-path-from-powershell from a remote buffer (Tramp, etc.)"))
    (let* ((names (exec-path-from-powershell--maybe-extend-names-with-visual-studio-env names))
           (script (exec-path-from-powershell--build-env-script
                    names
                    exec-path-from-powershell-includes-visual-studio-environment))
           (raw (exec-path-from-powershell--call-powershell script))
           (json-object-type 'alist)
           (json-key-type 'string)
           (json-null nil))
      (json-read-from-string (substring raw (or (string-match "{\\s-*\"" raw) 0))))))

(advice-add #'exec-path-from-shell-getenvs
            :around
            #'exec-path-from-powershell--getenvs)

(advice-add #'exec-path-from-shell-printf
            :around
            #'exec-path-from-powershell--printf)

(provide 'exec-path-from-powershell)

;;; exec-path-from-powershell.el ends here
