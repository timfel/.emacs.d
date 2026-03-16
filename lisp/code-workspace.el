;;; code-workspace.el --- Project support for VS Code workspaces  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;;   A project.el plugin that looks for VSCode workspaces to determine
;;;   external project roots in addition to the vc-root.
;;;
;;; Code:

(require 'project)
(require 'json)
(require 'subr-x)

(defvar code-workspace--cache (make-hash-table :test #'equal)
  "Cache keyed by VC root.

Each value is a cons cell (MTIMES . CANDIDATES).
MTIMES is an alist (WORKSPACE-FILE . MODTIME).
CANDIDATES is a list of plists with keys:
  :workspace-file absolute path
  :folders list of absolute directory names (as directories).")

(defvar code-workspace--eglot-buffer-count 0
  "We refcount active eglot buffers for our projects.

If this reaches 0, we flush code-workspace--vc-root-to-project-cache so
that e.g. when we want to switch between overlapping code-workspaces, we
can do so after the last eglot-managed buffer is killed for the previous
code-workspace.")

(defvar code-workspace--vc-root-to-project-cache (make-hash-table :test #'equal)
  "Cache from vc-root to single code-workspace project")

(defun code-workspace--vc-root (dir)
  (when-let ((vc-proj (project-try-vc dir)))
    (file-name-as-directory (expand-file-name (project-root vc-proj)))))

(defun code-workspace--dir-files (dir)
  (when (and dir (file-directory-p dir))
    (directory-files dir t "\\.code-workspace\\'")))

(defun code-workspace--list-workspaces (vc-root)
  (let* ((vc-root (and vc-root (file-name-as-directory (expand-file-name vc-root))))
         (parent (and vc-root
                      (file-name-parent-directory (directory-file-name vc-root))))
         (files (append (code-workspace--dir-files vc-root)
                        (code-workspace--dir-files parent))))
    (seq-filter #'file-regular-p (delete-dups files))))

(defun code-workspace--mtime (file)
  (file-attribute-modification-time (file-attributes file)))

(defun code-workspace--json-read-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (json-parse-buffer :object-type 'hash-table
                       :array-type 'list
                       :null-object nil
                       :false-object :json-false)))

(defun code-workspace--resolve-folder (workspace-file folder-path)
  (let* ((base (file-name-directory (expand-file-name workspace-file)))
         (abs (if (file-name-absolute-p folder-path)
                  folder-path
                (expand-file-name folder-path base))))
    (file-name-as-directory (expand-file-name abs))))

(defun code-workspace--parse (workspace-file)
  (condition-case _
      (let* ((obj (code-workspace--json-read-file workspace-file))
             (folders (and (hash-table-p obj) (gethash "folders" obj)))
             (paths
              (seq-keep (lambda (elt)
                          (when (hash-table-p elt)
                            (when-let ((p (gethash "path" elt)))
                              (and (stringp p)
                                   (code-workspace--resolve-folder workspace-file p)))))
                        folders)))
        (delete-dups paths))
    (error nil)))

(defun code-workspace--buffer-path (buf)
  (with-current-buffer buf
    (cond
     ((buffer-file-name buf) (expand-file-name (buffer-file-name buf)))
     ((and (boundp 'default-directory) default-directory)
      (file-name-as-directory (expand-file-name default-directory)))
     (t nil))))

(defun code-workspace--normalize-path (path)
  (when path
    (let ((expanded (expand-file-name path)))
      (if (file-remote-p expanded)
          expanded
        (condition-case _
            (file-truename expanded)
          (error expanded))))))

(defun code-workspace--path-under-dir-p (path dir)
  (let* ((dir (file-name-as-directory dir)))
    (string-prefix-p dir path)))

(defun code-workspace--path-under-any-root-p (path roots)
  (seq-some (lambda (root) (code-workspace--path-under-dir-p path root))
            roots))

(defun code-workspace--score-workspace (roots)
  (let* ((roots (mapcar #'code-workspace--normalize-path roots))
         (roots (seq-filter #'identity roots))
         (score 0))
    (dolist (buf (buffer-list))
      (when-let ((p (code-workspace--buffer-path buf)))
        (let ((p (code-workspace--normalize-path p)))
          (when (and p (code-workspace--path-under-any-root-p p roots))
            (setq score (1+ score))))))
    score))

(defun code-workspace--target-path-for-dir (dir)
  (or (and (buffer-file-name) (expand-file-name (buffer-file-name)))
      (file-name-as-directory (expand-file-name dir))))

(defun code-workspace--workspace-matches-path-p (workspace path)
  (let* ((path (code-workspace--normalize-path path))
         (roots (mapcar #'code-workspace--normalize-path (plist-get workspace :folders)))
         (roots (seq-filter #'identity roots)))
    (and path roots (code-workspace--path-under-any-root-p path roots))))

(defun code-workspace--prefer-in-vc-root-p (workspace vc-root)
  (let* ((wf (plist-get workspace :workspace-file))
         (wf-dir (file-name-as-directory (expand-file-name (file-name-directory wf))))
         (vc-root (file-name-as-directory (expand-file-name vc-root))))
    (string= wf-dir vc-root)))

(defun code-workspace--choose-best (target-path candidates vc-root)
  (let* ((matching (seq-filter (lambda (ws)
                                (code-workspace--workspace-matches-path-p ws target-path))
                              candidates)))
    (when matching
      (car
       (sort matching
             (lambda (a b)
               (let* ((aroots (plist-get a :folders))
                      (broots (plist-get b :folders))
                      (ascore (code-workspace--score-workspace aroots))
                      (bscore (code-workspace--score-workspace broots)))
                 (cond
                  ((/= ascore bscore) (> ascore bscore))
                  ((/= (if (code-workspace--prefer-in-vc-root-p a vc-root) 1 0)
                       (if (code-workspace--prefer-in-vc-root-p b vc-root) 1 0))
                   (code-workspace--prefer-in-vc-root-p a vc-root))
                  (t
                   (string< (file-name-nondirectory (plist-get a :workspace-file))
                            (file-name-nondirectory (plist-get b :workspace-file))))))))))))

(defun code-workspace--cached-candidates (vc-root)
  (let* ((files (code-workspace--list-workspaces vc-root))
         (mtimes (mapcar (lambda (f) (cons f (code-workspace--mtime f))) files))
         (cached (gethash vc-root code-workspace--cache)))
    (if (and cached (equal (car cached) mtimes))
        (cdr cached)
      (let ((cands
             (seq-keep (lambda (wf)
                         (when-let ((folders (code-workspace--parse wf)))
                           (and folders
                                (list :workspace-file wf
                                      :folders folders))))
                       files)))
        (puthash vc-root (cons mtimes cands) code-workspace--cache)
        cands))))

;;;###autoload
(defun code-workspace-debug-current ()
  "Print code-workspace discovery details for the current buffer.

This is a troubleshooting helper for `project-try-code-workspace'."
  (interactive)
  (let* ((dir default-directory)
         (vc-root (code-workspace--vc-root dir))
         (target (code-workspace--target-path-for-dir dir)))
    (unless vc-root
      (user-error "No VC root detected for %s" dir))
    (let* ((cands (code-workspace--cached-candidates vc-root))
           (chosen (code-workspace--choose-best target cands vc-root))
           (buf (get-buffer-create "*code-workspace-debug*")))
      (with-current-buffer buf
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert (format "vc-root:   %s\n" vc-root))
        (insert (format "target:    %s\n\n" target))
        (insert (format "candidates (%d):\n" (length cands)))
        (dolist (ws cands)
          (let* ((wf (plist-get ws :workspace-file))
                 (roots (plist-get ws :folders))
                 (matches (code-workspace--workspace-matches-path-p ws target))
                 (score (code-workspace--score-workspace roots))
                 (prefer (code-workspace--prefer-in-vc-root-p ws vc-root)))
            (insert (format "- %s\n" wf))
            (insert (format "  roots:   %d\n" (length roots)))
            (insert (format "  matches: %s\n" (if matches "yes" "no")))
            (insert (format "  score:   %d\n" score))
            (insert (format "  in-root: %s\n" (if prefer "yes" "no")))))
        (insert "\n")
        (if chosen
            (insert (format "chosen:    %s\n" (plist-get chosen :workspace-file)))
          (insert "chosen:    (none)\n"))
        (setq buffer-read-only t)
        (goto-char (point-min)))
      (display-buffer buf))))

;;;###autoload
(defun project-try-code-workspace (dir)
  (if-let ((vc-root (code-workspace--vc-root dir)))
      (if-let* ((cached-project (gethash vc-root code-workspace--vc-root-to-project-cache)))
          ;; If we have a cached project for this vc-root, return that
          cached-project
        (when-let* ((cands (code-workspace--cached-candidates vc-root))
                    (target (code-workspace--target-path-for-dir dir))
                    (chosen (code-workspace--choose-best target cands vc-root))
                    (folders (plist-get chosen :folders)))
          ;; If we found a good code-workspace file, make a new project
          (list 'code-workspace
                (list :workspace-file (plist-get chosen :workspace-file)
                      :folders folders
                      :root vc-root))))))

(defun code-workspace--remove-eglot-buffer-hook ()
  "When the eglot managed buffer count falls to 0, clear the project cache"
  (setq code-workspace--eglot-buffer-count (max 0 (- code-workspace--eglot-buffer-count 1)))
  (if (= 0 code-workspace--eglot-buffer-count)
      (clrhash code-workspace--vc-root-to-project-cache)))

(defun code-workspace--track-eglot-buffer ()
  (if-let ((project (project-current)))
      (when (eq 'code-workspace (car project))
        (if (eglot-managed-p)
            ;; if eglot starts managing one of our buffers, increment the count and add a kill hook
            (progn
              (setq code-workspace--eglot-buffer-count (+ code-workspace--eglot-buffer-count 1))
              (when (= code-workspace--eglot-buffer-count 1)
                ;; if we just started tracking the very first one, make sure to
                ;; fill the cache for this project, so subsequent files that
                ;; are opened will hit the cache and eglot will manage them in
                ;; the same project
                (let ((folders (plist-get (nth 1 project) :folders)))
                  ;; for all the folders in the code-workspace, we link their vc-roots to this new project,
                  ;; so in files in the external-roots we still return the same cached project
                  (dolist (folder folders)
                    (when-let ((folder-vc-root (code-workspace--vc-root folder)))
                      (puthash folder-vc-root project code-workspace--vc-root-to-project-cache)))))
              (add-hook 'kill-buffer-hook #'code-workspace--remove-eglot-buffer-hook 0 t))
          ;; if eglot stops managing one of our buffers, run and then remove the kill hook
          (remove-hook 'kill-buffer-hook #'code-workspace--remove-eglot-buffer-hook t)
          (code-workspace--remove-eglot-buffer-hook)))))

(add-hook 'eglot-managed-mode-hook #'code-workspace--track-eglot-buffer)

(add-hook 'project-find-functions #'project-try-code-workspace)

(cl-defmethod project-root ((project (head code-workspace)))
  (plist-get (nth 1 project) :root))

(cl-defmethod project-external-roots ((project (head code-workspace)))
  (plist-get (nth 1 project) :folders))

(cl-defmethod project-name ((project (head code-workspace)))
  (file-name-nondirectory (plist-get (nth 1 project) :workspace-file)))

(cl-defmethod project-buffers ((project (head code-workspace)))
  (let* ((roots (cons (project-root project)
                      (project-external-roots project)))
         (roots (seq-filter #'identity roots))
         (roots (mapcar (lambda (r)
                          (file-name-as-directory (expand-file-name r)))
                        roots))
         bufs)
    (dolist (buf (buffer-list))
      (when-let ((p (code-workspace--buffer-path buf)))
        (let ((p (expand-file-name p)))
          (when (code-workspace--path-under-any-root-p p roots)
            (push buf bufs)))))
    (nreverse bufs)))

(provide 'code-workspace)

;;; code-workspace.el ends here
