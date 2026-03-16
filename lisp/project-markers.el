;;; project-markers.el --- Project support via marker files  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;;   A project.el plugin that looks for maker files.
;;;
;;; Code:

(require 'project)

(defcustom project-markers-filenames
  '("Cargo.toml" "compile_commands.json" "compile_flags.txt"
    "pyproject.toml" ".venv" "setup.py" "pyrightconfig.json")
  "Files or directories that indicate the root of a project."
  :type '(repeat string)
  :group 'project)

(defun project-markers--project-root-p (path)
  "Check if the current PATH has any of the project root markers."
  (catch 'found
    (dolist (marker project-markers-filenames)
      (when (file-exists-p (concat path marker))
        (throw 'found marker)))))

(defun project-markers-find-root (path)
  "Search up the PATH for `project-markers-filenames'."
  (when-let ((root (locate-dominating-file path #'project-markers--project-root-p)))
    (cons 'transient (expand-file-name root))))

;;;###autoload
(add-hook 'project-find-functions #'project-markers-find-root)

(provide 'project-markers)

;;; project-markers.el ends here
