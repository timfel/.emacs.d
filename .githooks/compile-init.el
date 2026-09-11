;;; compile-init.el --- minimal init for batch byte-compilation  -*- lexical-binding: t; -*-

;; Keep this file small and side-effect free: it exists to make `require' work
;; during `emacs -q --batch' byte-compilation.

(setq user-emacs-directory
      (file-name-as-directory
       (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))
(setq load-prefer-newer t
      ad-redefinition-action 'accept)

;; Activate installed package.el packages so their directories are on `load-path'.
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(require 'package)
(require 'cl-lib)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("cselpa" . "https://elpa.thecybershadow.net/packages/"))
(setq package-archive-priorities
      '(("melpa" . 10)
        ("nongnu" . 5)
        ("gnu" . 5)
        ("melpa-stable" . 1)
        ("cselpa" . 0)))
;; Read package descriptors without activating them first.  A package can have
;; a malformed self-dependency in its generated descriptor; activating that
;; descriptor recursively otherwise produces a noisy max-lisp-eval-depth error
;; and prevents the package from being available to the compiler.
(package-initialize t)
(dolist (entry package-alist)
  (dolist (desc (cdr entry))
    (setf (package-desc-reqs desc)
          (cl-remove-if (lambda (requirement)
                          (eq (car requirement) (package-desc-name desc)))
                        (package-desc-reqs desc)))))
(package-activate-all)
(when (file-directory-p (expand-file-name "archives" package-user-dir))
  (condition-case nil
      (package-read-all-archive-contents)
    (error nil)))

;; These are Oracle libraries that are not available everywhere
(provide 'emacs-ci)
(provide 'oca)
(provide 'orcl)

;; Add local lisp directories.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(provide 'compile-init)
;;; compile-init.el ends here
