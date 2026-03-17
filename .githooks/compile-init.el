;;; compile-init.el --- minimal init for batch byte-compilation  -*- lexical-binding: t; -*-

;; Keep this file small and side-effect free: it exists to make `require' work
;; during `emacs -Q --batch' byte-compilation.

(setq user-emacs-directory
      (file-name-as-directory
       (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))
(setq load-prefer-newer t
      ad-redefinition-action 'accept)

;; Activate installed package.el packages so their directories are on `load-path'.
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("cselpa" . "https://elpa.thecybershadow.net/packages/"))
(setq package-archive-priorities
      '(("melpa-stable" . 10)
        ("nongnu" . 5)
        ("gnu" . 5)
        ("melpa" . 1)
        ("cselpa" . 0)))
(package-initialize)

;; These are Oracle libraries that are not available everywhere
(provide 'emacs-ci)
(provide 'oca)
(provide 'orcl)

;; Add local lisp directories.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(provide 'compile-init)
;;; compile-init.el ends here
