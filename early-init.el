;;; -*- lexical-binding: t -*-
(setq load-prefer-newer t)
(setenv "LSP_USE_PLISTS" "true")
(defvar lsp-use-plists)
(setq lsp-use-plists t)
(when (eq system-type 'android)
  (let ((termuxpath "/data/data/com.termux/files/usr/bin"))
    (setenv "PATH" (concat (getenv "PATH") ":"  termuxpath))
    (add-to-list 'exec-path termuxpath t)))
