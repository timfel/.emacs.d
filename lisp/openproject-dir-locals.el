;;; openproject-dir-locals.el -*- lexical-binding: t; -*-

(require 'files)

(defvar inf-ruby-console-environment)

(defun openproject-rails-console ()
  (interactive)
  (let ((inf-ruby-console-environment "development"))
    (inf-ruby-console-auto)))

(defun openproject-rspec-current ()
  (interactive)
  (let* ((file (buffer-file-name))
         (root (locate-dominating-file file ".env")))
    (unless root
      (user-error "Could not find the OpenProject project root"))
    (compile
     (format
      "docker compose exec -T backend-test bundle exec rspec %s"
      (shell-quote-argument
       (file-relative-name file root))))))

(dir-locals-set-class-variables
 'openproject-docker
 '((nil
    (compile-command . "bin/compose rspec ")
    (ruby-indent-level . 2)
    (ruby-shell-interpreter . "docker")
    (ruby-shell-interpreter-args
     . "compose exec -T backend bundle exec ruby")
    (inf-ruby-wrapper-command
     . "docker compose exec -T backend %s")
    (rspec-use-bundler . nil)
    (rspec-command . "bin/compose rspec")
    (rspec-spec-command . "bin/compose rspec %s")
    (rspec-use-rake-when-possible . nil))

   (ruby-base-mode
    (eval
     . (progn
         (local-set-key (kbd "C-c C-r") #'openproject-rails-console)
         (local-set-key (kbd "C-c C-t") #'openproject-rspec-current))))))

;; Change this to your actual checkout.
(dir-locals-set-directory-class
 (expand-file-name "~/dev/openproject/")
 'openproject-docker)

(provide 'openproject-dir-locals)
