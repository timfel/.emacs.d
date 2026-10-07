;;; openproject-dir-locals.el -*- lexical-binding: t; -*-

(require 'files)

(defvar inf-ruby-console-environment)

(defcustom openproject-dev-setup
  nil
  "Selects docker-based (t) or bare-metal setup (nil)"
  :type '(radio (const :tag "Docker using bin/compose" t)
                (const :tag "Bare metal setup" nil)))

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
     (concat
      (if openproject-dev-setup "compose exec -T backend-test " "")
      (format
       "bundle exec rspec %s"
       (shell-quote-argument
        (file-relative-name file root)))))))

(dir-locals-set-class-variables
 'openproject-docker
 `((nil
    (compile-command . ,(if openproject-dev-setup "bin/compose rspec " "rspec "))
    (ruby-indent-level . 2)
    (ruby-shell-interpreter . ,(if openproject-dev-setup "docker" "ruby"))
    (ruby-shell-interpreter-args
     . ,(if openproject-dev-setup "compose exec -T backend bundle exec ruby" ""))
    (inf-ruby-wrapper-command
     . ,(if openproject-dev-setup "docker compose exec -T backend %s" nil))
    (rspec-use-bundler . nil)
    (rspec-command . ,(if openproject-dev-setup "bin/compose rspec" "bundle exec rspec"))
    (rspec-spec-command . ,(if openproject-dev-setup "bin/compose rspec %s" "bundle exec rspec %s"))
    (rspec-use-rake-when-possible . nil))

   (ruby-base-mode
    (eval
     . (progn
         (local-set-key (kbd "C-c C-r") #'openproject-rails-console)
         (local-set-key (kbd "C-c C-t") #'openproject-rspec-current))))))

(setopt safe-local-variable-values
        ;; concat to existing safe-local-variable-values
        (seq-uniq (append safe-local-variable-values
                          '((rspec-use-rake-when-possible)
                            (rspec-use-bundler)
                            (ruby-shell-interpreter-args . "compose exec -T backend bundle exec ruby")
                            (ruby-shell-interpreter-args . "")
                            (ruby-shell-interpreter . "docker")
                            (ruby-shell-interpreter . "ruby")
                            (ruby-indent-level . 2)
                            (inf-ruby-wrapper-command . "docker compose exec -T backend %s")
                            (inf-ruby-wrapper-command . nil)
                            (rspec-command . "bin/compose rspec")
                            (rspec-command . "bundle exec rspec")
                            (rspec-spec-command . "bin/compose rspec %s")
                            (rspec-spec-command . "bundle exec rspec %s")))))

;; Change this to your actual checkout.
(dir-locals-set-directory-class
 (expand-file-name "~/dev/openproject/")
 'openproject-docker)

(dir-locals-set-directory-class
 (expand-file-name "~/opf/")
 'openproject-docker)

(provide 'openproject-dir-locals)
