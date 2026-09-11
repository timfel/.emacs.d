;;; -*- lexical-binding: t -*-

(require 'use-package)

(use-package gptel
  :ensure t
  :defines (gptel--openai-models)
  :functions (gptel-make-openai-oauth)
  :commands (gptel)
  :pin melpa
  :custom
  (gptel-model 'gpt-5.6-luna)
  (gptel-default-mode 'org-mode)
  (gptel-log-level 'info)
  (gptel-org-branching-context t)
  :bind
  (:prefix-map my-gptel-prefix-map
               :prefix "C-x g"
               :prefix-docstring "GPTel commands"
               ("t" . gptel-org-set-topic)
               ("f" . gptel-fim)
               ("c" . gptel-compile)
               ("s" . gptel-send)
               ("p" . gptel-pi)
               ("b" . gptel-buffers))
  :config
  (require 'gptel-openai)
  (require 'gptel-openai-oauth)
  (require 'gptel-request)

  ;; customize the prompt prefix so it works better with org mode for me
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@llm\n")

  (defun gptel-buffers ()
    (interactive)
    (ibuffer
     t
     "*gptel buffers*"
     '((predicate . (and (boundp 'gptel-mode)
                         gptel-mode)))))

  ;; Backends
  (progn
    (setq gptel-backend
          (gptel-make-openai-oauth "OpenAI" :models gptel--openai-models))
    (dolist (model-effort
             '((gpt-5.6-terra . "high")
               (gpt-5.6-luna  . "high")
               (gpt-5.6-sol   . "high")
               (gpt-5.6-astra . "high")))
      (let ((model (car model-effort))
            (effort (cdr model-effort)))
        (setplist
         model
         (plist-put (symbol-plist model)
                    :request-params
                    `(:reasoning (:effort ,effort
                                          :summary "detailed"))))))

    (gptel-make-openai "llama-cpp"
      :host "127.0.0.1:8080"
      :protocol "http"
      :stream t
      :models '(local-code-model local-chat-model)
      :key "none"))

  ;; Directives
  (setq gptel-directives
        `((default . ,(concat "You are a large language model living in Emacs and a helpful assistant. "
                              "Respond concisely."))
          (code . ,(concat "Continue the code. No markup, do not repeat parts of the request, "
                           "no questions, no explanations, ONLY code.")))))

(use-package gptel-pi
  :commands (gptel-pi))

(use-package gptel-fim
  :commands (gptel-fim))

(use-package gptel-compile
  :commands (gptel-compile))

(use-package gptel-modeline-status
  :after gptel
  :demand t)

(use-package timfel-gptel-orchestration
  :commands timfel/weekly-confluence-report
  :bind (("C-x a m" . timfel/gptel-open-agents-orchestration)))
