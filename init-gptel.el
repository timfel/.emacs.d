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
               ("s" . gptel-send)
               ("b" . gptel-buffers))
  :config
  (require 'gptel-openai)
  (require 'gptel-openai-oauth)
  (require 'gptel-request)

  ;; customize the prompt prefix so it works better with org mode for me
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@llm\n")

  (add-hook 'gptel-mode-hook
            (lambda ()
              (setq-local tool-bar-map (copy-tree (default-value 'tool-bar-map)))
              (define-key-after tool-bar-map [separator-0] menu-bar-separator)
              (tool-bar-local-item "symbols/check-mark_16"
                                   'gptel-send
                                   'send
                                   tool-bar-map)
              (tool-bar-local-item "conceal"
                                   'org-fold-hide-block-all
                                   'hide
                                   tool-bar-map)))

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
                    `(:reasoning (:effort ,effort))))))

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
          (pi . ,(string-join '("You are an expert coding assistant."
                                "You help users with coding tasks by reading files, executing commands,"
                                "editing code, and writing new files."
                                ""
                                "Guidelines:"
                                "- Use `eval` to get recent buffers, kill ring, or other editor state"
                                "- Use `bash` for file operations like ls, grep, find"
                                "- Use `read` to examine files before editing"
                                "- Use `edit` for precise changes"
                                "- Use `write` only for new files or complete rewrites"
                                "- When summarizing your actions, output plain text directly"
                                "- Do NOT use cat or bash to display what you did"
                                "- Be concise in your responses"
                                "\n")))
          (code . ,(concat "Continue the code. No markup, do not repeat parts of the request, "
                           "no questions, no explanations, ONLY code.")))))

(use-package gptel-annotate
  :vc (:url "https://github.com/karthink/gptel-annotate"
            :rev :newest)
  :bind ("C-x g a" . gptel-annotate)
  :after gptel)

(use-package gptel-preset-collection
  :vc (:url "https://github.com/karthink/gptel-preset-collection"
       :rev :newest)
  :after gptel)

(use-package gptel-pi
  :after gptel
  :demand t
  :commands (gptel-pi)
  :bind (:map my-gptel-prefix-map
              ("p" . gptel-pi)))

(use-package gptel-fim
  :after gptel
  :commands (gptel-fim)
  :bind (:map my-gptel-prefix-map
              ("f" . gptel-fim)))

(use-package gptel-compile
  :after gptel
  :commands (gptel-compile)
  :bind (:map my-gptel-prefix-map
              ("c" . gptel-compile)))

(use-package gptel-modeline-status
  :after gptel
  :demand t)

(use-package timfel-gptel-orchestration
  :commands timfel/weekly-confluence-report
  :bind (("C-x a m" . timfel/gptel-open-agents-orchestration)))
