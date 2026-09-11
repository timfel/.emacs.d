;;; gptel-fim.el --- GPTel configuration to fill code in the middle -*- lexical-binding: t -*-

;; Copyright (C) 2026 Tim Felgentreff <timfelgentreff@gmail.com>
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, version 3.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see https://www.gnu.org/licenses/.

;;; Author: Tim Felgentreff <timfelgentreff@gmail.com>

;;; Code:

(require 'gptel)
(require 'gptel-request)

(defconst gptel-fim-prefix "<|fim_prefix|>")
(defconst gptel-fim-suffix "<|fim_suffix|>")
(defconst gptel-fim-middle "<|fim_middle|>")
(defconst gptel-fim-file-separator "<|file_separator|>")

(defun gptel-fim--strip-terminators (response)
  "Return RESPONSE up to the first FIM control token.

FIM models use these tokens as generation terminators.  Stop at all of
them in case a server returns a terminator as ordinary response text."
  (let ((terminators (regexp-opt
                      (list gptel-fim-prefix
                            gptel-fim-suffix
                            gptel-fim-middle
                            gptel-fim-file-separator))))
    (if-let* ((position (string-match terminators response)))
        (substring response 0 position)
      response)))

(defun gptel-fim ()
  "Replace the active region, or insert at point, with a FIM completion."
  (interactive)
  (let* ((buffer (current-buffer))
         (gap-start (copy-marker (if (use-region-p) (region-beginning) (point))))
         (gap-end (copy-marker (if (use-region-p) (region-end) (point)) t))
         (prefix (buffer-substring-no-properties (max (point-min) (- gap-start 1000)) gap-start))
         (suffix (buffer-substring-no-properties gap-end (min (point-max) (+ gap-start 1000))))
         (prompt (concat gptel-fim-prefix prefix
                         gptel-fim-suffix suffix
                         gptel-fim-middle))
         ;; Do not add chat context, tools, reasoning, or instructions to a FIM prompt.
         (gptel-use-context nil)
         (gptel-use-tools nil)
         (gptel-tools nil)
         (gptel-include-reasoning nil)
         (gptel-temperature 0.0)
         (gptel-stream t)
         (response-parts nil))
    ;; Buffer streaming output so terminators split across chunks are removed.
    (gptel-request
        prompt
      :buffer buffer
      :position gap-start
      :stream t
      ;; This is a pattern on which actual FIM models are trained, but more generic models
      ;; can also generally deal with this, so I put it in the system prompt
      :system (concat "Perform fill in the middle (FIM) completion and NO extra output. "
                      "`<|fim_prefix|>` is before the code, <|fim_suffix|> is where the "
                      "cursor is, <|fim_middle|> is the end of the request.")
      :callback (lambda (response info)
                  (cond
                   ((stringp response)
                    (push response response-parts))
                   ;; Reasoning arrives on the streaming callback even when
                   ;; `gptel-include-reasoning' is nil.  It is metadata, not a
                   ;; failed response, so leave the insertion markers intact.
                   ((eq (car-safe response) 'reasoning))
                   ((eq response t)
                    (unwind-protect
                        (when (buffer-live-p buffer)
                          (with-current-buffer buffer
                            (when (and (marker-position gap-start)
                                       (marker-position gap-end))
                              (save-excursion
                                (goto-char gap-start)
                                (delete-region gap-start gap-end)
                                (insert (gptel-fim--strip-terminators
                                         (apply #'concat (nreverse response-parts))))))))
                      (set-marker gap-start nil)
                      (set-marker gap-end nil)))
                   (t
                    (set-marker gap-start nil)
                    (set-marker gap-end nil)
                    (message "FIM completion failed: %s" (plist-get info :status))))))))

(provide 'gptel-fim)
