;;; timfel-agent-shell-ui.el --- Agent-shell UI helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Window layout helpers for agent-shell buffers.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'agent-shell)

;;;###autoload
(defun timfel/agent-shell-tile-buffers-grid (&optional prefix)
  "Tile live `agent-shell' buffers in the selected frame as a grid.

With no PREFIX, tile all live agent-shell buffers.
With a PREFIX other than numeric 1, tile only busy agent-shell buffers.
With numeric PREFIX 1, tile only idle agent-shell buffers."
  (interactive "P")
  (let* ((buffers
          (let ((all-buffers
                 (sort (seq-filter #'buffer-live-p (agent-shell-buffers))
                       (lambda (left right)
                         (string-lessp (buffer-name left)
                                       (buffer-name right))))))
            (if prefix
                (seq-filter
                 (if (= (prefix-numeric-value prefix) 1)
                     (lambda (buffer)
                       (with-current-buffer buffer
                         (not (and (derived-mode-p 'agent-shell-mode)
                                   (fboundp 'shell-maker-busy)
                                   (shell-maker-busy)))))
                   (lambda (buffer)
                     (with-current-buffer buffer
                       (and (derived-mode-p 'agent-shell-mode)
                            (fboundp 'shell-maker-busy)
                            (shell-maker-busy)))))
                 all-buffers)
              all-buffers)))
         (count (length buffers)))
    (if (zerop count)
        (message
         (cond
          ((not prefix) "No live agent-shell buffers")
          ((= (prefix-numeric-value prefix) 1)
           "No idle agent-shell buffers")
          (t
           "No busy agent-shell buffers")))
      (let* ((columns (max 1 (ceiling (sqrt count))))
             (rows (max 1 (ceiling (/ (float count) columns)))))
        (let ((root (selected-window))
              (column-windows (list (selected-window)))
              all-windows)
          (delete-other-windows root)
          (dotimes (_ (1- columns))
            (setq column-windows
                  (append column-windows
                          (list (split-window (car (last column-windows))
                                              nil 'right)))))
          (dolist (column-window column-windows)
            (let ((windows-in-column (list column-window)))
              (dotimes (_ (1- rows))
                (setq windows-in-column
                      (append windows-in-column
                              (list (split-window
                                     (car (last windows-in-column))
                                     nil 'below)))))
              (setq all-windows (append all-windows windows-in-column))))
          (cl-mapc #'set-window-buffer all-windows buffers)
          (dolist (window (nthcdr count all-windows))
            (when (window-live-p window)
              (delete-window window)))
          (balance-windows-area)
          (select-window (car (window-list (selected-frame) 'nomini))))))))

(provide 'timfel-agent-shell-ui)

;;; timfel-agent-shell-ui.el ends here
