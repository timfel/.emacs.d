;;; org-from-ical.el --- Import remote calendars into Org -*- lexical-binding: t; -*-

(require 'url)

(defgroup org-from-ical nil
  "Import iCalendar URLs into Org files."
  :group 'org)

(defcustom org-from-ical-alist nil
  "Alist of iCalendar URLs and destination Org files."
  :type '(alist :key-type string :value-type file)
  :group 'org-from-ical)

(defun org-from-ical ()
  "Asynchronously fetch and convert all calendars in `org-from-ical-alist'."
  (dolist (calendar org-from-ical-alist)
    (let ((url (car calendar))
          (file (expand-file-name (cdr calendar))))
      (url-retrieve
       url
       (lambda (status)
         (unwind-protect
             (unless (plist-get status :error)
               (goto-char (point-min))
               (when (re-search-forward "\r?\n\r?\n" nil t)
                 (let ((process
                        (make-process
                         :name (format "org-from-ical-%s" (gensym))
                         :command (list "ical2orgpy" "-" file)
                         :connection-type 'pipe
                         :noquery t
                         :sentinel
                         (lambda (process _event)
                           (when (and (eq (process-status process) 'exit)
                                      (zerop (process-exit-status process)))
                             (when-let* ((buffer (find-buffer-visiting file)))
                               (with-current-buffer buffer
                                 (unless (buffer-modified-p)
                                   (revert-buffer t t t)))))))))
                   (process-send-region process (point) (point-max))
                   (process-send-eof process))))
           (kill-buffer (current-buffer))))
       nil t))))

(provide 'org-from-ical)
;;; org-from-ical.el ends here
