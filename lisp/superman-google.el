;;; superman-google.el --- superman has google

;; Copyright (C) 2013  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds <tag@biostat.ku.dk>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; If you capture a meeting in this form
;;
;; ,----
;; |  *** iconic Batgirl
;; |  :PROPERTIES:
;; |  :Date:   <1973-01-18 Thu 11:00>
;; |  :Participants: Robin, WonderWoman
;; |  :CaptureDate: <2013-01-16 Wed>
;; |  :GoogleCalendar: Bat
;; |  :Status: confirmed
;; |  :END:
;; `----
;;
;; and you also have
;;
;; (add-hook
;; 'org-capture-before-finalize-hook
;; 'org-google-export-appointment)
;; 
;; then finalizing the capture \C-c \C-c
;; will prompt you for exporting the appointment
;; to your calendar "Bat"
;;
;; If property GoogleCalendar is not set,
;; the prompt uses your
;; superman-google-default-calendar
;;
;; ADVARSEL: non-standard letters like ø or ä
;;           can easily destroy the transporter
;;
;;; Code:

(defvar superman-google-calendars
  nil
  "List of google calendar names. E.g. '(\"Work\" \"Family\").")

(defvar superman-google-default-calendar
  nil
  "Name of your default google calendar.")

(defun superman-google-export-appointment (&optional set)
  (interactive "P")
  (org-back-to-heading t)
  (let ((g-cal (org-entry-get nil "GoogleCalendar")))
    (if (or g-cal
            (and set
                 (progn
                   (setq g-cal
                         (completing-read
			  (concat "Choose google calendar (default: " superman-google-default-calendar "): ")
			  (mapcar '(lambda (entry) (cons entry nil)) superman-google-calendars)
			  nil t nil nil
			  superman-google-default-calendar))
                   (org-set-property "GoogleCalendar" g-cal)
                   g-cal)))
        ;; search for time-stamp
        (if (and (not (org-at-timestamp-p nil))
                 (progn
                   (re-search-forward ":Schedule:" nil t)
                   (re-search-forward ">" nil t)
                   (not (org-at-timestamp-p nil))))
            (message "Cursor not in time-stamp")
          (save-excursion
            (beginning-of-line)
            (re-search-forward "<" nil t)
            (or (looking-at "\\(\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) *\\([^]+0-9>\r\n -]*\\)\\( \\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\)?-?--?\\(\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\)>\\)")
                (looking-at "\\(\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) *\\([^]+0-9>\r\n -]*\\)\\( \\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\)>\\)")
                (looking-at "\\(\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) *\\([^]+0-9>\r\n -]*\\)>\\)"))
            (let* ((g-date (format "%s/%s/%s" 
                                   (match-string-no-properties 2)
                                   (match-string-no-properties 3)
                                   (match-string-no-properties 4)))
                   (g-start (match-string-no-properties 6))
                   (g-stop (match-string-no-properties 9))
                   (g-time (if g-stop
                               (format " from %s to %s"
                                       (org-agenda-time-of-day-to-ampm g-start)
                                       (org-agenda-time-of-day-to-ampm g-stop))
                             (format " from %s to %s"
                                     (org-agenda-time-of-day-to-ampm g-start)
                                     (org-agenda-time-of-day-to-ampm
                                      (concat (number-to-string
                                               (+ 1 (string-to-number
                                                     (match-string-no-properties 7))))
                                              ":"
                                              (match-string-no-properties 8))))))
                   (text (progn (outline-previous-heading)
                                (looking-at org-complex-heading-regexp)
                                (match-string-no-properties 4))))
              (let* ((g-command (concat "/usr/bin/google calendar add --cal \"" g-cal "\" \"" text " on " g-date g-time "\""))
                     (g-doit (y-or-n-p (concat "Add to google calendar?: " g-command))))
                (when g-doit
                  (shell-command g-command)))))))))

(provide 'superman-google)
;;; superman-google.el ends here


