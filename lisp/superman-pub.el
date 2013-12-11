;;; superman-pub.el --- Superman views of project contents 

;; Copyright (C) 2013  Thomas Alexander Gerds, Klaus Kaehler Holst

;; Authors: Thomas Alexander Gerds <tag@biostat.ku.dk>
;;          Klaus Kaehler Holst <kkho@biostat.ku.dk>
;; Keywords: tools

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

;; Code:

(defun superman-bibtex-parse-bibtex ()
  (interactive)
  (let* ((bibfile (buffer-file-name (current-buffer)))
	(orgfile  (concat (file-name-sans-extension bibfile) ".org")))
  (goto-char (point-max))
  (while (re-search-backward "@" nil t)
    (ignore-errors
      (superman-bibtex-parse-entry)))
  (when (file-exists-p orgfile)
    (delete-file orgfile))
  (write-file orgfile)
  (org-mode)))
    

(defun superman-bibtex-parse-entry (&optional pom)
  (interactive)
  (org-with-point-at (or pom (point))
    (let* ((start (bibtex-beginning-of-entry))
	   (end (bibtex-end-of-entry))
	   (type (progn
		   (goto-char start)
		   (looking-at bibtex-entry-head)
		   (setq type (match-string-no-properties 1))))
	   (bibkey (match-string-no-properties 2))
	   (bib  (buffer-substring start end))
	   (plain (ignore-errors (superman-bibtex2text bib)))
	   done
	   previous
	   next
	   fields)
      (goto-char start)
      (while (and (not done)
		  (setq next (bibtex-next-field 1))
		  (not (string= next previous))
		  (not (string= next "Entry key")))
	(let* ((field-info (bibtex-find-text-internal t nil t))
	       (field-key (when field-info
			    (downcase
			     (car field-info))))
	       (end-of-field (nth 2 field-info))
	       (field-val (when field-info
			    (replace-regexp-in-string "[\n}{]*" ""
						      (buffer-substring-no-properties
						       (nth 1 field-info)
						       end-of-field)))))
	  (setq fields
		(append fields
			(list (cons field-key field-val))))
	  (setq previous next)
	  (if (> (point) end)
	      (setq done t))))
      (goto-char start)
      (insert "*** " bibkey "\n" ":PROPERTIES:\n")
      (while fields
	(let ((prop (caar fields))
	      (val (cdar fields)))
	  (insert ":" prop ": ")
	  (insert val "\n")
	  (setq fields (cdr fields))))
      (insert ":END:\n")
      (when plain
	(insert "\n**** Plain\n"
		(replace-regexp-in-string "\\[1\\]" ""
					  (car (split-string plain "=+")))))
      (insert "\n**** BibTeX \n"))))
      
  
(defun superman-bibtex2text (bib-string)
  (interactive)
  (save-window-excursion
    (find-file "/tmp/superman-pub.bib")
    (erase-buffer)
    (insert bib-string)
    (save-buffer)
    (shell-command  "bibtex2html -nobibsource -o /tmp/superman-pub /tmp/superman-pub.bib")
    (shell-command-to-string "html2text -ascii /tmp/superman-pub.html")))
  
(defun superman-show-bibtex (&optional marker &rest args)
  (org-with-point-at (or marker (point))
    (save-restriction
      (org-narrow-to-subtree)
      (re-search-forward "BibTeX" nil t)
      (forward-line 1)
      (replace-regexp-in-string
       "^[ \t]*" ""
       (buffer-substring
       (point)
       (progn (forward-paragraph) (point)))))))

(defun superman-show-plain (&optional marker &rest args)
  "Show citation in plain format."
  (if (markerp marker)
      (let* ((pdf (superman-get-property marker "pdf"))
	     (pdf-string (when pdf
			   (string-match org-bracket-link-regexp pdf)
			   (org-make-link-string
			    (org-match-string-no-properties 1 pdf)
			    "pdf")))
	     (plain (org-with-point-at marker
		      (save-restriction
			(org-narrow-to-subtree)
			(re-search-forward "Plain" nil t)
			(forward-line 1)
			(replace-regexp-in-string
			 "^[ \t]*" ""
			 (buffer-substring
			  (point)
			  (progn (forward-paragraph) (point))))))))
	(if pdf
	    (concat plain pdf-string)
	  plain))
    marker))

(defun superman-pub-make-sort-buttons (&optional names)
  (let ((names (or names '("year" "author" "journal" "title"))))
    (while names
      (let* ((name (car names))
	     (sort-cmd (concat "sort-publications-by-" name))
	     (map (make-sparse-keymap)))
	(define-key map [mouse-2]
	  `(lambda () (interactive)
	     (superman-sort-publications-by ,name)))
	(define-key map [return]
	  `(lambda () (interactive)
	     (superman-sort-publications-by ,name)))
	(add-text-properties
	 0 (length name) 
	 (list
	  'button (list t)
	  'face 'superman-warning-face
	  'keymap map
	  'mouse-face 'highlight
	  'follow-link t
	  'help-echo sort-cmd)
	 name)
	(insert name " "))
      (setq names (cdr names)))))

(defun superman-sort-publications-by (&optional column)
  "Idea is simple but not very efficient:
first produce a single column view of the section, then
sort the section, and finally play the original balls.
"
  (let ((cat-head (superman-cat-point))
	(column (or column "year"))
	(buffer-read-only nil))
    (if (not cat-head)
	(message "Nothing to sort here")
      (let ((cat-tail (or (next-single-property-change (point) 'cat) (point-max)))
	    (balls (get-text-property cat-head 'balls))
	    (temp-balls `((,column ("width" 4)))))
	;; (delete-region cat-head cat-tail)
	(save-excursion
	  (superman-change-balls temp-balls)
	  (superman-refresh-cat temp-balls)
	  (goto-char (previous-single-property-change (point) 'names))
	  (superman-sort-section)
	  (superman-change-balls balls)
	  (superman-refresh-cat balls))))))
				 
  

(provide 'superman-pub)

;;; superman-pub.el ends here
