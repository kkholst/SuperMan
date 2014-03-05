;;; superman-export.el --- Buttonized org to latex export

;; Copyright (C) 2014  Thomas Alexander Gerds

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

;; Org provides a series of export functions that are invoked
;; via C-c C-e. The case where an org document
;; is exported to pdf via latex is special because the exported
;; latex document needs to be compiled and thus errors can occur
;; on different stages. The superman feature defined below
;; provides a control buffer with buttons which guide to:
;;
;;  - the next LaTeX error
;;  - the first LaTeX error
;; 
;; and for documents that include R-code there are further buttons
;; to find the next/previous error in the inferior ESS buffer.
;; 
;;

;;; Code:

(defun superman-control-latex-export ()
  (interactive)
  (superman-export-as-latex 'debug))

;; See library tex-buf for help on TeX-process.
(defun superman-export-as-latex (&optional debug)
  "Export current org document to latex and further to pdf via latexmk.
If no process is running start the pearl script latexmk.

This function works outside R src blocks. Inside R src block
 it calls `superman-ess-eval-and-go'."
  (interactive "P")
  (if (string= (car (org-babel-get-src-block-info)) "R")
      (superman-ess-eval-and-go)
    (let* ((org-buf (current-buffer))
	   (raw-file (superman-file-name nil nil t ""))
	   (tex-file (concat raw-file ".tex"))
	   (org-file (concat raw-file ".org"))
	   (tex-buf (get-file-buffer tex-file))
	   (help-buf (concat "*Superman-export-control: " (file-name-nondirectory org-file) "*"))
	   (wconf (current-window-configuration))
	   (process (TeX-process tex-file))
	   R-buf
	   R-proc)
      (save-buffer)
      ;; we silently assume that the user wants to overwrite
      ;; the current tex file and to avoid that the user has
      ;; to confirm this -- in case of an open tex-buffer -- we
      ;; kill a possibly existing tex-buffer
      (when tex-buf
	(save-excursion
	  (set-buffer tex-buf)
	  (revert-buffer t t t) 
	  (kill-buffer (get-file-buffer tex-file))))
      ;; find R process
      (when debug
	(switch-to-buffer org-buf)
	(setq R-proc (and ess-current-process-name (get-process ess-current-process-name)))
	(if R-proc (setq R-buf (buffer-name (process-buffer R-proc)))
	  (save-excursion
	    (goto-char (point-min))
	    (while (and (not R-buf)
			(re-search-forward org-block-regexp nil t))
	      (beginning-of-line) ;; move inside block
	      (let ((info (org-babel-get-src-block-info)))
		(and (string= (car info) "R")
		     (setq R-buf (cdr (assoc ':session (caddr info))))))))))
      (when (and R-buf (buffer-live-p R-buf))
	(with-current-buffer R-buf
	  (let ((ess-current-process-name R-buf))
	    (ess-switch-to-end-of-ESS)
	    (put-text-property (point-at-bol) (1+ (point-at-bol)) 'eval-point t))))
      ;; export to latex or beamer
      (if org-beamer-mode
	  (org-beamer-export-to-latex)
	(org-latex-export-to-latex))      
      ;; check/start latexmk process
      ;; (message "export")
      (if process
	  (cond (debug (delete-process process)
		       (find-file tex-file)
		       (TeX-command "LaTeX" 'TeX-master-file nil))  ;; run latex to identify problems
		((not (eq (process-status process) 'run));; kill not running process
		 (delete-process process)
		 (TeX-run-TeX "make-pdf" (concat "latexmk -pvc -pdf -f " tex-file) tex-file))
		(t (message (concat "Currently running process: " (process-name process)))))
	(if debug ;; run latex to identify problems
	    (progn
	      (find-file tex-file)
	      (TeX-command "LaTeX" 'TeX-master-file nil))
	  (TeX-run-TeX "make-pdf" (concat "latexmk -pvc -pdf -f " tex-file) tex-file)))
      ;; (message "process")
      (when debug
	(superman-set-config
	 (concat (buffer-name org-buf) " | " tex-file " / 0-8 " help-buf))
	(set-buffer help-buf)
	(select-window (get-buffer-window help-buf nil))
	(font-lock-mode -1)
	(superman-control-export-mode-on)
	(let ((buffer-read-only nil))
	  (erase-buffer)
	  (insert "Control org export (\\M-j): " org-file "\n")
	  (put-text-property (point-min) (1+ (point-min)) 'tex-file tex-file)
	  (put-text-property (point-min) (1+ (point-min)) 'org-buffer org-buf)
	  (put-text-property (point-min) (1+ (point-min)) 'wconf wconf)
	  (put-text-property (point-min) (1+ (point-min)) 'beamer-p org-beamer-mode)
	  (when R-buf
	    (put-text-property (point-min) (1+ (point-min)) 'R-buf R-buf))
	  (insert
	   (superman-make-button
	    "Run LaTeX (l)"
	    'superman-run-latex
	    nil
	    "Run LaTeX.")
	   "  "
	   (superman-make-button
	    "Run BibTeX (b)"
	    'superman-run-bibtex
	    nil
	    "Run BibTeX.")
	   "  "
	   (superman-make-button
	    "start viewer (v)"
	    'superman-start-viewer
	    nil
	    "Start viewer.")
	   "  "
	   (superman-make-button
	    "Start LaTeX make (m)"
	    'superman-start-latexmk
	    nil
	    "Start latexmk script.")
	   "\n")
	  ;; (insert (superman-make-button
	  ;; "Export to LaTeX (e)"
	  ;; 'superman-latex-export
	  ;; nil
	  ;; "Export from org to latex (e)")
	  ;; "  ")
	  (insert (superman-make-button
		   "LaTeX-next-error (n)"
		   'superman-next-latex-error
		   nil
		   "Show next LaTeX error")
		  "  "
		  (superman-make-button
		   "LaTeX-first-error (1)"
		   'superman-first-latex-error
		   nil
		   "Show first LaTeX error")
		  " "
		  ;; (superman-make-button
		  ;; "LaTeX-find-error (f)"
		  ;; 'superman-find-latex-error
		  ;; nil
		  ;; "Find LaTeX error")
		  "\n")
	  (when R-buf
	    (insert
	     (superman-make-button
	      "R-previous-error (r)"
	      'superman-previous-R-error
	      nil
	      (concat "Show previous error in " R-buf))
	     "  "
	     (superman-make-button
	      "R-next-error (s)"
	      'superman-next-R-error
	      nil
	      (concat "Show next error in " R-buf))
	     "\n"))
	  (insert
	   (superman-make-button
	    "Back to org (q)"
	    `(lambda () (interactive)
	       (set-window-configuration (get-text-property (point-at-bol) 'wconf)))
	    nil
	    "Switch to previous window configuration.")))))))

(defun superman-ess-eval-and-go ()
  (interactive)
  (if (region-active-p)
      (let* ((start (region-beginning))
	     (end (region-end))
	     (visibly (< (length (buffer-substring-no-properties start end)) 300)))
	(ess-eval-region-and-go start end (not visibly)))
    (save-excursion
      (ess-eval-line-and-step))))


(defun superman-control-export-back-to-org ()
  (interactive)
  (set-window-configuration
   (get-text-property (point-min) 'wconf)))


(defun superman-file-name (&optional file buf full ext)
  "Function to turn name.xxx into name.org. When FILE is given
it must be regular file-name, it may be the absolute filename including
the directory. If FILE is nil and BUF is given, then use the filename of
the file visited by buffer BUF. If BUF is also nil then use 
'current-buffer'. If FULL is non-nil return the absolute filename.
If EXT is given then turn name.xxx into name.ext. EXT must be a string like '.tex'" 
  (let ((name (or file (buffer-file-name (or buf (current-buffer))))))
    (concat (file-name-sans-extension
	     (if full
		 name
	       (file-name-nondirectory name)))
	    (or ext ".org"))))
  

(defvar superman-control-export-mode-map (make-sparse-keymap)
  "Keymap used for `superman-view-mode' commands.")
(define-key superman-control-export-mode-map  "q" 'superman-control-export-back-to-org)
(define-key superman-control-export-mode-map  "n" 'superman-next-latex-error)
(define-key superman-control-export-mode-map  "1" 'superman-first-latex-error)
(define-key superman-control-export-mode-map  "r" 'superman-previous-R-error)
(define-key superman-control-export-mode-map  "s" 'superman-next-R-error)
(define-key superman-control-export-mode-map  "l" 'superman-run-latex)
(define-key superman-control-export-mode-map  "b" 'superman-run-bibtex)
(define-key superman-control-export-mode-map  "v" 'superman-start-viewer)
(define-key superman-control-export-mode-map  "m" 'superman-start-latexmk)
(define-key superman-control-export-mode-map  "e" 'superman-latex-export)
(define-key superman-control-export-mode-map  "\M-j" 'superman-latex-export)
;; (define-key superman-control-export-mode-map  "f" 'superman-find-latex-error)

(defun superman-find-latex-error ()
  (interactive)
  (let ((tex-file (get-text-property (point-min) 'tex-file))
	(control-buf (buffer-name (current-buffer)))
	(last-pos (get-text-property (point-min) 'latex-pos))
	tex-buf
	pos)
    (if (get-file-buffer tex-file)
	(set-buffer (get-file-buffer tex-file))
      (find-file tex-file))
    (setq tex-buf (current-buffer))
    (goto-char (or last-pos (point-min)))
    (re-search-forward "\\\\\\(sub\\)*section{" nil t)
    (previous-line 1)
    (end-of-line)
    (insert "\n\\end{document}")
    (save-buffer)
    (save-excursion
      (TeX-command "LaTeX" 'TeX-master-file nil))
    (beginning-of-line)
    (kill-line)
    (save-buffer)
    (forward-line 1)
    (setq pos (point))
    (superman-switch-config
     nil nil
     (concat tex-file " | *TeX Help* / " control-buf))
    (with-current-buffer control-buf
      (let ((buffer-read-only nil))
      (put-text-property (point-min) (1+ (point-min)) 'latex-pos pos)))))
    
(defun superman-latex-export ()
  (interactive)
  (let ((obuf (get-text-property (point-min) 'org-buffer)))
    (with-current-buffer obuf
      (superman-export-as-latex))))

(defun superman-run-latex ()
  (interactive)
  (let ((tex-file (get-text-property (point-min) 'tex-file)))
    (save-window-excursion
      (if (get-file-buffer tex-file)
	  (set-buffer (get-file-buffer tex-file))
	(find-file tex-file))
      ;; (set-buffer (find-buffer-visiting tex-file))
      (TeX-command "LaTeX" 'TeX-master-file nil))))

(defun superman-start-viewer ()
  (interactive)
  (let ((tex-file (get-text-property (point-min) 'tex-file)))
    (save-excursion
      (if (get-file-buffer tex-file)
	  (set-buffer (get-file-buffer tex-file))
	(find-file tex-file))
      (TeX-command "View" 'TeX-master-file nil))))

(defun superman-run-bibtex ()
  (interactive)
  (let ((tex-file (get-text-property (point-min) 'tex-file)))
    (save-excursion
      (if (get-file-buffer tex-file)
	  (set-buffer (get-file-buffer tex-file))
	(find-file tex-file))
      (TeX-command "BibTeX" 'TeX-master-file nil))))

(defun superman-start-latexmk ()
  (interactive)
  (let ((tex-file (get-text-property (point-min) 'tex-file)))
    (TeX-run-TeX "make-pdf" (concat "latexmk -pvc -pdf -f " tex-file) tex-file)))

(defun superman-first-latex-error ()
  (interactive)
  (superman-next-latex-error 'first))

(defun superman-next-latex-error (&optional first)
  (interactive)
  (let ((control-buf (buffer-name (current-buffer)))
	(tex-file (get-text-property (point-min) 'tex-file))
	(org-buf (buffer-name (get-text-property (point-min) 'org-buffer))))
    ;; (help-tick
    ;; (when (buffer-live-p (get-buffer "*TeX Help*"))
    ;; (with-current-buffer
    ;; "*TeX Help*"
    ;; (buffer-modified-tick)))))
    (when (buffer-live-p (get-buffer "*TeX Help*"))
      (with-current-buffer  "*TeX Help*"
	(erase-buffer)
	(insert "No more errors.")))
    (TeX-next-error first)
    (superman-set-config
     (concat org-buf " / " tex-file " | *TeX Help* / " control-buf))
    (other-window 2)))
  
(defun superman-previous-R-error ()
  (interactive)
  "See `superman-next-R-error'."
  (superman-next-R-error 'back))

(defun superman-next-R-error (&optional backwards)
  "Search for Error terms in current R output console. If BACKWARDS
is non-nil, search backwards within the boundery set by last call to
`superman-latex-export'."
  (interactive)
  (let ((control-buf (buffer-name (current-buffer)))
	(R-buf (get-text-property (point-min) 'R-buf)))
    (when R-buf
      (superman-switch-config
       nil nil
       (concat R-buf " / " control-buf))
      (unless
	  (if backwards
	      ;; search for errors within the bound set by the last
	      ;; call to superman-export-as-latex 
	      (re-search-backward "^Error[: ]+" (previous-single-property-change (point) 'eval-point) t)
	    (re-search-forward "^Error[: ]+ " nil t))
	(message "No next error"))
      (other-window 1))))

(define-minor-mode superman-control-export-mode
  "Toggle superman view item mode.
With argument ARG turn superman-control-export-mode on if ARG is positive, otherwise
turn it off."
  :lighter "*S:export*"
  :group 'org
  :keymap 'superman-control-export-mode-map
  (setq buffer-read-only t))

(defun superman-control-export-mode-on ()
  (interactive)
  (superman-control-export-mode t))

(defun org-turn-on-auto-export ()
  (interactive)
  (add-hook 'after-save-hook 'superman-export-as-latex))

(defun org-turn-off-auto-export ()
  (interactive)
  (remove-hook 'after-save-hook 'superman-export-as-latex))


(defun superman-save-and-run (&optional arg)
  (interactive)
  (save-buffer)
  (let ((cmd (completing-read "Command (default make-pdf): " 
			      '("make-pdf" "make-html")
			      nil 'must-match nil nil "make-pdf" nil))
	(dir (when arg (read-file-name "Publishing directory: " default-directory))))
    (cond ((string= cmd "make-pdf")
	   ;;	   (call-interactively 'org-export-as-latex)
	   (if org-beamer-mode
	       (org-beamer-export-as-latex 3 nil nil nil nil nil)
	     (org-latex-export-as-latex 3 nil nil nil nil nil)))
	  ((string= cmd "make-html")
	   (org-html-export-as-html-and-open 3)))))

(defun superman-export-as-docx ()
  "Save current buffer, then export to docx via soffice."
  (interactive)
  (save-buffer)
  (let* ((org-odt-preferred-output-format "docx")
	 (old-proc (get-buffer-process (current-buffer)))
	 (name (file-name-sans-extension (buffer-name)))
	 (proc-name (concat "superman-opens:" name ".docx"))
	 (proc-buf (get-buffer-create (concat "superman-oo-run:" name)))
	 file)
    "/usr/bin/soffice /home/tag/tmp/u.docx"
    ;; (org-open-file file 'system)))
    (when (get-process proc-name)
      (kill-process proc-name))
    (setq file (org-odt-export-to-odt))
    (start-process-shell-command
     proc-name
     proc-buf
     (concat "/usr/bin/soffice -norestore " file))))

(provide 'superman-export)
;;; superman-export.el ends here
