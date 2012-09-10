
(require 'ido)
(require 'org)  
(require 'deft)

(defvar org-project-manager-default-directory (file-name-as-directory org-directory) "A place for new projects.")
(defvar org-project-manager (concat
                             (file-name-as-directory org-directory)
                                       "Projects.org")
            "Where the org-project-manager defines the projects. The contents of
        the file has the following structure:
        
    ,* Fun projects
     :PROPERTIES:
     :LOCATION: ~/fun
     :END:  
        
    ,**** Tex Avery Collection 
    :PROPERTIES:
    :index: bla.org
    :nickname: texAvery
    :others: Foghorn Leghorn
    :END:
  
    ,**** Cultural stuff like films, tv-series, music etc.
    :PROPERTIES:
    :nickname: culture
    :END:
  
    ,* Work projects
     :PROPERTIES:
     :LOCATION: ~/work
     :END:    
  
  ,**** ACTIVE BinomialRegression
  :PROPERTIES:
  :NICKNAME: BinomialRegression
  :END:
    ")

(defvar org-project-manager-project-level 4
"Subheading level at which projects are defined
in `org-project-manager'.")

(defvar org-project-manager-minor-mode nil)
(make-variable-buffer-local 'org-project-manager-minor-mode)
(defvar org-project-manager-minor-mode-map (make-sparse-keymap)
  "Keymap used for `org-project-manager-minor-mode' commands.")
(or (assq 'org-project-manager-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (append minor-mode-map-alist
                  (list (cons 'org-project-manager-minor-mode org-project-manager-minor-mode-map)))))
(or (assq 'org-project-manager-minor-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(org-project-manager-minor-mode " Project") minor-mode-alist)))
(defun org-project-manager-minor-mode (&optional arg)
  "A minor mode for using org Project Manager."
  (interactive "P")
  ;; (make-variable-buffer-local 'hippie-expand-try-functions-list)
  (setq org-project-manager-minor-mode
        (not (or (and (null arg) org-project-manager-minor-mode)
                 (<= (prefix-numeric-value arg) 0))))
  (add-hook 'after-save-hook 'org-project-manager-refresh nil 'local))
(define-key org-project-manager-minor-mode-map [(meta return)] 'org-project-manager-return)
(define-key org-project-manager-minor-mode-map [(meta n)] 'org-project-manager-next-project)
(define-key org-project-manager-minor-mode-map [(meta p)] 'org-project-manager-previous-project)
(add-hook 'find-file-hooks 
          (lambda ()
            (let ((file (buffer-file-name)))
              (when (and file (equal file (expand-file-name org-project-manager)))
                (org-project-manager-minor-mode)))))

(defvar org-project-manager-project-alist nil
           "Alist of projects associating the nickname of the project
     with information like the location of the project, the index file, collaborators, category, publishing-directory, etc.")
    
    (defvar org-project-manager-current-project nil "The currently selected project.")
           
               
  (defun org-project-manager-parse-projects (&optional all)
       "Parse file 'project-manager' and update 'org-project-manager-project-alist'"
       (interactive)
       (save-excursion
         (setq org-project-manager-project-alist nil)
         (set-buffer (find-file-noselect org-project-manager))
         (save-buffer)
         (goto-char (point-min))
         (while (org-project-manager-forward-project)
             (let* ((loc (or (org-entry-get nil "LOCATION" 'inherit) org-project-manager-default-directory))
                    (category (org-entry-get nil "CATEGORY" 'inherit))
                    (others (org-entry-get nil "OTHERS" nil))
                    (publish-dir (org-entry-get nil "PUBLISH" 'inherit))
                    (name (or (org-entry-get nil "NICKNAME" nil)
                              (nth 4 (org-heading-components))))
                    (git (org-entry-get nil "GIT" 'inherit))
                    (index (or (org-entry-get nil "INDEX" nil)
                               (let ((default-org-home
                                       (concat (file-name-as-directory loc)
                                               name
                                               org-project-manager-org-location)))
                                 ;; (make-directory default-org-home t)
                                 (concat (file-name-as-directory default-org-home) name ".org")))))
               (unless (file-name-absolute-p index)
                 (setq index
                       (expand-file-name (concat (file-name-as-directory loc) name "/" index))))
               (add-to-list 'org-project-manager-project-alist
                            (list name
                                  (list (cons "location"  loc)
                                        (cons "index" index)
                                        (cons "category" category)
                                        (cons "others" others)
                                        (cons "git" git)
                                        (cons "publish-directory" publish-dir))))))
           org-project-manager-project-alist))
     
  (defvar org-project-manager-project-categories nil
"List of categories for sorting projects.")

(defun org-project-manager-get-buffer-props (property)
    "Get a table of all values of PROPERTY used in the buffer, for completion."
    (let (props)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (concat ":" property ":") nil t)
          (add-to-list 'props (list
                               (org-entry-get
                                nil property nil)))))
      props))
  
(defun org-project-manager-parse-categories ()
    (interactive)
      (set-buffer (find-file-noselect org-project-manager))
      (setq org-project-manager-project-categories
            (reverse (org-project-manager-get-buffer-props "CATEGORY"))))

(defun org-project-manager-refresh ()
  (interactive)
  (org-project-manager-parse-categories)
  (org-project-manager-parse-projects))

(defun org-project-manager-index-list (&optional category extension not-exist-ok update)
 "Return a list of project specific indexes.
Projects are filtered by CATEGORY unless CATEGORY is nil.
Only existing files are returned unless NOT-EXIST-OK is non-nil.
Only files ending on EXTENSION are returned unless EXTENSION is nil.
If UPDATE is non-nil first parse the file org-project-manager."
 (interactive "P")
 (if update
 (org-project-manager-refresh))
 (delq nil (mapcar '(lambda (x)
 (let ((f (org-project-manager-get-index x)))
       (when (and (or not-exist-ok (file-exists-p f))
                (or (not extension)
                    (string= extension (file-name-extension f))))
                     f)))
  (if category
      (delq nil (mapcar '(lambda (p) (if (string= category (org-project-manager-get-category p))
                           p))
                        org-project-manager-project-alist))
  org-project-manager-project-alist))))

(defvar org-project-manager-org-location "/"
   "Relative to the project location this defines
 the path to the index file of a project. If set to
 '/org/' then the index file will be placed
 in a subdirectory 'org' of the project directory.")

(defvar org-project-manager-default-category "Unsorted" "Category for new projects.")
;; (setq org-refile-targets (quote ((org-project-manager :maxlevel . 3) (nil :maxlevel . 2))))

(defun org-project-manager-set-nickname ()
  (interactive)
  (org-set-property
   "NICKNAME"
   (read-string "NickName for project: "
                (nth 4 (org-heading-components)))))

(defun org-project-manager-set-others ()
  (interactive)
  (let* ((pro (assoc (org-project-manager-project-at-point t)
    org-project-manager-project-alist))
         (others (cdr (assoc "others" (cadr pro))))
         (init (if others (concat others ", ") "")))
       ;; (org-entry-get nil "others")
(if pro
     (org-set-property
     "others"
   (replace-in-string
    (read-string (concat "Set collaborators for " (car pro) ": ") init)
    "[,\t ]+$" "")))))


(defun org-project-manager-fix-others ()
(interactive)
(goto-char (point-min))
(while (org-project-manager-forward-project)
  (org-project-manager-set-others)))

(add-to-list 'org-structure-template-alist
 '("P" "**** ACTIVE %?:PROPERTIES:\n:NICKNAME:\n:OTHERS:\n:CaptureDate:\n:END:"))

(defvar org-project-manager-default-content "" "Initial contents of org project index file.")
                     (defvar org-project-manager-project-subdirectories nil)
      
  
(defun org-project-manager-create-project (&optional project ask)
  "Create the index file, the project directory, and subdirectories if
    'org-project-manager-project-subdirectories' is set."
  (interactive)
  (let ((pro (assoc project org-project-manager-project-alist)))
    (when pro
      (let ((dir (org-project-manager-get-location pro))
            (index (org-project-manager-get-index pro)))
        (unless (or (not dir) (file-exists-p dir) (and ask (not (y-or-n-p (concat "Create directory " dir "? ")))))
          (make-directory dir)
          (loop for subdir in org-project-manager-project-subdirectories
                do (unless (file-exists-p subdir) (make-directory (concat path subdir) t))))
        (when (and index (not (file-exists-p index)))
          (unless (file-exists-p (file-name-directory index))
            (make-directory (file-name-directory) t))
          (find-file index))))))
          ;; (append-to-file org-project-manager-default-content nil index)
;; )))
      
      (defun org-project-manager-show-properties ()
        (let ((pop-up-windows t)
              (obuf (current-buffer))
              (pbuf (get-buffer "*Org project manager properties*")))
          (set-buffer pbuf)
          (erase-buffer)
          (insert "Current project categories:\n\n")
          (mapcar '(lambda (x) (if (car x) (insert (car x) ", "))) org-project-manager-project-categories)
          (delete-backward-char 2)
          (insert "\n\n")
          (pop-to-buffer pbuf)
          (pop-to-buffer obuf)))
    
    (defun org-project-manager-new-project (&optional nickname category)
              "Create a new project. Prompt for CATEGORY and NICKNAME if necessary.
              This function modifies the 'org-project-manager' and creates and visits the index file of the new project.
              Thus, to undo all this you may want to call 'org-project-manager-delete-project'. 
              " 
              (interactive)
              (org-project-manager-refresh)
              (let ((nickname (or nickname (read-string "Project name (short) "))))
                ;; check if nickname exists 
                (while (assoc nickname org-project-manager-project-alist)
                  (setq nickname
                        (read-string (concat "Project " nickname " exists. Please choose a different name (C-g to exit): "))))
                ;; a local capture command places the new project
                (let ((org-capture-templates
                       `(("p" "Project" plain
                        (file+headline org-project-manager "New projects")
                        ,(concat (make-string org-project-manager-project-level (string-to-char "*"))
                                 " ACTIVE %c%?\n:PROPERTIES:\n:NICKNAME: "
                                 nickname
                                 "\n:LOCATION:\n:CATEGORY:\n:INDEX:\n:GIT:\n:OTHERS:\n:END:\n"))))
                      (org-capture-bookmark nil))
                  (kill-new nickname)
                  (add-hook 'org-capture-after-finalize-hook `(lambda () (org-project-manager-create-project ,nickname 'ask)) nil 'local)
                  ;;(add-hook 'org-capture-mode-hook 'org-project-manager-show-properties nil 'local)
                  (org-capture nil "p")
                  (pop kill-ring)
                  )))
                                      
    (defun org-project-manager-delete-project (&optional project)
                    (interactive)
                    (let* ((pro (or project org-project-manager-select-project))
                           (dir (org-project-manager-get-location pro))
                           (git (org-project-manager-get-git pro))
                           (index (org-project-manager-get-location pro)))
                      (pop-to-buffer "*Org-project-files*")
                      (erase-buffer)
                      (insert index "\n" dir "\n" git "\n")
                      (when (yes-or-no-p (concat "Really remove project " pro "?")))))

(defun org-project-manager-goto-project-manager ()
    (interactive)
    (find-file org-project-manager))
  
  (defun org-project-manager-project-at-point (&optional noerror)
    "Check if point is at project heading and return the project,
      i.e. its entry from the 'org-project-manager-project-alist'.
      Otherwise return error or nil if NOERROR is non-nil. "
    (interactive)
      ;; (org-back-to-heading)
    (if (or (org-before-first-heading-p)
            (not (org-at-heading-p))
            (not (= org-project-manager-project-level
                    (- (match-end 0) (match-beginning 0) 1))))
        (if noerror nil
          (error "No project at point"))
      (or (org-entry-get nil "NICKNAME")
          (progn (org-project-manager-set-nickname)
                 (save-buffer) ;; to update the project-alist
                 (org-entry-get nil "NICKNAME")))))
  
  
  (defun org-project-manager-return ()
    (interactive)
    (let* ((pro (assoc (org-project-manager-project-at-point)
                       org-project-manager-project-alist)))
      (delete-other-windows)
            (split-window-horizontally 25)
            (other-window 1)
            (find-file (org-project-manager-get-index pro))
            (split-window-vertically 13)
            (switch-to-buffer "*Current project*")
            (erase-buffer)
            (insert (car pro) "\n------------------------------\n")
            (mapc (lambda (x) (insert (car x) ": " (if (cdr x) (cdr x) "")  "\n")) (cadr pro))
            (other-window -1)))
        
(defun org-project-manager-forward-project ()
      (interactive)
        (re-search-forward
         (format "^\\*\\{%d\\} " org-project-manager-project-level) nil t))
        
        (defun org-project-manager-backward-project ()
        (interactive)
        (re-search-backward
         (format "^\\*\\{%d\\} " org-project-manager-project-level) nil t))
        
        (defun org-project-manager-next-project (arg)
        (interactive  "p")
        (org-project-manager-forward-project)
        (org-project-manager-return))
        
        (defun org-project-manager-previous-project (arg)
        (interactive  "p")
        (org-project-manager-backward-project)
        (org-project-manager-return))

(defun org-project-manager-git-p (dir)
         "Test if directory DIR is under git control."
        (eq 0 (shell-command (concat "cd " dir ";git rev-parse --is-inside-work-tree "))))
      
      (defun org-project-manager-git-init-directory (dir)
      "Put directory DIR under git control."
       (if (org-project-manager-git-p dir)
        (message (concat "Directory " dir " is under git control."))
       (shell-command (concat "cd " dir "; git init"))
       (append-to-file org-project-manager-git-ignore nil (concat dir ".gitignore"))))
      
 (defun org-project-manager-git-update-directory (dir silent)
      "Put directory DIR under git control."
    (let* ((necessary (not (string-match "nothing to commit" (shell-command-to-string  (concat "cd " dir "; git status")))))
             (doit (when necessary (or silent (y-or-n-p (concat "Update git at " dir "? ")))))
             (message (when doit (if silent "silent update" (read-string "Git commit message: ")))))
        (if doit
            (shell-command (concat "cd " dir "; git add -u;git commit -m \"" message "\"")))))
    
    
(defun org-project-manager-git-push-directory (dir silent)
      "Put directory DIR under git control."
      (let* ((status (shell-command-to-string  (concat "cd " dir "; git status")))
             (necessary (string-match "Your branch is ahead .*\n" status))
             (doit (or silent (y-or-n-p (concat "Your branch is ahead ... push git at " dir "? ")))))
        (if doit
            (async-shell-command (concat "cd " dir "; git push")))))
      

(defun org-project-manager-git-update-project (project before)
    "Check if project needs to be put under git control and update.
  If BEFORE is set then either initialize or pull. Otherwise, add, commit and/or push.
  "
    (let* ((git-control (downcase (org-project-manager-get-git project))))
      (unless (or (string= git-control "") (string-match "no\\|never\\|nil" git-control))
        (let ((silent-p (string= git-control "silent"))
              (dir (org-project-manager-get-git-location project)))
          (when (file-exists-p dir)
            (if before
                ;; activating project
                (unless (or (org-project-manager-git-p dir) (string-match "no" git-control) (string= "" git-control))
              (when (or silent-p
                        (y-or-n-p (concat "Initialize git control at " dir "?")))
                (org-project-manager-git-init-directory dir))
              (when (and (string-match "pull" git-control)
                         (or silent-p (y-or-n-p (concat "Run this command: \"git pull\" at " dir "? "))))
                (shell-command (concat "cd " dir "; git pull \""))))
          ;; deactivating project
          (when (and (org-project-manager-git-p dir)
                     (string-match "yes\\|silent" git-control))
            (org-project-manager-git-update-directory dir silent-p)
            (when (string-match "push" git-control)
              (org-project-manager-git-push-directory dir silent-p)
            ))))))))
  
  (defvar org-project-manager-git-ignore "
  export
  ,*~
  ,*.ind
  ,*.brf
  ,*.idx
  ,*.ilg
  ,*.lof
  ,*.html
  ,*pdfsync
  ,*.pdf
  ,*.png
  ,*.ind
  ,*.o
  ,*.so
  ,*.bbl
  ,*.blg
  ,*.bak
  ,*.snm
  ,*.aux
  ,*.log
  ,*.xref
  ,*.idv
  ,*.4ct
  ,*.out
  ,*.swp
  ,*.nav
  ,*.toc
  ,*.vrb
  ,*.dvi
  r_env_cache
  .Rhistory
  .RData
  Rplots.p*
  _region*
  ")

;; Hack to quickly start new projects via deft 
(defun deft-new-file ()
  "Create a new project quickly."
  (interactive)
  (org-project-manager-new-project (deft-whole-filter-regexp)))
(defun deft-find-all-files ()
  (org-project-manager-index-list))

(defun org-project-manager-project-agenda ()
    "Show an agenda of all the projects. Useful, e.g. for toggling
the active status of projects."
    (interactive)
    (find-file org-project-manager)
    (push ?t unread-command-events)
    (push ?< unread-command-events)
    (call-interactively 'org-agenda))
;;     (defun org-project-manager-agenda ()
;;      (interactive)
;;      (let ((org-agenda-files
;;             (delq nil (mapcar '(lambda (x) (let ((f (org-project-manager-get-index x))) (if (file-exists-p f) f))) 
;;                               (org-project-manager-parse-projects))))
;;            (org-agenda-include-diary nil))
;;            (org-agenda-list)))

(defun org-project-manager-format-project (entry)
      (let ((cat (org-project-manager-get entry "category"))
            (coll (org-project-manager-get entry "others"))
            (nickname (car entry)))
        (cons
         ;; (format format cat (if coll coll "") nickname)
         (concat cat "/" (if coll (concat coll "/")) (car entry))
         (car entry))))
    
(defun org-project-manager-select-project ()
      "Select a project from the project alist, 
  which is modified such that 'org-project-manager-current-project'
  is the first choice."
      (let* ((plist org-project-manager-project-alist)
             (project-array (mapcar 'org-project-manager-format-project
                                    (if (not org-project-manager-current-project)
                                        plist
                                      (setq plist (append (list org-project-manager-current-project)
                                              (remove org-project-manager-current-project plist))))))
             (completion-ignore-case t)
             (key (ido-completing-read "Project: " (mapcar 'car project-array)))
             (nickname (cdr (assoc key project-array))))
        (assoc nickname org-project-manager-project-alist)))

(defun org-project-manager-activate-project (project)
 "Sets the current project.
Start git, if the project is under git control, and git is not up and running yet."
  (setq org-project-manager-current-project project)
  ;; maybe activate git control
  (org-project-manager-git-update-project project 'before))

(defvar org-project-manager-save-buffers 'save-some-buffers
  "Function to be called to save buffers before switching project.")
(defun org-project-manager-save-project (&optional project)
  (interactive)
  (when (and (object-p org-project-manager-save-buffers)
             (functionp org-project-manager-save-buffers))
    (funcall org-project-manager-save-buffers))
  (let* ((pro (or project
                  org-project-manager-current-project)))
    (org-project-manager-git-update-project pro nil)))

(defvar org-project-manager-switch-always t "If nil 'org-project-manager-switch-to-project' will
            switch to current project unless the last command also was 'org-project-manager-switch-to-project'.
            Setting this variable to non-nil (the default) will force 'org-project-manager-switch-to-project'
            to always prompt for new project")
  
(defun org-project-manager-switch-to-project (&optional force)
      "Select project via 'org-project-manager-select-project', activate it
    via 'org-project-manager-activate-project',  find the associated index file."
                (interactive "P")
                (let ((change (or force
                                    org-project-manager-switch-always
                                   (and (eq last-command 'org-project-manager-switch-to-project))
                                  (not org-project-manager-current-project)))
                      (curpro org-project-manager-current-project))
                  (if (not change)
                      (let ((index (org-project-manager-get-index org-project-manager-current-project)))
                        (find-file index)
                      (message "Press the same key again to switch project"))
                  (let ((pro (org-project-manager-select-project)))
                    (unless (eq pro curpro)
                      (org-project-manager-save-project curpro)
                      (org-project-manager-activate-project pro))
                    (find-file (org-project-manager-get-index
                                org-project-manager-current-project))))))
              
  (defun org-project-manager-get (project el)
   (cdr (assoc el (cadr project))))
              
  (defun org-project-manager-get-index (project)
    (cdr (assoc "index" (cadr project))))
  
  (defun org-project-manager-get-git (project)
    (or (cdr (assoc "git" (cadr project))) ""))
  
  (defun org-project-manager-get-git-location (project)
    (or (cdr (assoc "git-location" (cadr project)))
        (org-project-manager-get-location project)))

(defun org-project-manager-get-location (project)
  (let ((loc (cdr (assoc "location" (cadr project)))))
    (if loc 
        (concat (file-name-as-directory
                 loc)
                (car project)))))
  
  (defun org-project-manager-get-publish-directory (project)
    (cdr (assoc "publish-directory" (cadr project))))
  
  (defun org-project-manager-get-category (project)
    (cdr (assoc "category" (cadr project))))

(defun org-project-manager-goto-project (&optional project heading create)
  (interactive)
  (let ((pro 
         (or project
            (car (org-project-manager-select-project)))))
    (when (and (not (string-equal pro "")) pro)
      (let* ((entry (assoc pro org-project-manager-project-alist))
        (index (org-project-manager-get-index entry))
        (head (or heading "WorkFlow")))
      (if index
          (find-file index)
        (error (concat "Project " pro " does not have an index.")))
      (goto-char (point-min))
      (or (re-search-forward (concat "^[*]+ " heading) nil t)
          (when create
            (insert "* " heading "\n\n")
            (forward-line  -1)))))))


(defun org-project-manager-goto-project-workflow ()
  (interactive)
  (or (org-project-manager-goto-project nil "WorkFlow" 'create)))

;; (org-project-manager-goto-project nil "WorkFlow" t)


(defun org-project-manager-goto-project-taskpool (&optional arg)
  (interactive)
  (if arg (org-store-link nil))
  (let* ((buf (current-buffer))
         (pro (completing-read "Select project: " org-project-manager-project-alist))
         (entry (assoc pro org-project-manager-project-alist))
         (index (org-project-manager-get-index entry)))
    (if index
        (find-file index)
      (error (concat "Project " pro " does not have an index.")))
    (goto-char (point-min))
    (or (re-search-forward "^[*]+ TaskPool" nil t)
        (progn
          (goto-char (point-max))
          (insert "\n\n* TaskPool\n")
          (point)))))

(defvar org-project-manager-export-subdirectory "export")
(defvar org-project-manager-public-directory "~/public_html/")
;; (defvar org-project-manager-publish-subdirectory "public")
(require 'org-publish)
(defun org-project-manager-set-publish-alist ()
  (interactive)
  (let ((p-alist org-project-manager-project-alist))
    (while p-alist
      (let* ((entry  (car p-alist))
             (nickname (car entry))
             (base-directory (file-name-as-directory (org-project-manager-get-location entry)))
             (export-directory
              (concat base-directory
                      org-project-manager-export-subdirectory))
             (public-directory
              (or (org-project-manager-get-publish-directory entry)
                  (concat (file-name-as-directory org-project-manager-public-directory)
                          nickname))))
        ;;(replace-regexp-in-string org-project-manager-public-directory (getenv "HOME") (expand-file-name export-directory))))
        (add-to-list 'org-publish-project-alist
                     `(,(concat nickname "-export")
                       :base-directory
                       ,base-directory
                       :base-extension "org"
                       :publishing-directory
                       ,base-directory
                       :headline-levels 4
                       :auto-preamble t
                       :recursive t
                       :publishing-function
                       org-publish-org-to-html))
        (add-to-list 'org-publish-project-alist
                     `(,(concat nickname "-copy")
                       :base-directory
                       ,export-directory
                       :base-extension
                       "html\\|png\\|jpg\\|org\\|pdf"
                       :publishing-directory
                       ,public-directory
                       :recursive t
                       :publishing-function
                       org-publish-attachment))
        (add-to-list 'org-publish-project-alist
                     `(,nickname
                       :components (,(concat nickname "-export") ,(concat nickname "-copy")))))
      (setq p-alist (cdr p-alist)))))

(provide 'org-project-manager)
