
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
    :collab: Foghorn Leghorn
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
                 (<= (prefix-numeric-value arg) 0)))))
(define-key org-project-manager-minor-mode-map [(meta return)] 'org-project-manager-return)
(add-hook 'find-file-hooks 
          (lambda ()
            (let ((file (buffer-file-name)))
              (when (and file (equal file (expand-file-name org-project-manager)))
                (org-project-manager-minor-mode)))))

(defvar org-project-manager-project-alist nil
        "Alist of projects associating the nickname of the project
  with information like the location of the project, the org-file, collaborators,
  index-file, category, publishing-directory, etc.")
  
    (defun org-project-manager-project-list ()
      "List the org-files assocated with the projects. "
      (interactive)
      (delq nil (mapcar '(lambda (x) (let ((f (org-project-manager-get-org x))) (if (file-exists-p f) f))) 
                        (org-project-manager-parse-projects))))
    
    (defun org-project-manager-list-projects (&optional class flag)
      (interactive)
      ;; (org-project-manager-parse-projects)
      (let* ((cl (or class (completing-read "Project class: " (org-project-manager-parse-categories))))
             (projects org-project-manager-project-alist))
        (delq nil (mapcar '(lambda (x)
                             (let ((org-file (org-project-manager-get-org x))) 
                             (if class
                                 (when (and (string= (org-project-manager-get-category x) class)
                                            org-file
                                            (file-exists-p org-file))
                                   org-file))))
                          projects))))
  

(defvar org-project-manager-include-keywords nil
    "Sub-list of the `org-todo-keywords' as defined locally
in the file `org-project-manager'.

If nil (the default) then all projects at the level
`org-project-manager-project-level' are added.

Otherwise, it should be a list of todo-keywords as they are
defined either globally or locally in the file
 `org-project-manager'
If matched with the todo-keyword of a project, i.e. a
level `org-project-manager-project-level' section heading
in `org-project-manager' is added to the
`org-project-manager-project-alist' during parsing.

For example, if set to 
(list \"ACTIVE\").
only ACTIVE projects are included.")

  (defun org-project-manager-project-include-p ()
    "For parsing of projects:
  
  Decides about whether or not the project is
  in included into the active project list.
  
  Compares the todo-keyword of the project description
  in `org-project-manager' with the value of
  `org-project-manager-include-keywords'. If matched
  the project is added to the `org-project-manager-project-alist'."
    (save-excursion
      (org-back-to-heading t)
      (and (looking-at org-todo-line-regexp)
           (match-end 2)
           (or (not org-project-manager-include-keywords)
           (member (org-match-string-no-properties 2)
                   org-project-manager-include-keywords)))))
  
    (defvar org-project-manager-include-keywords    '("ACTIVE")
      "Sub-list of the `org-todo-keywords' as defined locally
  in the file `org-project-manager'.
  
  If matched with the todo-keyword of a project, i.e. a
  level `org-project-manager-project-level' section heading
  in `org-project-manager' is added to the
  `org-project-manager-project-alist' during parsing. Defaults to
  \"ACTIVE\"."
  )
  
(defun org-project-manager-parse-projects (&optional all)
    "Parse file 'project-manager' and update 'org-project-manager-project-alist'"
    (interactive)
    (save-excursion
      (setq org-project-manager-project-alist nil)
      (set-buffer (find-file-noselect org-project-manager))
      (save-buffer)
      (goto-char (point-min))
      (while (re-search-forward (format "^\\*\\{%d\\} " org-project-manager-project-level) nil t)
        (when (or all (org-project-manager-project-include-p))
          (let* ((loc (org-entry-get nil "LOCATION" 'inherit))
                 (category (org-entry-get nil "CATEGORY" 'inherit))
               (collab (org-entry-get nil "COLLAB" nil))
               (publish-dir (org-entry-get nil "PUBLISH" 'inherit))
               (name (or (org-entry-get nil "NICKNAME" nil)
                         (nth 4 (org-heading-components))))
               (index (or (org-entry-get nil "INDEX" nil)
                          (let ((default-org-home
                                  (concat (file-name-as-directory loc)
                                          name
                                          org-project-manager-org-location)))
                            (make-directory default-org-home t)
                            (concat (file-name-as-directory default-org-home) name ".org"))))
               (org (or (org-entry-get nil "PROJECTORG")
                        (when (string= (file-name-extension index) "org")
                          index))))
          (unless (file-name-absolute-p index)
            (setq index
                  (expand-file-name (concat (file-name-as-directory loc) name "/" index))))
          ;; (if loc (concat (file-name-as-directory loc) name "/" index)))))
          (add-to-list 'org-project-manager-project-alist
                       (list name
                             (list (cons "location"  loc)
                             (cons "org-file" org)
                             (cons "index-file" index)
                             (cons "category" category)
                             (cons "collab" collab)
                             (cons "publish-directory" publish-dir)))))))
      org-project-manager-project-alist))

(defvar org-project-manager-current-project nil "The currently selected project.")

(defvar org-project-manager-org-location "/"
      "Relative to the project location this defines
the path to the index file of a project. If set to
'/org/' then the index file will be placed
in a subdirectory 'org' of the project directory.")

(setq org-project-manager-default-category "Miscellaneous")
(setq org-refile-targets (quote ((org-project-manager :maxlevel . 3) (nil :maxlevel . 2))))

(defun org-project-manager-set-nickname ()
  (interactive)
  (org-set-property
   "NICKNAME"
   (read-string "NickName for project: "
                (nth 4 (org-heading-components)))))

(add-to-list 'org-structure-template-alist
             '("p" "**** ACTIVE \n:PROPERTIES:\n:NICKNAME: \n:COLLAB: \n:CaptureDate: \n:END:"))

(defun org-project-manager-new-project ()
  (interactive)
  (org-project-manager-parse-projects)
  (org-project-manager-parse-categories)
  (let* ((title (read-string "Project title (long name): "))
         (nickname (read-string
                    (concat "Nickname (default " title "): ") nil 'minibuffer-history title))
         (cat (completing-read
               "Choose a category: "
               org-project-manager-project-categories nil nil nil "New projects")))
    (find-file org-project-manager)
    (goto-char (point-min))
    (if (or (re-search-forward (concat ":" cat ":") nil t)
            (re-search-forward (concat "^**[ \t]+" cat) nil t)
            (re-search-forward "^**[ \t]+ New projects" nil t))
        (let
            ((org-insert-heading-respect-content t))
          (org-insert-subheading))
      (insert
       (make-string org-project-manager-project-level
                    (string-to-char "*"))
       " "
       title
       "\n:PROPERTIES:\n:NICKNAME: "
       nickname
       "\n:END:\n")
      (org-set-tags-to "active"))))

(setq org-project-manager-default-project-content "")
(defvar org-project-manager-default-project-ignore "
export
*.html
*.pdf
*.png
")     
(defvar org-project-manager-project-directories '("analysis" "org" "data" "email" "export" "misc" "presentation" "manuscript"))

(defun org-project-manager-template (directory projectname)
  "Generate project skeleton"
   (interactive "DDirectory: 
sProject-name (short): ")
   (let* (my-project-cd my-proj-path my-proj-file)
     (setq my-proj-path (concat directory "/" projectname "/"))
     (setq my-proj-cd (concat "cd " my-proj-path ";"))
     (setq my-proj-file (concat my-proj-path projectname ".org"))

     (loop for dir in org-project-manager-project-directories
           do (make-directory (concat my-proj-path dir) t))
     (if (not (file-exists-p my-proj-file))
         (append-to-file org-project-manager-default-project-content nil my-proj-file)
       )
     (if (not (file-exists-p (concat my-proj-path ".git")))
         ((lambda ()       
           (shell-command (concat my-proj-cd "git init"))      
           (append-to-file org-project-manager-default-project-ignore nil (concat my-proj-path ".gitignore"))      
           (shell-command (concat my-proj-cd "git add *"))
           ))
       )
     (message "Created new project: " my-proj-path)                
     )     
)

(defvar org-project-manager-default-category "Miscellaneous")
(defun org-project-manager-add-project (nickname)
  "Get parameters"
  (interactive ;;"DDirectory: 
   "sProject-name (short): ")
  (org-project-manager-parse-categories)
  
  (let* ((category (completing-read
                    "Choose a category: "
                    org-project-manager-project-categories nil nil nil nil org-project-manager-default-category)) loc directory)
    (save-excursion
      (set-buffer (find-file-noselect org-project-manager))
      (save-buffer)
      (goto-char (point-min))
      (re-search-forward (concat ":CATEGORY: " category))  
      (setq loc (org-entry-get nil "LOCATION" 'inherit))
      )  
    (setq directory (read-directory-name
                     "Choose location: "
                     loc
                     ))
    (let (org-capture-templates)
      (setq org-capture-templates
            `(("p" "Project" plain (file+headline org-project-manager 
                                                  ,category)
               ,(concat (make-string org-project-manager-project-level
                                     (string-to-char "*"))
                        " ACTIVE %c%?\n:PROPERTIES:\n:NICKNAME: %c\n:END:\n")
               )))
      (org-project-manager-template directory nickname)
      (kill-new nickname)
      (org-capture nil "p")
      (pop kill-ring)
      )
    )
  )

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
    (save-excursion
      (set-buffer (find-file-noselect org-project-manager))
      (setq org-project-manager-project-categories
            (reverse (org-project-manager-get-buffer-props "CATEGORY")))))

(defun org-project-manager-project-agenda ()
    "Show an agenda of all the projects. Useful, e.g. for toggling
the active status of projects."
    (interactive)
    (find-file org-project-manager)
    (push ?t unread-command-events)
    (push ?< unread-command-events)
    (call-interactively 'org-agenda))
(defun org-project-manager-project-orgfile-at-point ()
    "Construct the orgfile of project at point. Return nil if
  a) the point is not at a project
  b) the construction of the orgile failed (needs property PROJECTORG, or both LOCATION and NICKNAME)"
    (if (not (eq (nth 0 (org-heading-components)) org-project-manager-project-level))
        (error "No project at point.")
      (let* ((indexfile (org-entry-get nil "INDEX"))
             (orgfile (org-entry-get nil "PROJECTORG"))
             (pro (org-entry-get nil "NICKNAME"))
             (loc (org-entry-get nil "LOCATION" 'inherit)))
        (cond (indexfile
               (if (file-name-absolute-p indexfile)
                   indexfile
                 (expand-file-name (concat (file-name-as-directory loc) pro "/" indexfile))))
              (orgfile
               (if (file-name-absolute-p orgfile)
                   orgfile
                 (expand-file-name (concat (file-name-as-directory loc) pro "/" orgfile))))
              (t
               (expand-file-name (concat (file-name-as-directory loc) pro
                                         org-project-manager-org-location
                                         pro ".org")))))))
  
;;     (defun org-project-manager-agenda ()
;;      (interactive)
;;      (let ((org-agenda-files
;;             (delq nil (mapcar '(lambda (x) (let ((f (org-project-manager-get-org x))) (if (file-exists-p f) f))) 
;;                               (org-project-manager-parse-projects))))
;;            (org-agenda-include-diary nil))
;;            (org-agenda-list)))

(defun org-project-manager-goto-project-manager ()
  (interactive)
  (find-file org-project-manager))

(defun org-project-manager-project-at-point ()
  (interactive)
  (if (org-before-first-heading-p)
      (message "No project at point")
    (org-project-manager-project-orgfile-at-point)))

  (defun org-project-manager-return ()
    (interactive)
    (if (org-project-manager-project-at-point)
        (org-project-manager-visit-project)
      (org-return)))
  
  (defun org-project-manager-visit-project ()
    (interactive)
    ;; FIXME: ask user if buffer org-project-manager needs saving
    ;;        and/or parsing
    (when (org-project-manager-project-at-point)
      (find-file (org-project-manager-project-orgfile-at-point))))

(defun org-project-manager-format-project (entry)
  (let ((cat (org-project-manager-get entry "category"))
        (coll (org-project-manager-get entry "collab"))
        (nickname (car entry)))
    (cons
     ;; (format format cat (if coll coll "") nickname)
     (concat cat "/" (if coll (concat coll "/")) (car entry))
     (car entry))))

(defun org-project-manager-select-project ()
    (org-project-manager-parse-projects 'all)
    (let* ((project-array (mapcar 'org-project-manager-format-project org-project-manager-project-alist))
           ;; (pro (completing-read "Project: " org-project-manager-project-alist)))
           (completion-ignore-case t)
           ;; (key (completing-read "Project: " project-array))
           ;; (key (org-icompleting-read "Project: " project-array))
           (key (ido-completing-read "Project: " (mapcar 'car project-array)))
           (nickname (cdr (assoc key project-array))))
           (assoc nickname org-project-manager-project-alist)))
    
(defun org-project-manager-switch-to-project ()
  (interactive)
  (if (and org-project-manager-current-project
           (not (eq last-command 'org-project-manager-switch-to-project)))
      (let ((index (or
                    (org-project-manager-get-index org-project-manager-current-project)
                    (org-project-manager-get-org org-project-manager-current-project))))
        (find-file index)
        (message "Press the same key again to switch project"))
    (let ((pro (org-project-manager-select-project)))
      (setq org-project-manager-current-project pro)
      (find-file (org-project-manager-get-index org-project-manager-current-project)))))

(defun org-project-manager-get (project el)
  (cdr (assoc el (cadr project))))

(defun org-project-manager-get-index (project)
  (cdr (assoc "index-file" (cadr project))))

(defun org-project-manager-get-org (project)
  (cdr (assoc "org-file" (cadr project))))

(defun org-project-manager-get-location (project)
  (cdr (assoc "location" (cadr project))))

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
        (loc (org-project-manager-get-location entry))
        (org (org-project-manager-get-org entry))
        (head (or heading "WorkFlow")))
      (if org
          (find-file org)
        (error "Project " pro " does not have an org-file."))
      (goto-char (point-min))
      (or (re-search-forward (concat "^[*]+ " heading) nil t)
          (when create
            (insert "* " heading "\n\n")
            (previous-line 1)))))))


(defun org-project-manager-goto-project-workflow ()
  (interactive)
  (or (org-project-manager-goto-project nil "WorkFlow" 'create)))

;; (org-project-manager-goto-project nil "WorkFlow" t)


(defun org-project-manager-goto-project-taskpool (&optional arg)
  (interactive)
  (if arg (org-store-link))
  (let* ((buf (current-buffer))
         (pro (completing-read "Select project: " org-project-manager-project-alist))
         (entry (assoc pro org-project-manager-project-alist))
         (loc (org-project-manager-get-location entry))
         (org (org-project-manager-get-org entry)))
    (if org
        (find-file org)
      (error "Project " pro " does not have an org-file."))
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
             (base-directory (file-name-as-directory
                              (concat (file-name-as-directory
                                       (org-project-manager-get-location entry))
                                      nickname)))
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
