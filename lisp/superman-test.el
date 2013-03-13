
(defun mymesg (&optional arg)
  (interactive)
  (message (concat "hey: " arg)))

(defun tt (&optional dont-redo)
  (interactive)
  (superman-loop 'mymesg (list 'dont) nil nil `(face ,superman-mark-face)))
