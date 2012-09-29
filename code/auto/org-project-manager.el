(TeX-add-style-hook "org-project-manager"
 (lambda ()
    (TeX-add-symbols
     '("alert" 1)
     '("sfootnote" 1)
     "blfootnote")
    (TeX-run-style-hooks
     "array"
     "attachfile"
     "xcolor"
     "table"
     "usenames"
     "dvipsnames"
     "natbib"
     "authblk"
     "listings"
     "color"
     "hyperref"
     "amssymb"
     "latexsym"
     "wasysym"
     "marvosym"
     "textcomp"
     "soul"
     "wrapfig"
     "float"
     "longtable"
     "graphicx"
     "fixltx2e"
     "fontenc"
     "T1"
     "inputenc"
     "utf8"
     "latex2e"
     "art11"
     "article"
     "11pt")))

