(TeX-add-style-hook "S"
 (lambda ()
    (LaTeX-add-labels
     "sec-1")
    (TeX-run-style-hooks
     "listings"
     "color"
     "hyperref"
     "amsmath"
     "wrapfig"
     "float"
     "longtable"
     "graphicx"
     ""
     "inputenc"
     "utf8"
     "latex2e"
     "art11"
     "article"
     "11pt")))

