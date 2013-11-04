(TeX-add-style-hook "sample-beamer"
 (lambda ()
    (LaTeX-add-labels
     "sec-1"
     "sec-1-1")
    (TeX-add-symbols
     '("sfootnote" 1)
     "blfootnote")
    (TeX-run-style-hooks
     "fancyvrb"
     "array"
     "attachfile"
     "natbib"
     "listings"
     "color"
     "latex2e"
     "beamer10"
     "beamer")))

