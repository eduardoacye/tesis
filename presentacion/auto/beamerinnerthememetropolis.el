(TeX-add-style-hook
 "beamerinnerthememetropolis"
 (lambda ()
   (TeX-run-style-hooks
    "etoolbox"
    "keyval"
    "calc"
    "pgfopts"
    "tikz")
   (TeX-add-symbols
    "metropolis"
    "maketitle"
    "titlepage"
    "inserttotalframenumber")
   (LaTeX-add-lengths
    "metropolis"))
 :latex)

