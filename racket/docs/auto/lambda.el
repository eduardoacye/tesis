(TeX-add-style-hook
 "lambda"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("book" "letterpaper" "twoside" "openright" "10pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "lmargin=1.4in" "rmargin=.8in" "tmargin=1in" "bmargin=1in") ("inputenc" "utf8") ("fontenc" "T1") ("amsmath" "sumlimits") ("textcomp" "full") ("newpxtext" "osf") ("inconsolata" "varqu" "varl") ("newpxmath" "bigdelims" "vvarbb") ("mathalfa" "cal=boondoxo")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "book"
    "bk10"
    "setspace"
    "geometry"
    "inputenc"
    "fontenc"
    "tikz"
    "amsmath"
    "amssymb"
    "amsthm"
    "textcomp"
    "newpxtext"
    "cabin"
    "inconsolata"
    "newpxmath"
    "mathalfa"
    "proof"
    "xcolor"
    "enumerate"
    "newunicodechar"
    "noweb"
    "hyperref")
   (TeX-add-symbols
    '("cn" 1)
    '("convertible" 1)
    '("xreduce" 2)
    '("reduce" 1)
    '("xcontract" 2)
    '("contract" 1)
    '("subst" 3)
    "bs"
    "mc"
    "synteq"))
 :latex)

