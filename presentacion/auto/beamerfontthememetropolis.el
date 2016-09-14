(TeX-add-style-hook
 "beamerfontthememetropolis"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("fontspec" "no-math")))
   (TeX-run-style-hooks
    "etoolbox"
    "ifxetex"
    "ifluatex"
    "pgfopts"
    "fontspec")
   (TeX-add-symbols
    '("iffontsavailable" 3)
    '("checkfont" 1)
    "x"
    "metropolis"
    "inserttitle"
    "insertsubtitle"
    "insertsectionhead")
   (LaTeX-add-counters
    "fontsnotfound"))
 :latex)

