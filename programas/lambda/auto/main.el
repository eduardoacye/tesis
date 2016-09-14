(TeX-add-style-hook
 "main"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("book" "letterpaper" "twoside" "openright" "11pt") ("standalone" "preview")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "lmargin=1.4in" "rmargin=1.0in" "tmargin=1.0in" "bmargin=1.0in") ("inputenc" "utf8") ("babel" "spanish" "es-minimal") ("fontenc" "T1") ("mathalfa" "cal=boondoxo") ("inconsolata" "varqu" "varl") ("microtype" "activate={true,nocompatibility}" "final" "tracking=true" "kerning=true" "spacing=true" "factor=1100" "stretch=10" "shrink=10") ("hyperref" "hidelinks")))
   (TeX-run-style-hooks
    "latex2e"
    "book"
    "bk11"
    "emptypage"
    "geometry"
    "inputenc"
    "babel"
    "fontenc"
    "enumerate"
    "enumitem"
    "amsmath"
    "amssymb"
    "proof"
    "centernot"
    "tikz"
    "fancyhdr"
    "kpfonts"
    "mathalfa"
    "inconsolata"
    "microtype"
    "noweb"
    "newunicodechar"
    "hyperref"
    "standalone"
    "standalone10")
   (TeX-add-symbols
    "titulo"
    "subtitulo"
    "autor"
    "lugar"
    "institucion"
    "programa"
    "departamento"
    "division"
    "fecha")
   (LaTeX-add-labels
    "ap2:piezas-fundamentales"
    "ap2:entorno-interactivo"
    "ap2:extensiones")
   (LaTeX-add-lengths
    "titlepagetop"
    "titlepageby"
    "titlepagemiddle"))
 :latex)

