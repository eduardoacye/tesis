#lang racket

(require pict
         pdf-read)

(define *latex-template*
  "\\documentclass[preview]{standalone}\n\\usepackage{amsmath}\n\\begin{document}\n\\( ~a \\)\n\\end{document}")

(define *latex-command*
  "pdflatex ~a")

(define *temporary-filename* "temp")

(define (latex-pict str)
  (define dir (make-temporary-file "latex~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (parameterize ([current-directory dir]
                                 [current-input-port (open-input-bytes #"")]
                                 [current-output-port (open-output-string)])
                    (call-with-output-file* (string-append *temporary-filename* ".tex") #:exists 'truncate
                                            (lambda (p) (fprintf p *latex-template* str)))
                    (unless (system (format *latex-command* (string-append *temporary-filename* ".tex")))
                      (display (get-output-string (current-output-port))
                               (current-error-port))
                      (error 'latex
                             "La compilación a LaTeX ha fallado"))
                    (page->pict (string-append *temporary-filename* ".pdf"))))
                (lambda ()
                  (delete-directory/files dir))))
