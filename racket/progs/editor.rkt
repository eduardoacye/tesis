;; -*- mode: racket; coding: utf-8 -*-

#lang racket

(require racket/gui/base
         pict
         pict/tree-layout
         pdf-read)

;;; Auxiliares str -> tex -> pdf -> pict
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
;;;

(struct variable (nombre) #:transparent)
(struct abstracción (argumento cuerpo) #:transparent)
(struct aplicación (operador operando) #:transparent)
(struct metainstrucción (nombre argumentos) #:transparent)

(define (cursor-abajo #:cursor [cursor *cursor*])
  (match cursor
    [(variable nombre) cursor]
    [(abstracción argumento cuerpo) cuerpo]
    [(aplicación operador operando) cursor]
    [else cursor]))

(define (cursor-izquierda #:cursor [cursor *cursor*])
  (match cursor
    [(variable nombre) cursor]
    [(abstracción argumento cuerpo) cursor]
    [(aplicación operador operando) operador]
    [else cursor]))

(define (cursor-derecha #:cursor [cursor *cursor*])
  (match cursor
    [(variable nombre) cursor]
    [(abstracción argumento cuerpo) cursor]
    [(aplicación operador operando) operando]
    [else cursor]))

(define (cursor-arriba #:cursor [cursor *cursor*] #:raíz [raíz *expresión-actual*])
  (define (recorre raíz)
    (cond [(variable? raíz) #f]
          [(abstracción? raíz)
           (if (eq? cursor (abstracción-cuerpo raíz))
               raíz
               (recorre (abstracción-cuerpo raíz)))]
          [(aplicación? raíz)
           (if (or (eq? cursor (aplicación-operador raíz))
                   (eq? cursor (aplicación-operando raíz)))
               raíz
               (let ([prueba (recorre (aplicación-operador raíz))])
                 (if prueba prueba (recorre (aplicación-operando raíz)))))]
          [else #f]))
  (let ([resultado (recorre raíz)])
    (if resultado
        resultado
        cursor)))

(define *expresión-actual*
  (abstracción
   (variable "f")
   (aplicación
    (abstracción (variable "x")
                 (aplicación (variable "f")
                             (aplicación (variable "x")
                                         (variable "x"))))
    (abstracción (variable "x")
                 (aplicación (variable "f")
                             (aplicación (variable "x")
                                         (variable "x"))))))
  )

(define *cursor*
  (abstracción-cuerpo (aplicación-operando (abstracción-cuerpo *expresión-actual*))))

(define *expresión-figura-tex*
  (latex-pict "(\\lambda f.((\\lambda x.(f (x\\, x)))\\,(\\lambda x.(f (x\\, x)))))")
  )

(define (expresión->árbol e)
  (cond
    [(variable? e) (variable->árbol e)]
    [(abstracción? e) (abstracción->árbol e)]
    [(aplicación? e) (aplicación->árbol e)]
    [else (error 'expresión->árbol "No es una expresión" e)]))

(define (variable->árbol v)
  (tree-layout #:pict (enmarcar (text (variable-nombre v))
                                #:border? #f
                                #:color (if (eq? *cursor* v) "yellow" "white"))))

(define (abstracción->árbol a)
  (tree-layout #:pict (enmarcar (text (string-append "λ " (variable-nombre (abstracción-argumento a))))
                                #:border? #t
                                #:color (if (eq? *cursor* a) "yellow" "white"))
               (tree-edge #:edge-width 2
                          (expresión->árbol (abstracción-cuerpo a)))))

(define (aplicación->árbol a)
  (tree-layout #:pict (filled-ellipse 10 10 #:draw-border? #t
                                      #:color (if (eq? *cursor* a) "yellow" "white")
                                      #:border-width 1)
               (tree-edge #:edge-width 2 (expresión->árbol (aplicación-operador a)))
               (tree-edge #:edge-width 2 (expresión->árbol (aplicación-operando a)))))

(define (enmarcar x #:color [color "white"] #:border? [border? #t])
  (define w (pict-width x))
  (define h (pict-height x))
  (define r (filled-rectangle (+ w 10) (+ h 10)
                              #:draw-border? border?
                              #:color color))
  (pin-over r 5 5 x))

(define ventana (new frame%
                     [label "Editor Lambda"]
                     [width 800] [min-width 200]
                     [height 600] [min-height 200]))

(define panel (new vertical-panel%
                   [parent ventana]
                   [border 5]
                   [spacing 5]))

(define lienzo-árbol%
  (class canvas%
    (inherit refresh-now get-dc get-width get-height)
    (define minibuffer-run! #f)
    (define minibuffer-prompt
      (make-continuation-prompt-tag 'minibuffer))
    (define-syntax-rule (with-minibuffer key e)
      (call-with-continuation-prompt
       (lambda ()
         (if minibuffer-run!
             (minibuffer-run! key)
             e))
       minibuffer-prompt))
    (define (minibuffer-read prompt)
      (begin0
        (call/cc
         (lambda (return-to-minibuffer-call)
           (define input-so-far "")
           (set! minibuffer-run!
                 (lambda (key)
                   (match (send key get-key-code)
                     [#\return
                      (return-to-minibuffer-call input-so-far)]
                     [#\backspace
                      (unless (string=? "" input-so-far)
                        (set! input-so-far
                              (substring input-so-far 0 (sub1 (string-length input-so-far)))))]
                     [(and (? char? c)
                           (or (? char-alphabetic?)
                               (? char-numeric?)))
                      (set! input-so-far
                            (string-append input-so-far (string c)))]
                     ['escape
                      (set! minibuffer-run! #f)
                      (set! input-so-far "[canceled]")]
                     [_ (void)])
                   (format "~a > ~a" prompt input-so-far)))
           (abort-current-continuation
            minibuffer-prompt
            (lambda () (format "~a > " prompt))))
        minibuffer-prompt)
        (set! minibuffer-run! #f)))
    (define/override (on-char key)
      (define new-status
        (with-minibuffer key
          (match (send key get-key-code)
            ['up    (set! *cursor* (cursor-arriba)) "arriba"]
            ['down  (set! *cursor* (cursor-abajo)) "abaho"]
            ['left  (set! *cursor* (cursor-izquierda)) "izquierda"]
            ['right (set! *cursor* (cursor-derecha)) "derecha"]
            [#\e (minibuffer-read "Nombre de variable")]
            [#\q (exit 0)]
            [x (printf "Se capturó ~v\n" x) #f])))
      (when new-status
        (send ventana set-status-text
              (format "~a" new-status)))
      ;(match (send key get-key-code)
      ;  ['up    (set! *cursor* (cursor-arriba))]
      ;  ['down  (set! *cursor* (cursor-abajo))]
      ;  ['left  (set! *cursor* (cursor-izquierda))]
      ;  ['right (set! *cursor* (cursor-derecha))]
      ;  [x (printf "Se capturó ~v\n" x)])
      (refresh-now))
    (define/override (on-paint)
      (define dc (get-dc))
      (define figura (naive-layered (expresión->árbol *expresión-actual*)))
      (define fw (pict-width figura))
      (define fh (pict-height figura))
      (define lw (get-width))
      (define lh (get-height))
      (define escalada
        (if (< (- lw fw) (- lh fh))
            (scale figura (* (/ lw fw) .9))
            (scale figura (* (/ lh fh) .9))))
      (define ew (pict-width escalada))
      (define eh (pict-height escalada))
      (draw-pict escalada dc (- (/ lw 2) (/ ew 2)) (- (/ lh 2) (/ eh 2))))
    (super-new)))

(define lienzo-árbol (new lienzo-árbol%
                         [parent panel]))

(send (send lienzo-árbol get-dc) set-smoothing 'smoothed)

(define lienzo-tex%
  (class canvas%
    (inherit refresh-now get-dc get-width get-height)
    (define/override (on-paint)
      (define dc (get-dc))
      (define figura *expresión-figura-tex*)
      (define escalada
        (let ([w (get-width)]
              [h (get-height)])
          (if (< (+ w 40) (+ h 40))
              (scale figura (/ (- w 40) (pict-width figura)))
              (scale figura (/ (- h 40) (pict-height figura))))))
      (draw-pict escalada dc 20 20))
    (super-new)))

(define lienzo-salida (new lienzo-tex%
                           [parent panel]
                           [style '(hscroll no-focus)]
                           [stretchable-height #f]
                           [min-height 75]))

(send (send lienzo-salida get-dc) set-smoothing 'smoothed)
(send lienzo-salida init-auto-scrollbars 1280 900 0 0)

(send ventana create-status-line)
(send lienzo-árbol focus)
(send ventana show #t)
