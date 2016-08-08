;; -*- mode: racket; coding: utf-8 -*-

#lang racket

(require racket/gui/base
         framework
         pict
         pict/tree-layout
         pict/color
         pdf-read
         "../docs/estructuras.rkt"
         "../docs/lector.rkt")

(struct hoyo () #:transparent)

(define *expresión-actual*
  (hoyo))

(define *cursor* *expresión-actual*)

(define *realces* null)

(define *expresión-tex* "")

(define (cursor-abajo [cursor *cursor*])
  (if (abstracción? cursor)
      (abstracción-cuerpo cursor)
      cursor))

(define (cursor-izquierda [cursor *cursor*])
  (if (aplicación? cursor)
      (aplicación-operador cursor)
      cursor))

(define (cursor-derecha [cursor *cursor*])
  (if (aplicación? cursor)
      (aplicación-operando cursor)
      cursor))

(define (cursor-arriba [cursor *cursor*])
  (define (recorre actual)
    (cond [(variable? actual) #f]
          [(abstracción? actual)
           (if (eq? cursor (abstracción-cuerpo actual))
               actual
               (recorre (abstracción-cuerpo actual)))]
          [(aplicación? actual)
           (if (or (eq? cursor (aplicación-operador actual))
                   (eq? cursor (aplicación-operando actual)))
               actual
               (cond [(recorre (aplicación-operador actual)) => identity]
                     [else (recorre (aplicación-operando actual))]))]
          [else #f]))
  (cond [(recorre *expresión-actual*) => identity]
        [else cursor]))

(define (cursor-cambiar expresión [cursor *cursor*])
  (define (recorre actual)
    (cond [(eq? actual cursor)
           expresión]
          [(variable? actual)
           (variable (variable-nombre actual))]
          [(abstracción? actual)
           (abstracción (variable (variable-nombre (abstracción-argumento actual)))
                        (recorre (abstracción-cuerpo actual)))]
          [(aplicación? actual)
           (aplicación (recorre (aplicación-operador actual))
                       (recorre (aplicación-operando actual)))]
          [(hoyo? actual)
           (hoyo)]))
  (recorre *expresión-actual*))

(define (expresión->árbol e)
  (cond [(variable? e) (variable->árbol e)]
        [(abstracción? e) (abstracción->árbol e)]
        [(aplicación? e) (aplicación->árbol e)]
        [(hoyo? e) (hoyo->árbol e)]
        [else (error 'expresión->árbol "El objeto ~a no es una expresión lambda" e)]))



(define (variable->árbol e)
  (tree-layout #:pict (enmarcar (text (variable-nombre e)
                                      (if (memq e *realces*) (list (make-object color% "blue")) null))
                                #:borde? #f
                                #:color (if (eq? *cursor* e) "yellow" "white"))))

(define (abstracción->árbol e)
  (tree-layout #:pict (enmarcar (text (string-append "λ " (variable-nombre (abstracción-argumento e)))
                                      (if (memq e *realces*) (list (make-object color% "blue")) null))
                                #:borde? #t
                                #:color (if (eq? *cursor* e) "yellow" "white"))
               (tree-edge #:edge-width 2
                          (expresión->árbol (abstracción-cuerpo e)))))


(define (aplicación->árbol e)
  (tree-layout #:pict (filled-ellipse 10 10 #:draw-border? #t
                                      #:color (if (eq? *cursor* e) "yellow" "white")
                                      #:border-width 1
                                      #:border-color (if (memq e *realces*) "blue" #f))
               (tree-edge #:edge-width 2 (expresión->árbol (aplicación-operador e)))
               (tree-edge #:edge-width 2 (expresión->árbol (aplicación-operando e)))))

(define (hoyo->árbol e)
  (tree-layout #:pict (frame (enmarcar (text "?") #:borde? #f #:color (if (eq? *cursor* e) "yellow" "white"))
                             #:segment 4 #:color "black")))

(define (enmarcar x #:borde? [borde? #t] #:color [color "white"])
  (pin-over
   (filled-rectangle (+ (pict-width x) 10) (+ (pict-height x) 10)
                     #:draw-border? borde?
                     #:color color)
   5 5 x))

(define ventana (new frame%
                     [label "Editor lambda"]
                     [width  800] [min-width  200]
                     [height 600] [min-height 200]))

(define panel (new panel:vertical-dragable%
                   ;; vertical-panel%
                   [parent ventana]
                   [border 5] [spacing 5]))

(define lienzo-editor%
  (class canvas%
    (inherit refresh-now get-dc get-width get-height)
    (define/override (on-char evento)
      (gestionar-comando-teclas (send evento get-key-code)
                                (send evento get-control-down)
                                (send evento get-meta-down))
      (refresh-now)
      (send visor refresh-now))
    (define/override (on-paint)
      (define dc (get-dc))
      (define lienzo-ancho (get-width))
      (define lienzo-alto (get-height))
      (define figura (expresión->figura *expresión-actual* lienzo-ancho lienzo-alto))
      (define figura-ancho (pict-width figura))
      (define figura-alto (pict-height figura))
      (draw-pict figura dc
                 (- (/ lienzo-ancho 2) (/ figura-ancho 2))
                 (- (/ lienzo-alto 2) (/ figura-alto 2))))
    (super-new)))


(define *hash-vacío* (make-hash))

(define *combinaciones-teclas* (make-hash))

(hash-set! *combinaciones-teclas* '(meta) (make-hash))
(hash-set! *combinaciones-teclas* '(control) (make-hash))
(hash-set! *combinaciones-teclas* '(control meta) (make-hash))
(hash-set! *combinaciones-teclas* '() (make-hash))

(define (gestionar-comando-teclas tecla control? meta?)
  ((hash-ref (hash-ref *combinaciones-teclas*
                       (cond [(and control? meta?) '(control meta)]
                             [meta? '(meta)]
                             [control? '(control)]
                             [else '()])
                       *hash-vacío*)
             tecla (lambda () void))))

(define (agregar-comando-teclas lista procedimiento)
  (match lista
    [(list 'meta tecla)
     (hash-set! (hash-ref *combinaciones-teclas* '(meta)) tecla procedimiento)]
    [(list 'control tecla)
     (hash-set! (hash-ref *combinaciones-teclas* '(control)) tecla procedimiento)]
    [(list 'control 'meta tecla)
     (hash-set! (hash-ref *combinaciones-teclas* '(control meta)) tecla procedimiento)]
    [(list tecla)
     (hash-set! (hash-ref *combinaciones-teclas* '()) tecla procedimiento)]
    [x (error 'agregar-comando-teclas
              "El comando debe ser de la forma (list modificadores tecla) pero se obtuvo ~a" lista)]))

(agregar-comando-teclas '(up)
                        (lambda () (set! *cursor* (cursor-arriba))))
(agregar-comando-teclas '(meta #\w)
                        (lambda () (set! *cursor* (cursor-arriba))))

(agregar-comando-teclas '(down)
                        (lambda () (set! *cursor* (cursor-abajo))))
(agregar-comando-teclas '(meta #\s)
                        (lambda () (set! *cursor* (cursor-abajo))))

(agregar-comando-teclas '(left)
                        (lambda () (set! *cursor* (cursor-izquierda))))
(agregar-comando-teclas '(meta #\a)
                        (lambda () (set! *cursor* (cursor-izquierda))))

(agregar-comando-teclas '(right)
                        (lambda () (set! *cursor* (cursor-derecha))))
(agregar-comando-teclas '(meta #\d)
                        (lambda () (set! *cursor* (cursor-derecha))))

(agregar-comando-teclas
 '(#\e)
 (lambda ()
   (cond
     [(variable? *cursor*)
      (let ([nuevo-cursor (variable-del-usuario #:título "Edita variable")])
        (when (variable? nuevo-cursor)
          (set! *expresión-actual* (cursor-cambiar nuevo-cursor))
          (set! *cursor* nuevo-cursor)))]
     [(abstracción? *cursor*)
      (let ([nueva-variable (variable-del-usuario #:título "Edita abstracción"
                                                  #:descripción "Ingresa el nombre del argumento")])
        (when (variable? nueva-variable)
          (let ([nuevo-cursor (abstracción nueva-variable (clonar-expresión (abstracción-cuerpo *cursor*)))])
            (set! *expresión-actual* (cursor-cambiar nuevo-cursor))
            (set! *cursor* nuevo-cursor))))])))

(agregar-comando-teclas
 '(#\v)
 (lambda ()
   (when (hoyo? *cursor*)
     (let ([nuevo-cursor (variable-del-usuario)])
       (when (variable? nuevo-cursor)
         (let ([nueva-expresión (cursor-cambiar nuevo-cursor)])
           (set! *expresión-actual* nueva-expresión)
           (set! *cursor* nuevo-cursor)))))))

(agregar-comando-teclas
 '(#\l)
 (lambda ()
   (when (hoyo? *cursor*)
     (let ([argumento (variable-del-usuario #:título "Crea abstracción"
                                            #:descripción "Ingresa el nombre del argumento")])
       (when (variable? argumento)
         (let* ([nuevo-cursor (abstracción argumento (hoyo))]
                [nueva-expresión (cursor-cambiar nuevo-cursor)])
           (set! *expresión-actual* nueva-expresión)
           (set! *cursor* nuevo-cursor)))))))


(agregar-comando-teclas
 '(#\p)
 (lambda ()
   (when (hoyo? *cursor*)
     (let ([nuevo-cursor (aplicación (hoyo) (hoyo))])
       (set! *expresión-actual* (cursor-cambiar nuevo-cursor))
       (set! *cursor* nuevo-cursor)))))

(agregar-comando-teclas
 '(#\backspace)
 (lambda ()
   (let ([nuevo-cursor (hoyo)])
     (set! *expresión-actual* (cursor-cambiar nuevo-cursor))
     (set! *cursor* nuevo-cursor))))

(define *expresión-copiada* #f)

(define (clonar-expresión e)
  (cond
    [(variable? e)
     (variable (variable-nombre e))]
    [(abstracción? e)
     (abstracción (clonar-expresión (abstracción-argumento e))
                  (clonar-expresión (abstracción-cuerpo e)))]
    [(aplicación? e)
     (aplicación (clonar-expresión (aplicación-operador e))
                 (clonar-expresión (aplicación-operando e)))]
    [(hoyo? e)
     (hoyo)]))

(agregar-comando-teclas
 '(control #\c)
 (lambda ()
   (set! *expresión-copiada* (clonar-expresión *cursor*))))

(agregar-comando-teclas
 '(control #\x)
 (lambda ()
   (set! *expresión-copiada* (clonar-expresión *cursor*))
   (let ([vacío (hoyo)])
     (set! *expresión-actual* (cursor-cambiar vacío))
     (set! *cursor* vacío))))

(agregar-comando-teclas
 '(control #\v)
 (lambda ()
   (when *expresión-copiada*
     (set! *expresión-actual* (cursor-cambiar *expresión-copiada*))
     (set! *cursor* *expresión-copiada*)
     (set! *expresión-copiada* (clonar-expresión *expresión-copiada*)))))

(agregar-comando-teclas
 '(control #\i)
 (lambda ()
   (define entrada (texto-del-usuario "Crea expresión"
                                      "Ingresa una expresión"))
   (define expresión
     (with-handlers ([exn:fail? (lambda (exn) #f)])
       (leer (open-input-string entrada))))
   (when (término? expresión)
     (set! *expresión-actual* (cursor-cambiar expresión))
     (set! *cursor* expresión))))

(agregar-comando-teclas
 '(meta #\b)
 (lambda ()
   (when (abstracción? *cursor*)
     (set! *realces*
           (variables-ligadas (abstracción-cuerpo *cursor*)
                              (abstracción-argumento *cursor*))))))

(agregar-comando-teclas
 '(meta #\r)
 (lambda ()
   (set! *realces*
         (redexes *expresión-actual*))))

(agregar-comando-teclas
 '(control #\g)
 (lambda ()
   (set! *realces* null)))

(agregar-comando-teclas
 '(meta #\n)
 (lambda ()
   (let ([padre (cursor-arriba)])
     (when (aplicación? padre)
       (cond
         [(eq? (aplicación-operador padre) *cursor*)
          (set! *cursor* (aplicación-operando padre))]
         [(eq? (aplicación-operando padre) *cursor*)
          (let ([abuelo (cursor-arriba padre)])
            (when (aplicación? abuelo)
              (if (eq? (aplicación-operando abuelo) padre)
                  (void)
                  (set! *cursor* (aplicación-operando abuelo)))))])))))

(agregar-comando-teclas
 '(meta #\p)
 (lambda ()
   (let ([padre (cursor-arriba)])
     (when (aplicación? padre)
       (cond
         [(eq? (aplicación-operando padre) *cursor*)
          (let ([izquierda (aplicación-operador padre)])
            (if (aplicación? izquierda)
                (set! *cursor* (aplicación-operando izquierda))
                (set! *cursor* izquierda)))]
         [(eq? (aplicación-operador padre) *cursor*)
          (let ([abuelo (cursor-arriba padre)])
            (when (aplicación? abuelo)
              (if (eq? (aplicación-operador abuelo) padre)
                  (void)
                  (set! *cursor* (aplicación-operador abuelo)))))])))))

(define (variables-ligadas cuerpo enlazada)
  (cond
    [(variable? cuerpo)
     (if (string=? (variable-nombre cuerpo)
                   (variable-nombre enlazada))
         (list cuerpo)
         null)]
    [(abstracción? cuerpo)
     (if (string=? (variable-nombre (abstracción-argumento cuerpo))
                   (variable-nombre enlazada))
         null
         (variables-ligadas (abstracción-cuerpo cuerpo) enlazada))]
    [(aplicación? cuerpo)
     (append (variables-ligadas (aplicación-operador cuerpo) enlazada)
             (variables-ligadas (aplicación-operando cuerpo) enlazada))]
    [(hoyo? cuerpo)
     null]))

(define (redexes e)
  (cond
    [(variable? e) null]
    [(abstracción? e)
     (redexes (abstracción-cuerpo e))]
    [(aplicación? e)
     (append (redexes (aplicación-operador e))
             (if (abstracción? (aplicación-operador e))
                 (list e)
                 null)
             (redexes (aplicación-operando e)))]
    [(hoyo? e) null]))

(define (variable-del-usuario
         #:título [título "Crea variable"]
         #:descripción [descripción "Ingresa el nombre de la variable"]
         #:inicial [inicial (cond [(variable? *cursor*)
                                   (variable-nombre *cursor*)]
                                  [(abstracción? *cursor*)
                                   (variable-nombre (abstracción-argumento *cursor*))]
                                  [else ""])])
  (let ([nombre (texto-del-usuario título
                                   descripción
                                   #:inicial inicial
                                   #:valida valida-nombre-variable)])
    (if (and (string? nombre) (valida-nombre-variable nombre))
        (variable nombre)
        #f)))

(define (valida-nombre-variable str)
  (and (not (string=? str "")) (not (findf delimitador? (string->list str)))))


(define (texto-del-usuario título descripción
                           #:inicial [inicial ""]
                           #:valida [valida (lambda (str) #t)])
  (get-text-from-user título descripción ventana inicial '(disallow-invalid)
                      #:validate valida))


(define (expresión->figura e ancho alto)
  (let* ([original (naive-layered (expresión->árbol e))]
         [original-ancho (pict-width original)]
         [original-alto (pict-height original)])
    (scale original
           (min (* (/ ancho original-ancho) .9)
                (* (/ alto original-alto) .9)))))

(define editor (new lienzo-editor%
                    [parent panel]))

(send (send editor get-dc) set-smoothing 'smoothed)

;;;
(define (expresión->latex e)
  (cond [(variable? e)
         (cond
           [(eq? *cursor* e)
            (format " \\mathcolor{Gold}{~a} " (variable-nombre e))]
           [(memq e *realces*)
            (format " \\mathcolor{DodgerBlue}{~a} " (variable-nombre e))]
           [else
            (format " ~a " (variable-nombre e))])]
        [(abstracción? e)
         (cond
           [(eq? *cursor* e)
            (format " \\mathcolor{Gold}{( \\lambda ~a } . ~a \\mathcolor{Gold}{)}"
                    (expresión->latex (abstracción-argumento e))
                    (expresión->latex (abstracción-cuerpo e)))]
           [(memq e *realces*)
            (format " \\mathcolor{DodgerBlue}{( \\lambda ~a } . ~a \\mathcolor{DodgerBlue}{)}"
                    (expresión->latex (abstracción-argumento e))
                    (expresión->latex (abstracción-cuerpo e)))]
           [else
            (format " ( \\lambda ~a . ~a ) "
                    (expresión->latex (abstracción-argumento e))
                    (expresión->latex (abstracción-cuerpo e)))])]
        [(aplicación? e)
         (cond
           [(eq? *cursor* e)
            (format " \\mathcolor{Gold}{(} ~a\\ ~a \\mathcolor{Gold}{)} "
                    (expresión->latex (aplicación-operador e))
                    (expresión->latex (aplicación-operando e)))]
           [(memq e *realces*)
            (format " \\mathcolor{DodgerBlue}{(} ~a\\ ~a \\mathcolor{DodgerBlue}{)} "
                    (expresión->latex (aplicación-operador e))
                    (expresión->latex (aplicación-operando e)))]
           [else
            (format " ( ~a\\ ~a ) "
                    (expresión->latex (aplicación-operador e))
                    (expresión->latex (aplicación-operando e)))])]
        [(hoyo? e)
         (cond
           [(eq? *cursor* e)
            (format " \\mathcolor{Gold}{[ \\quad ]}")]
           [(memq e *realces*)
            (format " \\mathcolor{DodgerBlue}{[ \\quad ]}")]
           [else
            (format " [ \\quad ] ")])]))

(define *plantilla-latex*
  "\\documentclass[preview]{standalone}\n\\usepackage{amsmath}\n\\usepackage[svgnames]{xcolor}\n\\makeatletter\n\\def\\mathcolor#1#{\\@mathcolor{#1}}\n\\def\\@mathcolor#1#2#3{%\n\\protect\\leavevmode\n\\begingroup\n\\color#1{#2}#3%\n\\endgroup\n}\n\\makeatother\n\\begin{document}\n\\( ~a \\)\n\\end{document}")

(define *comando-latex*
  "pdflatex ~a")

(define *nombre-archivo-temporal* "temp")

(define (figura-latex str)
  (define dir (make-temporary-file "latex~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (parameterize ([current-directory dir]
                                 [current-input-port (open-input-bytes #"")]
                                 [current-output-port (open-output-string)])
                    (call-with-output-file* (string-append *nombre-archivo-temporal* ".tex") #:exists 'truncate
                                            (lambda (p) (fprintf p *plantilla-latex* str)))
                    (unless (system (format *comando-latex* (string-append *nombre-archivo-temporal* ".tex")))
                      (display (get-output-string (current-output-port))
                               (current-error-port))
                      (error 'latex
                             "La compilación a LaTeX ha fallado"))
                    (page->pict (string-append *nombre-archivo-temporal* ".pdf"))))
                (lambda ()
                  (delete-directory/files dir))))


(define lienzo-visor-tex%
  (class canvas%
    (inherit refresh-now get-dc get-width get-height)
    (define/override (on-paint)
      (define dc (get-dc))
      (define lienzo-ancho (get-width))
      (define lienzo-alto (get-height))
      (define figura
        (let* ([original (figura-latex (expresión->latex *expresión-actual*))]
               [ancho (pict-width original)]
               [alto (pict-height original)])
          (scale original
                 (min (* (/ lienzo-ancho ancho) .9)
                      (* (/ lienzo-alto alto) .9)))))
      (define figura-ancho (pict-width figura))
      (define figura-alto (pict-height figura))
      (draw-pict figura dc
                 (- (/ lienzo-ancho 2) (/ figura-ancho 2))
                 (- (/ lienzo-alto 2) (/ figura-alto 2))))
    (super-new)))

;; (define visor (new lienzo-visor-tex%
;;                    [parent panel]))

;; (send (send visor get-dc) set-smoothing 'smoothed)

(define lienzo-visor%
  (class canvas%
    (inherit refresh-now get-dc get-width get-height)
    (define/override (on-paint)
      (define dc (get-dc))
      (define lienzo-ancho (get-width))
      (define lienzo-alto (get-height))
      (define figura
        (let* ([original (expresión->texto-figura *expresión-actual*)]
               [ancho (pict-width original)]
               [alto (pict-height original)])
          (scale original
                 (min (* (/ lienzo-ancho ancho) .9)
                      (* (/ lienzo-alto alto) .9)))))
      (define figura-ancho (pict-width figura))
      (define figura-alto (pict-height figura))
      (send dc set-background "black")
      (send dc clear)
      (draw-pict figura dc
                 (- (/ lienzo-ancho 2) (/ figura-ancho 2))
                 (- (/ lienzo-alto 2) (/ figura-alto 2))))
    (super-new)))

(define (expresión->texto-figura e)
  (cond
    [(variable? e)
     (cond
       [(eq? *cursor* e)
        (yellow (text (variable-nombre e)))]
       [(memq e *realces*)
        (blue (text (variable-nombre e)))]
       [else
        (white (text (variable-nombre e)))])]
    [(abstracción? e)
     (cond
       [(eq? *cursor* e)
        (hbl-append (yellow (text "(λ"))
                    (yellow (text (variable-nombre (abstracción-argumento e))))
                    (yellow (text "."))
                    (expresión->texto-figura (abstracción-cuerpo e))
                    (yellow (text ")")))]
       [(memq e *realces*)
        (hbl-append (blue (text "(λ"))
                    (blue (text (variable-nombre (abstracción-argumento e))))
                    (blue (text "."))
                    (expresión->texto-figura (abstracción-cuerpo e))
                    (blue (text ")")))]
       [else
        (hbl-append (white (text "(λ"))
                    (expresión->texto-figura (abstracción-argumento e))
                    (white (text "."))
                    (expresión->texto-figura (abstracción-cuerpo e))
                    (white (text ")")))])]
    [(aplicación? e)
     (cond
       [(eq? *cursor* e)
        (hbl-append (yellow (text "("))
                    (expresión->texto-figura (aplicación-operador e))
                    (text " ")
                    (expresión->texto-figura (aplicación-operando e))
                    (yellow (text ")")))]
       [(memq e *realces*)
        (hbl-append (blue (text "("))
                    (expresión->texto-figura (aplicación-operador e))
                    (text " ")
                    (expresión->texto-figura (aplicación-operando e))
                    (blue (text ")")))]
       [else
        (hbl-append (white (text "("))
                    (expresión->texto-figura (aplicación-operador e))
                    (white (text " "))
                    (expresión->texto-figura (aplicación-operando e))
                    (white (text ")")))])]
    [(hoyo? e)
     (cond
       [(eq? *cursor* e)
        (yellow (text "[ ]"))]
       [(memq e *realces*)
        (blue (text "[ ]"))]
       [else
        (white (text "[ ]"))])]))

(define visor (new lienzo-visor%
                   [parent panel]))

(send (send visor get-dc) set-smoothing 'smoothed)

(send editor focus)
(send ventana show #t)

;; (get-text-from-user "fuck" "bla bla bla" #f "" '(disallow-invalid) #:validate (lambda (str) (string=? str "flow")))
