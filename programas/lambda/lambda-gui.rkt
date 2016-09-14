#lang racket/gui
(require "lambda-lib.rkt")
(require framework)
(require (except-in parser-tools/lex
                    blank))
(require (prefix-in : parser-tools/lex-sre))
(require pict)
(require pict/snip)
(require pict/color)
(require pict/code)
(require pict/tree-layout)

(define-lex-abbrevs
  [paréntesis (:or #\( #\) #\[ #\])]
  [delimitadores (:or #\, #\. #\\ #\u3BB #\;)])

(define lexer-lambda
  (lexer
   [(:+ (:~ delimitadores paréntesis whitespace))
    (values lexeme 'otro #f
            (position-offset start-pos)
            (position-offset end-pos))]
   [(:+ whitespace)
    (values lexeme 'white-space #f
            (position-offset start-pos)
            (position-offset end-pos))]
   [delimitadores
    (values lexeme 'delimitadores #f
            (position-offset start-pos)
            (position-offset end-pos))]
   [paréntesis
    (values lexeme 'delimitadores (string->symbol lexeme)
            (position-offset start-pos)
            (position-offset end-pos))]
   [(eof)
    (values lexeme 'eof #f #f #f)]))

(define editor-texto%
  (class color:text%
    (super-new)
    (define expresión null)
    (define/augment (after-insert start len)
      (if (= len 1)
          (let ([cadena (send this get-text start (+ start 1))])
            (unless (char-whitespace? (string-ref cadena 0))
              (parsear)))
          (parsear)))
    (define/augment (after-delete start len)
      (parsear))
    (define/public (parsear)
      (with-handlers ([exn:fail? (lambda (err) (set! expresión null))])
        (let ([e (parsear-cadena (send this get-text))])
          (if (eof-object? e)
              (set! expresión null)
              (set! expresión e))))
      (reportar-cambio-editor expresión))
    (define/public (obtener-expresión)
      expresión)
    (define/public (cambiar-expresión e)
      (send this erase)
      (send this insert (if (null? e) "" (escribir-expresión-formal e)))
      (parsear))
    (send this start-colorer
          (lambda (what)
            (case what
              [(delimitadores paréntesis) "Realzado"]
              [else "Sencillo"]))
          lexer-lambda
          (list (list '|(| '|)|)
                (list '|[| '|]|)))
    (send this set-tabs null 2 #f)
    ;; Estilos
    (define estilo-realzado (make-object style-delta% 'change-size 10))
    (send estilo-realzado set-weight-on 'bold)
    (send estilo-realzado set-family 'modern)
    (editor:set-standard-style-list-delta "Realzado" estilo-realzado)
    (define estilo-sencillo (make-object style-delta% 'change-size 10))
    (send estilo-sencillo set-family 'modern)
    (editor:set-standard-style-list-delta "Sencillo" estilo-sencillo)
    ;; Atajos
    (define atajos (new keymap%))
    (define-syntax atajo
      (syntax-rules ()
        [(_ (nombre . argumentos) teclas cuerpo ...)
         (let ([nombre* nombre]
               [teclas* teclas])
           (send atajos add-function nombre*
                 (lambda argumentos
                   cuerpo ...))
           (send atajos map-function teclas* nombre*))]))
    (for-each (lambda (normal griego)
                (atajo ((format "inserta-~a" griego) editor evento)
                       (format "m:~a" normal)
                       (send editor insert (format "~a" griego))))
              (string->list "abcdefghijklmnoprstuvwxyzCDFGJLPSUVW")
              '(#\u3b1 #\u3b2 #\u3c8 #\u3b4 #\u3b5 #\u3c6 #\u3b3 #\u3b7 #\u3b9 #\u3be
                       #\u3ba #\u3bb #\u3bc #\u3bd #\u3bf #\u3c0 #\u3c1 #\u3c3 #\u3c4
                       #\u3b8 #\u3c9 #\u3c2 #\u3c7 #\u3c5 #\u3b6 #\u3a8 #\u394 #\u3a6
                       #\u393 #\u39e #\u39b #\u3a0 #\u3a3 #\u398 #\u3a9 #\u3a3))
    (atajo ("selecciona-todo" editor evento) "c:a"
           (send editor do-edit-operation 'select-all))
    (atajo ("deshacer" editor evento) "c:z"
           (send editor do-edit-operation 'undo))
    (atajo ("rehacer" editor evento) "c:y"
           (send editor do-edit-operation 'redo))
    (atajo ("copiar" editor evento) "c:c"
           (send editor do-edit-operation 'copy))
    (atajo ("pegar" editor evento) "c:v"
           (send editor do-edit-operation 'paste))
    (atajo ("cortar" editor evento) "c:x"
           (send editor do-edit-operation 'cut))
    (atajo ("borrar-línea" editor evento) "c:k"
           (send editor do-edit-operation 'kill))
    (atajo ("enviar-expresión" editor evento) "c:enter"
           (enviar-expresión (send editor obtener-expresión)))
    (send this set-keymap atajos)))
(define fuente-texto-figura
  (make-object font% 12 'modern 'normal 'bold))

(define (text* x)
  (text x fuente-texto-figura))

(define (expresión->dibujo expresión cursor índice)
  (define (p-fondo p color)
    (let ([ancho (pict-width p)]
          [alto  (pict-height p)])
      (cc-superimpose (color (filled-rectangle ancho alto)) p)))
  (define (p-nombre e nombre)
    (if (and (eq? e cursor) (or (variable? e) (metainstrucción? e)))
        (let ([n (string-length nombre)])
          (if (< índice n)
              (p-fondo (hbl-append (text* (substring nombre 0 índice))
                                   (frame (blue (text* (substring nombre índice
                                                                  (+ índice 1))))
                                          #:color "blue"
                                          #:line-width .5)
                                   (text* (substring nombre (+ índice 1) n)))
                       red)
              (p-fondo (hbl-append (text* nombre)
                                   (frame (blue (text* "•")) #:color "blue")) red)))
        (let ([p (text* nombre)])
          (p-fondo p (if (eq? e cursor) red white)))))
  (define (p-lambda e argumentos)
    (define en-cursor? #f)
    (define figuras (map (lambda (x)
                           (if (and (abstracción? cursor)
                                    (eq? x (abstracción-argumento cursor)))
                               (begin
                                 (set! en-cursor? #t)
                                 (frame (naive-layered (escribir-dibujo-breve x))
                                        #:color "red" #:line-width 2))
                               (naive-layered (escribir-dibujo-breve x))))
                         argumentos))
    (p-fondo (apply hbl-append (p-nombre (if en-cursor? cursor e) (string #\u3bb))   
                    (text* "  ")
                    (add-between figuras (text* "  ")))
             white))
  (define (e-variable e nombre)
    (tree-layout #:pict (p-nombre e nombre)))
  (define (e-abstracción e argumentos cuerpo)
    (tree-layout #:pict (p-lambda e argumentos)
                 (tree-edge #:edge-width 2
                            (escribir-dibujo-breve cuerpo))))
  (define (e-aplicación e operandos paréntesis)
    (define en-cursor? #f)
    (define hijos (map (lambda (x)
                         (if (and (aplicación? cursor)
                                  (eq? x (aplicación-operando cursor)))
                             (begin
                               (set! en-cursor? #t)
                               (escribir-dibujo-breve x))
                             (escribir-dibujo-breve x)))
                       operandos))
    (apply tree-layout #:pict (disk 10
                                    #:color (if en-cursor? "red" "white")
                                    #:border-width 2)
           (tree-edge #:edge-width 2
                      #:edge-color (if en-cursor? "red" "gray")
                      (car hijos))
           (map (lambda (e x)
                  (tree-edge #:edge-width 2
                             #:edge-color
                             (if en-cursor?
                                 (if (eq? e (aplicación-operando cursor))
                                     (begin
                                       (set! en-cursor? #f)
                                       "red")
                                     "red")
                                 "gray")
                             x))
                (cdr operandos)
                (cdr hijos))))
  (define (e-hueco e)
    (tree-layout #:pict (p-nombre e "[ ]")))
  (define (e-metainstrucción e nombre argumentos)
    (define en-cursor? (eq? e cursor))
    (tree-layout
     #:pict (p-nombre e nombre)
     (tree-edge #:edge-width 2
                #:edge-color (if en-cursor? "red" "gray")
                (tree-layout
                 #:pict
                 (p-fondo
                  (apply ht-append
                         (append (list ((if en-cursor? red black) (text* "[ ")))
                                 (add-between
                                  (map (lambda (x)
                                         (naive-layered (escribir-dibujo-breve x)))
                                       argumentos)
                                  ((if en-cursor? red black) (text* " , ")))
                                 (list ((if en-cursor? red black) (text* " ]")))))
                  white)))))
  (define escribir-dibujo-breve
    (escritor-breve e-variable e-abstracción e-aplicación
                    e-hueco e-metainstrucción))
  (if (null? expresión)
      (blank 50)
      (naive-layered (escribir-dibujo-breve expresión))))

(define (escalar-dibujo dibujo ancho alto)
  (let* ([dibujo-ancho (pict-width dibujo)]
         [dibujo-alto (pict-height dibujo)])
    (scale dibujo
           (min (* (/ ancho dibujo-ancho) .9)
                (* (/ alto dibujo-alto) .9)))))

(define editor-estructural%
  (class canvas%
    (inherit refresh-now get-dc get-width get-height)
    (super-new)
    (define expresión null)
    (define cursor expresión)
    (define índice 0)
    (define atajos (new keymap%))
    (define/override (on-char evento)
      (let ([código (send evento get-key-code)])
        (if (and (char? código)
                 (or (char-alphabetic? código)
                     (char-numeric? código)
                     (member código (string->list "+-_~:<>=?¿!@#$%^&*") char=?))
                 (not (send evento get-control-down))
                 (not (send evento get-meta-down)))
            (send this inserta (send evento get-key-code))
            (send atajos handle-key-event this evento))))
    (define/override (on-paint)
      (define dc (get-dc))
      (define editor-ancho (get-width))
      (define editor-alto (get-height))
      (define dibujo (escalar-dibujo (expresión->dibujo expresión cursor índice)
                                     editor-ancho editor-alto))
      (define dibujo-ancho (pict-width dibujo))
      (define dibujo-alto (pict-height dibujo))
      (draw-pict dibujo dc
                 (- (/ editor-ancho 2) (/ dibujo-ancho 2))
                 (- (/ editor-alto 2) (/ dibujo-alto 2))))
    (define/public (obtener-expresión)
      expresión)
    (define/public (inserta caracter)
      (when (or (variable? cursor) (metainstrucción? cursor))
        (let ([selector (if (variable? cursor)
                            variable-nombre
                            metainstrucción-nombre)]
              [mutador (if (variable? cursor)
                           set-variable-nombre!
                           set-metainstrucción-nombre!)])
          (if (= índice (string-length (selector cursor)))
              (mutador cursor
                       (string-append
                        (selector cursor)
                        (string caracter)))
              (mutador cursor
                       (string-append
                        (substring (selector cursor) 0 índice)
                        (string caracter)
                        (substring (selector cursor)
                                   índice
                                   (string-length (selector cursor)))))))
        (set! índice (+ índice 1))
        (send this refresh-now)))
    (define/public (elimina)
      (when (and (or (variable? cursor) (metainstrucción? cursor))
                 (> índice 0)
                 (> (string-length ((if (variable? cursor)
                                        variable-nombre
                                        metainstrucción-nombre)
                                    cursor))
                    1))
        (let* ([selector (if (variable? cursor)
                             variable-nombre
                             metainstrucción-nombre)]
               [mutador (if (variable? cursor)
                            set-variable-nombre!
                            set-metainstrucción-nombre!)]
               [nombre (selector cursor)]
               [n (string-length nombre)])
          (if (= índice n)
              (mutador cursor
                       (substring nombre 0 (- índice 1)))
              (mutador cursor
                       (string-append (substring nombre 0 (- índice 1))
                                      (substring nombre índice n)))))
        (set! índice (- índice 1))
        (send this refresh-now)))
    (define/public (borrar)
      (set! expresión null)
      (set! cursor expresión)
      (set! índice 0)
      (send this refresh-now))
    (define/public (cambiar-expresión e)
      (set! expresión e)
      (set! cursor expresión)
      (set! índice 0)
      (send this refresh-now))
    ;; Atajos
    (define-syntax atajo
      (syntax-rules ()
        [(_ (nombre . argumentos) teclas cuerpo ...)
         (let ([nombre* nombre]
               [teclas* teclas])
           (send atajos add-function nombre*
                 (lambda argumentos
                   cuerpo ...))
           (send atajos map-function teclas* nombre*))]))
    (atajo ("expresión-abajo" editor evento) "c:s"
           (cond [(abstracción? cursor)
                  (set! cursor (abstracción-cuerpo cursor))
                  (set! índice 0)
                  (send editor refresh-now)]
                 [(aplicación? cursor)
                  (set! cursor (aplicación-operando cursor))
                  (set! índice 0)
                  (send editor refresh-now)]
                 [(and (metainstrucción? cursor)
                       (not (null? (metainstrucción-argumentos cursor))))
                  (set! cursor (car (metainstrucción-argumentos cursor)))
                  (set! índice 0)
                  (send editor refresh-now)]))
    (atajo ("expresión-izquierda" editor evento) "c:a"
           (cond [(aplicación? cursor)
                  (set! cursor (aplicación-operador cursor))
                  (set! índice 0)
                  (send editor refresh-now)]))
    (atajo ("expresión-derecha" editor evento) "c:d"
           (cond [(abstracción? cursor)
                  (set! cursor (abstracción-argumento cursor))
                  (set! índice 0)
                  (send editor refresh-now)]))
    (letrec ([recorre
              (lambda (x)
                (cond [(variable? x) #f]
                      [(abstracción? x)
                       (if (or (eq? cursor (abstracción-cuerpo x))
                               (eq? cursor (abstracción-argumento x)))
                           x
                           (recorre (abstracción-cuerpo x)))]
                      [(aplicación? x)
                       (if (or (eq? cursor (aplicación-operador x))
                               (eq? cursor (aplicación-operando x)))
                           x
                           (cond [(recorre (aplicación-operador x))
                                  => identity]
                                 [else (recorre (aplicación-operando x))]))]
                      [(hueco? x) #f]
                      [(metainstrucción? x)
                       (if (memq cursor (metainstrucción-argumentos x))
                           x
                           (let itera ([argumentos (metainstrucción-argumentos x)])
                             (cond [(null? argumentos) #f]
                                   [(recorre (car argumentos))
                                    => identity]
                                   [else (itera (cdr argumentos))])))]
                      [else #f]))])
      (atajo ("expresión-arriba" editor evento) "c:w"
             (cond [(recorre expresión)
                    => (lambda (x)
                         (set! cursor x)
                         (set! índice 0)
                         (send editor refresh-now))]))
      (atajo ("expresión-siguiente" editor evento) "c:e"
             (cond [(recorre expresión)
                    => (lambda (p)
                         (when (metainstrucción? p)
                           (let itera ([argumentos (metainstrucción-argumentos p)])
                             (cond [(null? argumentos) (void)]
                                   [(null? (cdr argumentos)) (void)]
                                   [(eq? (car argumentos) cursor)
                                    (set! cursor (cadr argumentos))
                                    (set! índice 0)
                                    (send editor refresh-now)]
                                   [else (itera (cdr argumentos))]))))]))
      (atajo ("expresión-anterior" editor evento) "c:q"
             (cond [(recorre expresión)
                    => (lambda (p)
                         (when (metainstrucción? p)
                           (let itera ([argumentos (metainstrucción-argumentos p)])
                             (cond [(null? argumentos) (void)]
                                   [(null? (cdr argumentos)) (void)]
                                   [(eq? (cadr argumentos) cursor)
                                    (set! cursor (car argumentos))
                                    (set! índice 0)
                                    (send editor refresh-now)]
                                   [else (itera (cdr argumentos))]))))]))
      (atajo ("variable-aquí" editor evento) "c:1"
             (let ([x (variable "x")])
               (cond [(recorre expresión)
                      => (lambda (p)
                           (cond [(abstracción? p)
                                  (if (eq? cursor (abstracción-argumento p))
                                      (set-abstracción-argumento! p x)
                                      (set-abstracción-cuerpo! p x))]
                                 [(aplicación? p)
                                  (if (eq? cursor (aplicación-operador p))
                                      (set-aplicación-operador! p x)
                                      (set-aplicación-operando! p x))]
                                 [(metainstrucción? p)
                                  (set-metainstrucción-argumentos! p
                                   (map (lambda (e)
                                          (if (eq? e cursor) x e))
                                        (metainstrucción-argumentos p)))]))]
                     [else
                      (set! expresión x)])
               (set! cursor x)
               (set! índice 0)
               (send editor refresh-now)))
      (atajo ("abstracción-aquí" editor evento) "c:2"
             (let ([x (abstracción (variable "x") (variable "x"))])
               (cond [(recorre expresión)
                      => (lambda (p)
                           (cond [(abstracción? p)
                                  (set-abstracción-cuerpo! p x)]
                                 [(aplicación? p)
                                  (if (eq? cursor (aplicación-operador p))
                                      (set-aplicación-operador! p x)
                                      (set-aplicación-operando! p x))]
                                 [(metainstrucción? p)
                                  (set-metainstrucción-argumentos! p
                                   (map (lambda (e)
                                          (if (eq? e cursor) x e))
                                        (metainstrucción-argumentos p)))]))]
                     [else
                      (set! expresión x)])
               (set! cursor x)
               (set! índice 0)
               (send editor refresh-now)))
      (atajo ("aplicación-aquí" editor evento) "c:3"
             (let ([x (aplicación (variable "x") (variable "x"))])
               (cond [(recorre expresión)
                      => (lambda (p)
                           (cond [(abstracción? p)
                                  (set-abstracción-cuerpo! p x)]
                                 [(aplicación? p)
                                  (if (eq? cursor (aplicación-operador p))
                                      (set-aplicación-operador! p x)
                                      (set-aplicación-operando! p x))]
                                 [(metainstrucción? p)
                                  (set-metainstrucción-argumentos! p
                                   (map (lambda (e)
                                          (if (eq? e cursor) x e))
                                        (metainstrucción-argumentos p)))]))]
                     [else
                      (set! expresión x)])
               (set! cursor x)
               (set! índice 0)
               (send editor refresh-now)))
      (atajo ("hueco-aquí" editor evento) "c:4"
             (let ([x (hueco)])
               (cond [(recorre expresión)
                      => (lambda (p)
                           (cond [(abstracción? p)
                                  (set-abstracción-cuerpo! p x)]
                                 [(aplicación? p)
                                  (if (eq? cursor (aplicación-operador p))
                                      (set-aplicación-operador! p x)
                                      (set-aplicación-operando! p x))]
                                 [(metainstrucción? p)
                                  (set-metainstrucción-argumentos! p
                                   (map (lambda (e)
                                          (if (eq? e cursor) x e))
                                        (metainstrucción-argumentos p)))]))]
                     [else
                      (set! expresión x)])
               (set! cursor x)
               (set! índice 0)
               (send editor refresh-now)))
      (let ([metainstrucción-aquí-n
             (lambda (n)
               (lambda (editor evento)
                 (let ([x (metainstrucción "f" (build-list n (lambda (i)
                                                               (variable "x"))))])
                   (cond [(recorre expresión)
                          => (lambda (p)
                               (cond [(abstracción? p)
                                      (set-abstracción-cuerpo! p x)]
                                     [(aplicación? p)
                                      (if (eq? cursor (aplicación-operador p))
                                          (set-aplicación-operador! p x)
                                          (set-aplicación-operando! p x))]
                                     [(metainstrucción? p) p
                                      (set-metainstrucción-argumentos! p
                                       (map (lambda (e)
                                              (if (eq? e cursor) x e))
                                            (metainstrucción-argumentos p)))]))]
                         [else
                          (set! expresión x)])
                   (set! cursor x)
                   (set! índice 0)
                   (send editor refresh-now))))])
        (for-each (lambda (i)
                    (send atajos add-function (format "metainstrucción-aquí-~a" i)
                          (metainstrucción-aquí-n i))
                    (send atajos map-function
                          (format "c:5;~a" i)
                          (format "metainstrucción-aquí-~a" i)))
                  (range 0 10))))
    (atajo ("índice-derecha" editor evento) "right"
           (cond [(variable? cursor)
                  (when (< índice (string-length (variable-nombre cursor)))
                    (set! índice (+ índice 1))
                    (send editor refresh-now))]
                 [(metainstrucción? cursor)
                  (when (< índice (string-length (metainstrucción-nombre cursor)))
                    (set! índice (+ índice 1))
                    (send editor refresh-now))]))
    (atajo ("índice-izquierda" editor evento) "left"
           (cond [(or (variable? cursor) (metainstrucción? cursor))
                  (when (> índice 0)
                    (set! índice (- índice 1))
                    (send editor refresh-now))]))
    (for-each (lambda (normal griego)
                (atajo ((format "inserta-~a" griego) editor evento)
                       (format "m:~a" normal)
                       (send editor inserta griego)))
              (string->list "abcdefghijkmnoprstuvwxyzCDFGJLPSUVW")
              '(#\u3b1 #\u3b2 #\u3c8 #\u3b4 #\u3b5 #\u3c6 #\u3b3 #\u3b7 #\u3b9 #\u3be
                       #\u3ba  #\u3bc #\u3bd #\u3bf #\u3c0 #\u3c1 #\u3c3 #\u3c4
                       #\u3b8 #\u3c9 #\u3c2 #\u3c7 #\u3c5 #\u3b6 #\u3a8 #\u394 #\u3a6
                       #\u393 #\u39e #\u39b #\u3a0 #\u3a3 #\u398 #\u3a9 #\u3a3))
    (atajo ("eliminar-texto" editor evento) "backspace"
           (send editor elimina))
    (atajo ("enviar-expresión" editor evento) "c:enter"
           (enviar-expresión (send editor obtener-expresión)))))
(define expresión-snip%
  (class editor-snip%
    (init-field formal
                breve)
    (super-new)
    (define actual breve)
    (define editor (new text%))
    (send this set-editor editor)
    (refrescar)
    (define/override (on-event dc x y ex ey evento)
      (case (send evento get-event-type)
        [(left-down)
         (enviar-texto actual)
         (enfocar-editor)]
        [(right-down)
         (if (eq? actual breve)
             (set! actual formal)
             (set! actual breve))
         (refrescar)
         (enfocar-editor)]
        [else
         (enfocar-editor)]))
    (define/public (refrescar)
      (send editor erase)
      (send editor insert actual))))

(define evaluación%
  (class object%
    (init-field entrada)
    (super-new)
    (define entrada-formal (escribir-expresión-formal entrada))
    (define entrada-breve (escribir-expresión-breve entrada))
    (define salida
      (with-handlers ([exn:fail? (lambda (err)
                                   (cons 'error (exn-message err)))])
        (evaluar-expresión entrada)))
    (define salida-formal
      (with-handlers ([exn:fail? (lambda (err)
                                   (format "~v" salida))])
        (if (and (pair? salida) (eq? 'error (car salida)))
            (cdr salida)
            (escribir-expresión-formal salida))))
    (define salida-breve
      (with-handlers ([exn:fail? (lambda (err)
                                   (format "~v" salida))])
        (if (and (pair? salida) (eq? 'error (car salida)))
            (cdr salida)
            (escribir-expresión-breve salida))))
    (define/public (obtener-entrada)
      entrada)
    (define/public (obtener-entrada-formal)
      entrada-formal)
    (define/public (obtener-entrada-breve)
      entrada-breve)
    (define/public (obtener-entrada-snip)
      (new expresión-snip% [formal entrada-formal] [breve entrada-breve]))
    (define/public (obtener-salida)
      salida)
    (define/public (obtener-salida-formal)
      salida-formal)
    (define/public (obtener-salida-breve)
      salida-breve)
    (define/public (obtener-salida-snip)
      (new expresión-snip% [formal salida-formal] [breve salida-breve]))))
    

(define historial%
  (class text%
    (super-new)
    (define evaluaciones null)
    (define bloqueado? #t)
    (define/augment (can-insert? s l) (not bloqueado?))
    (define/augment (can-delete? s l) #f)
    (define/public (insertar evaluación)
      (define (al-final x)
        (send this insert x (send this last-position)))
      (set! evaluaciones (cons evaluación evaluaciones))
      (set! bloqueado? #f)
      (al-final #\newline)
      (al-final (send evaluación obtener-entrada-snip))
      (al-final (new pict-snip% [pict (arrow 10 0)]))
      (al-final (send evaluación obtener-salida-snip))
      (al-final #\newline)
      (set! bloqueado? #t))
    (send this hide-caret #t)))
(define (expresión->código expresión [columnas 50])
  (codeblock-pict
   #:keep-lang-line? #f
   (string-append "#lang racket\n"
                  (pretty-format expresión columnas))))

(define (expresión->figura-escrita-formal expresión)
  (define (e-variable e nombre)
    (colorize (text nombre) (dark "green")))
  (define (e-abstracción e argumento cuerpo)
    (hbl-append (colorize (text (string-append "(" (string #\u3BB))) (dark "brown"))
                (escribir-dibujo-formal argumento)
                (colorize (text ".") (dark "brown"))
                (escribir-dibujo-formal cuerpo)
                (colorize (text ")") (dark "brown"))))
  (define (e-aplicación e operador operando)
    (hbl-append (colorize (text "(") (dark "brown"))
                (escribir-dibujo-formal operador)
                (text " ")
                (escribir-dibujo-formal operando)
                (colorize (text ")") (dark "brown"))))
  (define (e-hueco e)
    (colorize (text "[ ]") (dark "brown")))
  (define (e-metainstrucción e nombre argumentos)
    (apply hbl-append
           (colorize (text nombre) (dark "green"))
           (colorize (text "[") (dark "brown"))
           (append (add-between (map escribir-dibujo-formal argumentos)
                                (colorize (text ",") (dark "brown")))
                   (list (colorize (text "]") (dark "brown"))))))
  (define escribir-dibujo-formal
    (escritor-formal e-variable e-abstracción e-aplicación
                     e-hueco e-metainstrucción))
  (if (null? expresión)
      (blank 50)
      (escribir-dibujo-formal expresión)))

(define (expresión->figura-escrita-breve expresión)
  (define (e-variable e nombre)
    (colorize (text nombre) (dark "green")))
  (define (e-abstracción e argumentos cuerpo)
    (apply hbl-append
           (colorize (text (string #\u3BB)) (dark "brown"))
           (append (add-between (map escribir-dibujo-breve argumentos)
                                (text " "))
                   (list (colorize (text ".") (dark "brown"))
                         (escribir-dibujo-breve cuerpo)))))
  (define (e-aplicación e operandos paréntesis)
    (apply hbl-append
           (add-between (map (lambda (x p?)
                               (if p?
                                   (hbl-append (colorize (text "(") (dark "brown"))
                                               (escribir-dibujo-breve x)
                                               (colorize (text ")") (dark "brown")))
                                   (escribir-dibujo-breve x)))
                             operandos
                             paréntesis)
                        (text " "))))
  (define (e-hueco e)
    (colorize (text "[ ]") (dark "brown")))
  (define (e-metainstrucción e nombre argumentos)
    (apply hbl-append
           (colorize (text nombre) (dark "green"))
           (colorize (text "[") (dark "brown"))
           (append (add-between (map escribir-dibujo-breve argumentos)
                                (colorize (text ",") (dark "brown")))
                   (list (colorize (text "]") (dark "brown"))))))
  (define escribir-dibujo-breve
    (escritor-breve e-variable e-abstracción e-aplicación
                    e-hueco e-metainstrucción))
  (if (null? expresión)
      (blank 50)
      (escribir-dibujo-breve expresión)))

(define código-lienzo%
  (class canvas%
    (inherit refresh refresh-now get-dc get-width get-height)
    (super-new)
    (send this init-auto-scrollbars 1000 1000 0.0 0.0)
    (define expresión null)
    (define expresión-pict (blank 50))
    (define/override (on-paint)
      (define dc (get-dc))
      (draw-pict expresión-pict dc 0 0))
    (define/public (actualizar-expresión e)
      (set! expresión e)
      (set! expresión-pict
            (scale (expresión->código e (inexact->exact
                                         (floor (/ (get-width) 15))))
                   2))
      (send this refresh-now))))

(define figura-escrita-formal-lienzo%
  (class canvas%
    (inherit refresh refresh-now get-dc get-width get-height)
    (super-new)
    (define expresión null)
    (define expresión-pict (expresión->figura-escrita-formal expresión))
    (define/override (on-paint)
      (define dc (get-dc))
      (define lienzo-ancho (get-width))
      (define lienzo-alto (get-height))
      (define dibujo (escalar-dibujo expresión-pict
                                     lienzo-ancho lienzo-alto))
      (define dibujo-ancho (pict-width dibujo))
      (define dibujo-alto (pict-height dibujo))
      (draw-pict dibujo dc
                 (- (/ lienzo-ancho 2) (/ dibujo-ancho 2))
                 (- (/ lienzo-alto 2) (/ dibujo-alto 2))))
    (define/public (actualizar-expresión e)
      (set! expresión e)
      (set! expresión-pict
            (expresión->figura-escrita-formal e))
      (send this refresh-now))))

(define figura-escrita-breve-lienzo%
  (class canvas%
    (inherit refresh refresh-now get-dc get-width get-height)
    (super-new)
    (define expresión null)
    (define expresión-pict (expresión->figura-escrita-breve expresión))
    (define/override (on-paint)
      (define dc (get-dc))
      (define lienzo-ancho (get-width))
      (define lienzo-alto (get-height))
      (define dibujo (escalar-dibujo expresión-pict
                                     lienzo-ancho lienzo-alto))
      (define dibujo-ancho (pict-width dibujo))
      (define dibujo-alto (pict-height dibujo))
      (draw-pict dibujo dc
                 (- (/ lienzo-ancho 2) (/ dibujo-ancho 2))
                 (- (/ lienzo-alto 2) (/ dibujo-alto 2))))
    (define/public (actualizar-expresión e)
      (set! expresión e)
      (set! expresión-pict (expresión->figura-escrita-breve e))
      (send this refresh-now))))
(pretty-print-current-style-table
 (pretty-print-extend-style-table (pretty-print-current-style-table)
                                  '(define-metainstrucción match)
                                  '(define case)))

(pretty-print-columns 80)

(define metainstrucciones-código (make-hash))

(define-syntax define-metainstrucción
  (syntax-rules ()
    [(define-metainstrucción (id . args) cuerpo ...)
     (begin (define (id . args) cuerpo ...)
            (hash-set! metainstrucciones
                       (symbol->string 'id)
                       id)
            (hash-set! metainstrucciones-código
                       (symbol->string 'id)
                       '(define-metainstrucción (id . args) cuerpo ...)))]))

(define (cargar-archivo-metainstrucciones archivo)
  (let ([file (open-input-file archivo)])
    (let loop ([e (read file)])
      (unless (eof-object? e)
        (eval e)
        (loop (read file)))))
  (reportar-cambio-registro))

(define código-racket%
  (class racket:text%
    (super-new)
    (define bloqueado? #t)
    (define/augment (can-insert? s l) (not bloqueado?))
    (define/augment (can-delete? s l) #t)
    (define/public (insertar sexp)
      (set! bloqueado? #f)
      (send this erase)
      (let ([p (open-output-string)])
        (pretty-print sexp p 1)
        (let ([cadena (get-output-string p)])
          (for-each (lambda (x)
                      (send this insert x)
                      (send this insert-return))
                    (map string-trim (string-split cadena "\n"))))
      (set! bloqueado? #t)))))
;; Ventana registro

(define ventana-registro
  (new frame%
       [label "Registro Metainstrucciones"]
       [width 800]
       [height 600]))

(define registro-panel
  (new panel:horizontal-dragable%
       [parent ventana-registro]
       [border 0]
       [spacing 20]))

(define listado-panel
  (new vertical-panel%
       [parent registro-panel]
       [spacing 20]))

(define listado-metainstrucciones
  (new list-box%
       [parent listado-panel]
       [label #f]
       [choices (list "")]
       [style (list 'single
                    'column-headers)]
       [columns (list "Nombre")]
       [callback
        (lambda (listado evento)
          (when (eq? 'list-box (send evento get-event-type))
            (let [(seleccionados (send listado get-selections))]
              (unless (null? seleccionados)
                (send código-texto insertar
                      (send listado get-data (car seleccionados)))))))]))

(define botón-cargar
  (new button%
       [label "Cargar archivo"]
       [parent listado-panel]
       [stretchable-width #t]
       [callback
        (lambda (botón evento)
          (when (eq? 'button (send evento get-event-type))
            (let ([p (get-file "Cargar archivo de metainstrucciones")])
              (when p
                (cargar-archivo-metainstrucciones (path->string p))))))]))

(define código-texto (new código-racket%))

(define lienzo-implementación (new canvas:color%
                                   [parent registro-panel]
                                   [editor código-texto]))

(define ventana-visualizador
  (new frame%
       [label "Visualizador"]
       [width 800]  [min-width 200]
       [height 600] [min-height 200]))

(define visualizador-panel
  (new vertical-panel%
       [parent ventana-visualizador]
       [border 0]
       [spacing 20]))

(define visualizador-código
  (new código-lienzo%
       [parent visualizador-panel]
       [style (list 'vscroll 'hscroll)]))

(define visualizador-formal
  (new figura-escrita-formal-lienzo%
       [parent visualizador-panel]
       [stretchable-height #f]
       [min-height 50]))

(define visualizador-breve
  (new figura-escrita-breve-lienzo%
       [parent visualizador-panel]
       [stretchable-height #f]
       [min-height 50]))

(define ventana-principal
  (new frame%
       [label "Lambda"]
       [width 800]  [min-width 200]
       [height 600] [min-height 200]))

(define panel-principal
  (new vertical-panel%
       [parent ventana-principal]))

(define panel-herramientas
  (new horizontal-panel%
       [parent panel-principal]
       [border 0]
       [stretchable-height #f]))

(define botón-visualizador
  (new button%
       [parent panel-herramientas]
       [label "visualizador"]
       [callback
        (lambda (botón evento)
          (send ventana-visualizador show #t))]))

(define botón-metainstrucciones
  (new button%
       [parent panel-herramientas]
       [label "metainstrucciones"]
       [callback
        (lambda (botón evento)
          (send ventana-registro show #t))]))

(define herramientas-espacio
  (new panel%
       [parent panel-herramientas]))

(define botón-ayuda
  (new button%
       [parent panel-herramientas]
       [label "ayuda"]))

(define panel-historial/editor
  (new panel:vertical-dragable%
       [parent panel-principal]))

(define historial-panel
  (new panel%
       [parent panel-historial/editor]
       [border 0]))

(define historial (new historial%))

(define historial-lienzo
  (new editor-canvas%
       [parent historial-panel]
       [editor historial]))

(define panel-tabs
  (new tab-panel%
       [parent panel-historial/editor]
       [choices (list "Texto" "Estructura")]
       [callback
        (lambda (panel evento)
          (case (send panel get-selection)
            [(0)
             (send panel change-children
                   (lambda (x)
                     (list editor-texto-panel)))
             (reportar-otro-editor 'Texto)]
            [(1)
             (send panel change-children
                   (lambda (x)
                     (list editor-estructural-panel)))
             (reportar-otro-editor 'Estructura)]))]))

(define editor-texto-panel
  (new panel%
       [parent panel-tabs]
       [border 0]))

(define editor-texto (new editor-texto%))

(define editor-texto-lienzo
  (new editor-canvas%
       [parent editor-texto-panel]
       [editor editor-texto]))

(define editor-estructural-panel
  (new panel%
       [parent panel-tabs]
       [border 0]))

(define editor-estructural
  (new editor-estructural%
       [parent editor-estructural-panel]))

(send panel-tabs change-children
      (lambda (x)
        (list editor-texto-panel)))

;;
;; Procedimientos conectivos
;; 

(define (enviar-expresión expresión)
  (send historial insertar
        (new evaluación% [entrada expresión]))
  (case editor-actual
    [(Texto)
     (send editor-texto erase)]
    [(Estructura)
     (send editor-estructural borrar)]))

(define (reportar-cambio-editor expresión)
  (send visualizador-código actualizar-expresión expresión)
  (send visualizador-formal actualizar-expresión expresión)
  (send visualizador-breve actualizar-expresión expresión))

(define (enfocar-editor)
  (case editor-actual
    [(Texto)
     (send editor-texto-lienzo focus)]
    [(Estructura)
     (send editor-estructural focus)]))

(define (enviar-texto cadena)
  (case editor-actual
    [(Texto)
     (send editor-texto erase)
     (send editor-texto insert cadena)]
    [(Estructura)
     (send editor-estructural borrar)
     (send editor-estructural cambiar-expresión
           (parsear-cadena cadena))]))

(define editor-actual 'Texto)

(define (reportar-otro-editor objetivo)
  (unless (eq? editor-actual objetivo)
    (case editor-actual
      [(Texto)
       (send editor-estructural cambiar-expresión
             (send editor-texto obtener-expresión))]
      [(Estructura)
       (send editor-texto cambiar-expresión
             (send editor-estructural obtener-expresión))]))
  (set! editor-actual objetivo)
  (enfocar-editor))

(define (reportar-cambio-registro)
  (hash-for-each metainstrucciones
                 (lambda (nombre proc)
                   (send listado-metainstrucciones append
                         nombre (hash-ref metainstrucciones-código nombre)))
                 #t))

;;
;; Mostrar la ventana principal
;;

(send ventana-principal show #t)

