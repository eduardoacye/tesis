;; -*- mode: racket; coding: utf-8 -*-

#lang racket

(require racket/gui/base
         pict
         pict/tree-layout
         "../docs/estructuras.rkt")

(struct hoyo () #:transparent)

(define *expresión-actual*
  (abstracción
   (variable "x")
   (aplicación
    (abstracción (variable "y")
                 (variable "y"))
    (hoyo))))

(define *cursor* *expresión-actual*)

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

(define (expresión->árbol e)
  (cond [(variable? e) (variable->árbol e)]
        [(abstracción? e) (abstracción->árbol e)]
        [(aplicación? e) (aplicación->árbol e)]
        [(hoyo? e) (hoyo->árbol e)]
        [else (error 'expresión->árbol "El objeto ~a no es una expresión lambda" e)]))

(define (variable->árbol e)
  (tree-layout #:pict (enmarcar (text (variable-nombre e))
                                #:borde? #f
                                #:color (if (eq? *cursor* e) "yellow" "white"))))

(define (abstracción->árbol e)
  (tree-layout #:pict (enmarcar (text (string-append "λ " (variable-nombre (abstracción-argumento e))))
                                #:borde? #t
                                #:color (if (eq? *cursor* e) "yellow" "white"))
               (tree-edge #:edge-width 2
                          (expresión->árbol (abstracción-cuerpo e)))))

(define (aplicación->árbol e)
  (tree-layout #:pict (filled-ellipse 10 10 #:draw-border? #t
                                      #:color (if (eq? *cursor* e) "yellow" "white")
                                      #:border-width 1)
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

(define panel (new vertical-panel%
                   [parent ventana]
                   [border 5] [spacing 5]))

(define lienzo-editor%
  (class canvas%
    (inherit refresh-now get-dc get-width get-height)
    (define/override (on-char evento)
      (gestionar-comando-teclas (send evento get-key-code)
                                (send evento get-control-down)
                                (send evento get-meta-down))
      (refresh-now))
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

(agregar-comando-teclas
 '(up)
 (lambda () (set! *cursor* (cursor-arriba))))

(agregar-comando-teclas
 '(down)
 (lambda () (set! *cursor* (cursor-abajo))))

(agregar-comando-teclas
 '(left)
 (lambda () (set! *cursor* (cursor-izquierda))))

(agregar-comando-teclas
 '(right)
 (lambda () (set! *cursor* (cursor-derecha))))

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

(send editor focus)
(send ventana show #t)

;; (get-text-from-user "fuck" "bla bla bla" #f "" '(disallow-invalid) #:validate (lambda (str) (string=? str "flow")))
