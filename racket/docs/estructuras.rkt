;;; -*- mode: racket; coding: utf-8 -*-
;;; Copyright (C) 2016 Eduardo Acuña Yeomans <eduardo.acye@gmail.com>
;;; Procura no editar este archivo, mejor modifica lambda.nw

#lang racket/base

(require srfi/1)

(provide variable variable? variable-nombre
         abstracción abstracción? abstracción-argumento abstracción-cuerpo
         aplicación aplicación? aplicación-operador aplicación-operando
         metainstrucción metainstrucción? metainstrucción-nombre metainstrucción-argumentos
         hueco hueco?
         término? expresión?)

;;; Estructuras del árbol de sintaxis
(struct variable (nombre) #:transparent)

(struct abstracción (argumento cuerpo) #:transparent)

(struct aplicación (operador operando) #:transparent)

(struct metainstrucción (nombre argumentos) #:transparent)

(struct hueco () #:transparent)


;;; Predicados especiales
(define (término? x)
  (cond
   [(variable? x)    (string? (variable-nombre x))]
   [(abstracción? x) (and (variable? (abstracción-argumento x))
                          (término? (abstracción-cuerpo x)))]
   [(aplicación? x)  (and (término? (aplicación-operador x))
                          (término? (aplicación-operando x)))]
   [else             #f]))

(define (expresión? x)
  (cond
   [(variable? x)        (string? (variable-nombre x))]
   [(abstracción? x)     (and (variable? (abstracción-argumento x))
                              (expresión? (abstracción-cuerpo x)))]
   [(aplicación? x)      (and (expresión? (aplicación-operador x))
                              (expresión? (aplicación-operando x)))]
   [(metainstrucción? x) (and (string? (metainstrucción-nombre x))
                              (every expresión? (metainstrucción-argumentos x)))]
   [(hueco? x)           #t]
   [else                 #f]))

