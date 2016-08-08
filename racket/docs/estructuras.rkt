;;; -*- mode: racket; coding: utf-8 -*-
;;; Copyright (C) 2016 Eduardo Acuña Yeomans <eduardo.acye@gmail.com>
;;; Procura no editar este archivo, mejor modifica lambda.nw

#lang racket/base

;;; Identificadores exportados
(provide variable variable? variable-nombre
         abstracción abstracción? abstracción-argumento abstracción-cuerpo
         aplicación aplicación? aplicación-operador aplicación-operando
         metainstrucción metainstrucción? metainstrucción-nombre metainstrucción-argumentos
         término? expresión?)

;;; Estructuras del árbol de sintaxis
(struct variable (nombre) #:transparent)
(struct abstracción (argumento cuerpo) #:transparent)
(struct aplicación (operador operando) #:transparent)
(struct metainstrucción (nombre argumentos) #:transparent)

;;; Predicados especiales
(define (término? x)
  (cond
   [(variable? x) #t]
   [(abstracción? x)
    (and (variable? (abstracción-argumento x))
         (término? (abstracción-cuerpo x)))]
   [(aplicación? x)
    (and (término? (aplicación-operador x))
         (término? (aplicación-operando x)))]
   [else #f]))
(define (expresión? x)
  (or (término? x)
      (metainstrucción? x)))
