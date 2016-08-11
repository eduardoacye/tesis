;;; -*- mode: racket; coding: utf-8 -*-
;;; Copyright (C) 2016 Eduardo Acuña Yeomans <eduardo.acye@gmail.com>
;;; Procura no editar este archivo, mejor modifica lambda.nw

#lang racket/base

(require "estructuras.rkt"
         racket/list
         racket/string)

(provide expresión->texto-plano
         expresión->abuso-texto-plano
         plantilla-latex)

;;; Traducir a texto plano sin abuso de notación
(define (expresión->texto-plano e)
  (cond
   [(variable? e)
    (variable->texto-plano e)]
   [(abstracción? e)
    (format "(~a~a.~a)"
            (integer->char 955)
            (variable->texto-plano (abstracción-argumento e))
            (expresión->texto-plano (abstracción-cuerpo e)))]
   [(aplicación? e)
    (format "(~a ~a)"
            (expresión->texto-plano (aplicación-operador e))
            (expresión->texto-plano (aplicación-operando e)))]
   [(hueco? e)
    "[ ]"]
   [(metainstrucción? e)
    (let ([nombre     (metainstrucción-nombre     e)]
          [argumentos (metainstrucción-argumentos e)])
      (if (and (string? nombre) (list? argumentos))
          (format "~a[~a]"
                  nombre
                  (string-join (map expresión->texto-plano argumentos) ", "))
          (error 'metainstrucción->texto-plano
                 "La metainstrucción ~a está mal formada" e)))]
   [else
    (error 'expresión->texto-plano
           "La expresión ~a no es válida" e)]))

(define (variable->texto-plano e)
  (if (variable? e)
      (variable-nombre e)
      (error 'variable->texto-plano
             "La expresión ~a no es una variable" e)))


;;; Traducir a texto plano con abuso de notación
(define (expresión->abuso-texto-plano e)
  (cond
   [(variable? e)
    (variable->abuso-texto-plano e)]
   [(abstracción? e)
    (abstracción->abuso-texto-plano e)]
   [(aplicación? e)
    (aplicación->abuso-texto-plano e)]
   [(hueco? e)
    (hueco->abuso-texto-plano e)]
   [(metainstrucción? e)
    (metainstrucción->abuso-texto-plano e)]
   [else
    (error 'expresión->abuso-texto-plano
           "La expresión ~a no es válida" e)]))

(define (variable->abuso-texto-plano e)
  (variable->texto-plano e))

(define (abstracción->abuso-texto-plano e)
  (let itera ([argumentos (list (abstracción-argumento e))]
              [cuerpo     (abstracción-cuerpo e)])
     (if (abstracción? cuerpo)
         (itera (cons (abstracción-argumento cuerpo) argumentos)
                (abstracción-cuerpo cuerpo))
         (format "~a~a.~a"
                 (integer->char 955)
                 (string-join (map variable->abuso-texto-plano
                                   (reverse argumentos))
                              " ")
                 (expresión->abuso-texto-plano cuerpo)))))

(define (aplicación->abuso-texto-plano e)
  (let itera ([operador    (aplicación-operador e)]
              [expresiones (list (aplicación-operando e))])
    (if (aplicación? operador)
        (itera (aplicación-operador operador)
               (cons (aplicación-operando operador) expresiones))
        (let itera ([expresiones  (cons operador expresiones)]
                    [traducciones null])
          (if (null? expresiones)
              (string-join (reverse traducciones) " ")
              (let ([e  (first expresiones)]
                    [es (rest  expresiones)])
                (cond
                 [(aplicación? e)
                  (itera es (cons (format "(~a)" (aplicación->abuso-texto-plano e))
                                  traducciones))]
                 [(and (abstracción? e) (not (null? es)))
                  (itera es (cons (format "(~a)" (abstracción->abuso-texto-plano e))
                                  traducciones))]
                 [else
                  (itera es (cons (expresión->abuso-texto-plano e)
                                  traducciones))])))))))

(define (hueco->abuso-texto-plano e)
  "[ ]")

(define (metainstrucción->abuso-texto-plano e)
  (let ([nombre     (metainstrucción-nombre e)]
        [argumentos (metainstrucción-argumentos e)])
    (if (and (string? nombre) (list? argumentos))
        (format "~a[~a]"
                nombre
                (string-join (map expresión->abuso-texto-plano argumentos) ", "))
        (error 'metainstrucción->abuso-text-plano
               "La metainstrucción ~a está mal formada" e))))


;;; Traducir a LaTeX sin abuso de notación
(define +plantilla-latex-inicial+
  "\
\\documentclass[preview]{standalone}\n\
\\usepackage{amsmath}\n\
\\begin{document}\n\
\\( ~a \\)\n\
\\end{document}")

(define plantilla-latex
  (make-parameter
   +plantilla-latex-inicial+
   (lambda (x)
     (if (and (string? x) (= (length (string-split x "~a" #:trim? #f)) 2))
         x
         +plantilla-latex-inicial+))))

