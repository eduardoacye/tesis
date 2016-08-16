;;; -*- mode: racket; coding: utf-8 -*-
;;; Copyright (C) 2016 Eduardo Acuña Yeomans <eduardo.acye@gmail.com>
;;; Procura no editar este archivo, mejor modifica lambda.nw

#lang racket/base

(require "estructuras.rkt"
         racket/list
         racket/string
         (for-syntax racket/base))

(provide expresión->texto-plano
         expresión->abuso-texto-plano
         plantilla-latex
         expresión->latex
         expresión->abuso-latex)

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
      (format "~a[~a]"
                  nombre
                  (string-join (map expresión->texto-plano argumentos) ", ")))]
   [else
    (error 'expresión->texto-plano
           "La expresión ~a no es válida" e)]))

(define (variable->texto-plano e)
  (variable-nombre e))


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
  (let itera ([cuerpo (abstracción-cuerpo e)]
              [resultado (format "~a~a"
                                 (integer->char 955)
                                 (variable->abuso-texto-plano (abstracción-argumento e)))])
    (if (not (abstracción? cuerpo))
        (format "~a.~a" resultado (expresión->abuso-texto-plano cuerpo))
        (itera (abstracción-cuerpo cuerpo)
               (format "~a ~a"
                       resultado
                       (variable->abuso-texto-plano (abstracción-argumento cuerpo)))))))

(define (aplicación->abuso-texto-plano e)
  (let itera ([operador (aplicación-operador e)]
              [resultado (format " ~a"
                                 (let ([e (aplicación-operando e)])
                                   (if (aplicación? e)
                                       (format "(~a)" (aplicación->abuso-texto-plano e))
                                       (expresión->abuso-texto-plano e))))])
    (if (not (aplicación? operador))
        (format "~a~a"
                (cond [(aplicación? operador)
                       (format "(~a)" (aplicación->abuso-texto-plano operador))]
                      [(abstracción? operador)
                       (format "(~a)" (abstracción->abuso-texto-plano operador))]
                      [else
                       (expresión->abuso-texto-plano operador)])
                resultado)
        (let ([e (aplicación-operando operador)])
          (itera (aplicación-operador operador)
                 (format " ~a~a"
                         (cond [(aplicación? e)
                                (format "(~a)" (aplicación->abuso-texto-plano e))]
                               [(abstracción? e)
                                (format "(~a)" (abstracción->abuso-texto-plano e))]
                               [else
                                (expresión->abuso-texto-plano e)])
                         resultado))))))

(define (hueco->abuso-texto-plano e)
  "[ ]")

(define (metainstrucción->abuso-texto-plano e)
  (let itera ([argumentos (metainstrucción-argumentos e)]
              [resultado (format "~a[" (metainstrucción-nombre e))])
    (cond [(null? argumentos)
           (format "~a]" resultado)]
          [(null? (cdr argumentos))
           (format "~a~a]"
                   resultado
                   (expresión->abuso-texto-plano (car argumentos)))]
          [else
           (itera (cdr argumentos)
                  (format "~a~a, "
                          resultado
                          (expresión->abuso-texto-plano (car argumentos))))])))


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

(define (expresión->latex e)
  (cond
   [(variable? e)
    (variable->latex e)]
   [(abstracción? e)
    (abstracción->latex e)]
   [(aplicación? e)
    (aplicación->latex e)]
   [(hueco? e)
    (hueco->latex e)]
   [(metainstrucción? e)
    (metainstrucción->latex e)]
   [else
    (error 'expresión->latex
           "La expresión ~a no es válida" e)]))

(define (variable->latex e)
  (variable-nombre e))

(define (abstracción->latex e)
  (format "(\\lambda ~a . ~a)"
          (variable->latex (abstracción-argumento e))
          (expresión->latex (abstracción-cuerpo e))))

(define (aplicación->latex e)
  (format "(~a\\, ~a)"
          (expresión->latex (aplicación-operador e))
          (expresión->latex (aplicación-operando e))))

(define (hueco->latex e)
  "[\\quad ]")

(define (metainstrucción->latex e)
  (let ([nombre     (metainstrucción-nombre e)]
        [argumentos (metainstrucción-argumentos e)])
    (format "~a[~a]"
            nombre
            (string-join (map expresión->latex argumentos) ",\\, "))))


;;; Traducir a LaTeX con abuso de notación
(define (expresión->abuso-latex e)
  (cond
   [(variable? e)
    (variable->abuso-latex e)]
   [(abstracción? e)
    (abstracción->abuso-latex e)]
   [(aplicación? e)
    (aplicación->abuso-latex e)]
   [(hueco? e)
    (hueco->abuso-latex e)]
   [(metainstrucción? e)
    (metainstrucción->abuso-latex e)]
   [else
    (error 'expresión->abuso-latex
           "La expresión ~a no es válida" e)]))

(define (variable->abuso-latex e)
  (variable->texto-plano e))

(define (abstracción->abuso-latex e)
  (let itera ([cuerpo (abstracción-cuerpo e)]
              [resultado (format "\\lambda ~a"
                                 (variable->abuso-latex (abstracción-argumento e)))])
    (if (not (abstracción? cuerpo))
        (format "~a . ~a" resultado (expresión->abuso-latex cuerpo))
        (itera (abstracción-cuerpo cuerpo)
               (format "~a\\, ~a"
                       resultado
                       (variable->abuso-latex (abstracción-argumento cuerpo)))))))

(define (aplicación->abuso-latex e)
  (let itera ([operador (aplicación-operador e)]
              [resultado (format "\\, ~a"
                                 (let ([e (aplicación-operando e)])
                                   (if (aplicación? e)
                                       (format "(~a)" (aplicación->abuso-latex e))
                                       (expresión->abuso-latex e))))])
    (if (not (aplicación? operador))
        (format "~a~a"
                (cond [(aplicación? operador)
                       (format "(~a)" (aplicación->abuso-latex operador))]
                      [(abstracción? operador)
                       (format "(~a)" (abstracción->abuso-latex operador))]
                      [else
                       (expresión->abuso-latex operador)])
                resultado)
        (let ([e (aplicación-operando operador)])
          (itera (aplicación-operador operador)
                 (format "\\, ~a~a"
                         (cond [(aplicación? e)
                                (format "(~a)" (aplicación->abuso-latex e))]
                               [(abstracción? e)
                                (format "(~a)" (abstracción->abuso-latex e))]
                               [else
                                (expresión->abuso-latex e)])
                         resultado))))))

(define (hueco->abuso-latex e)
  "[\\quad ]")

(define (metainstrucción->abuso-latex e)
  (let itera ([argumentos (metainstrucción-argumentos e)]
              [resultado (format "~a[" (metainstrucción-nombre e))])
    (cond [(null? argumentos)
           (format "~a]" resultado)]
          [(null? (cdr argumentos))
           (format "~a~a]"
                   resultado
                   (expresión->abuso-latex (car argumentos)))]
          [else
           (itera (cdr argumentos)
                  (format "~a~a,\\, "
                          resultado
                          (expresión->abuso-latex (car argumentos))))])))

