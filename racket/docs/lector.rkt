;;; -*- mode: racket; coding: utf-8 -*-
;;; Copyright (C) 2016 Eduardo Acuña Yeomans <eduardo.acye@gmail.com>
;;; Procura no editar este archivo, mejor modifica lambda.nw

#lang racket/base

;;; Importar estructuras del árbol de sintaxis
(require "estructuras.rkt")

;;; Exportar punto de entrada del parser
(provide leer)

;;; Procedimientos principales
(define (leer [p (current-input-port)])
  (let ([e (parsear-expresión p)])
    (cond
     [(and (char? e) (char=? #\; e))
      (leer p)]
     [(not (char? e))
      e]
     [else
      (error 'leer "Se esperaba una expresión completa pero se leyó `~a'" e)])))

(define (parsear-expresión [p (current-input-port)]
                           [c (peek-char p)]
                           [a null])
  (cond
   [(eof-object? c)
    (if (null? a) c a)]
   [(delimitador-final? c)
    (if (null? a) (read-char p) a)]
   [(char-whitespace? c)
    (read-char p)
    (parsear-expresión p (peek-char p) a)]
   [(char=? #\( c)
    (read-char p)
    (let ([e (parsear-paréntesis p)])
      (parsear-expresión p (peek-char p) (aplicación/identidad a e)))]
   [(or (char=? (integer->char 955) c)
        (char=? #\\ c))
    (read-char p)
    (let ([e (parsear-abstracción p)])
      (parsear-expresión p (peek-char p) (aplicación/identidad a e)))]
   [else
    ((lambda (e)
       (parsear-expresión p (peek-char p) (aplicación/identidad a e)))
     (let* ([x (parsear-constituyente p c)]
            [c (peek-char p)])
       (cond [(and (char? c) (char=? #\[ c))
              (read-char p)
              (metainstrucción x (parsear-corchetes p))]
             [else
              (variable x)])))]))


;;; Procedimientos auxiliares
(define (delimitador-final? c)
  (or (eof-object? c)
      (char=? #\] c)
      (char=? #\) c)
      (char=? #\. c)
      (char=? #\, c)
      (char=? #\; c)))

(define (delimitador-inicial? c)
  (or (char=? #\( c)
      (char=? #\[ c)
      (char=? (integer->char 955) c)
      (char=? #\\ c)))

(define (delimitador? c)
  (or (delimitador-final? c)
      (delimitador-inicial? c)
      (char-whitespace? c)))

(define (aplicación/identidad a e)
  (if (null? a) e (aplicación a e)))

(define (parsear-paréntesis [p (current-input-port)]
                            [c (peek-char p)])
  (let ([e (parsear-expresión p c)])
    (unless (or (abstracción? e) (aplicación? e))
      (error 'parsear-paréntesis "Se esperaba una abstracción o una aplicación pero se leyó ~a" e))
    (let ([c (read-char p)])
      (unless (char=? #\) c)
        (error 'parsear-paréntesis "Se esperaba `)' pero se leyó `~a'" c))
      e)))

(define (parsear-abstracción [p (current-input-port)]
                             [c (peek-char p)])
  (let itera ([c c]
              [a null])
    (cond [(or (eof-object? c)
               (and (delimitador? c) (not (char-whitespace? c)) (not (char=? #\. c))))
           (error 'parsear-abstracción "Se esperaba una variable pero se leyó `~a'" c)]
          [(char-whitespace? c)
           (read-char p)
           (itera (peek-char p) a)]
          [(char=? #\. c)
           (read-char p)
           (if (null? a)
               (error 'parsear-abstracción "Se esperaba una variable pero se leyó `~a'" c)
               (let ([e (parsear-expresión p)])
                 (if (expresión? e)
                     (foldr abstracción e (reverse a))
                     (error 'parsear-abstracción "Se esperaba una expresión pero se leyó ~a" e))))]
          [else
           (let ([v (variable (parsear-constituyente p))])
             (itera (peek-char p) (cons v a)))])))

(define (parsear-constituyente [p (current-input-port)]
                               [c (peek-char p)])
  (let itera ([a null]
              [c c])
    (if (delimitador? c)
        (if (null? a)
            (error 'parsear-constituyente "Se esperaba un identificador pero se leyó `~a'" c)
            (list->string (reverse a)))
        (itera (cons (read-char p) a)
               (peek-char p)))))

(define (parsear-corchetes [p (current-input-port)]
                           [c (peek-char p)])
  (let itera ([a null]
              [e (parsear-expresión p c)])
       (cond [(eof-object? e)
              (error 'parsear-corchetes "Se esperaba leer `]' o `,' pero se leyó ~a" e)]
             [(char? e)
              (case e
                [(#\]) (reverse a)]
                [(#\,) (itera a (parsear-expresión p))]
                [else
                 (error 'parsear-corchetes "Se esperaba leer `]' o `,' pero se leyó ~a" e)])]
             [else
              (itera (cons e a) (parsear-expresión p))])))

