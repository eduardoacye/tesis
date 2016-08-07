;; -*- mode: racket; coding: utf-8 -*-

;;; lector.rkt --- 

;; Copyright (C) 2016 Eduardo Acuña Yeomans <eduardo.acye@gmail.com>

;; Author: Eduardo Acuña Yeomans <eduardo.acye@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Este código es parte de mi tesis de licenciatura.
;;

;;; Code:

#lang racket/base

(require racket/match
         "estructuras.rkt")

(provide λ-leer)

(define (λ-leer [p (current-input-port)])
  (let ([e (leer-expresión p)])
    (cond [(and (char? e) (char=? #\; e)) (λ-leer p)]
          [(not (char? e)) e]
          [else (error 'λ-leer "Se esperaba leer una expresión completa pero se leyó ~a" e)])))

(define (leer-expresión [p (current-input-port)]
                        [c (peek-char p)]
                        [a null])
  (cond [(eof-object? c)        (if (null? a) c a)]
        [(char=? #\; c)         (if (null? a) (read-char p) a)]
        [(delimitador-final? c) (if (null? a) (read-char p) a)]
        [(char-whitespace? c)
         (read-char p)
         (leer-expresión p (peek-char p) a)]
        [(char=? #\( c)
         (read-char p)
         (let ([e (leer-paréntesis p)])
           (leer-expresión p (peek-char p) (aplicación/identidad a e)))]
        [(or (char=? #\λ c) (char=? #\\ c))
         (read-char p)
         (let ([e (leer-abstracción p)])
           (leer-expresión p (peek-char p) (aplicación/identidad a e)))]
        [else
         ((lambda (e)
            (leer-expresión p (peek-char p) (aplicación/identidad a e)))
          (let* ([x (leer-constituyente p c)]
                 [c (peek-char p)])
            (cond [(and (char? c) (char=? #\[ c))
                   (read-char p)
                   (λ-metainstrucción x (leer-corchetes p))]
                  [else
                   (λ-variable x)])))]))

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
      (char=? #\λ c)
      (char=? #\\ c)))

(define (delimitador? c)
  (or (delimitador-final? c)
      (delimitador-inicial? c)
      (char-whitespace? c)))

(define (aplicación/identidad a e)
  (if (null? a) e (λ-aplicación a e)))

(define (leer-paréntesis [p (current-input-port)]
                         [c (peek-char p)])
  (let ([e (leer-expresión p c)])
    (unless (or (λ-abstracción? e) (λ-aplicación? e))
      (error 'leer-paréntesis "Se esperaba leer una abstracción o una aplicación pero se leyó ~a" e))
    (let ([c (read-char p)])
      (unless (char=? #\) c)
        (error 'leer-paréntesis "Se esperaba leer `)' pero se leyó `~a'" c))
      e)))

(define (leer-corchetes [p (current-input-port)]
                        [c (peek-char p)])
  (let itera ([a null]
              [e (leer-expresión p c)])
    (cond [(eof-object? e)
           (error 'leer-corchetes "Se esperaba leer `]' o `,' pero se leyó ~a" e)]
          [(char? e)
           (case e
             [(#\]) (reverse a)]
             [(#\,) (itera a (leer-expresión p))]
             [else
              (error 'leer-corchetes "Se esperaba leer `]' o `,' pero se leyó ~a" e)])]
          [else
           (itera (cons e a) (leer-expresión p))])))

(define (leer-abstracción [p (current-input-port)]
                          [c (peek-char p)])
  (let itera ([c c]
              [a null])
    (cond [(or (eof-object? c)
               (and (delimitador? c) (not (char-whitespace? c)) (not (char=? #\. c))))
           (error 'leer-abstracción "Se esperaba leer una variable pero se leyó `~a'" c)]
          [(char-whitespace? c)
           (read-char p)
           (itera (peek-char p) a)]
          [(char=? #\. c)
           (read-char p)
           (if (null? a)
               (error 'leer-abstracción "Se esperaba leer una variable pero se leyó `~a'" c)
               (let ([e (leer-expresión p)])
                 (if (λ-expresión? e)
                     (foldr λ-abstracción e (reverse a))
                     (error 'leer-abstracción "Se esperaba leer una expresión pero se leyó ~a" e))))]
          [else
           (let ([v (λ-variable (leer-constituyente p))])
             (itera (peek-char p) (cons v a)))])))

(define (leer-constituyente [p (current-input-port)]
                            [c (peek-char p)])
  (let itera ([a null]
              [c c])
    (if (delimitador? c)
        (if (null? a)
            (error 'leer-constituyente "Se esperaba leer un identificador pero se leyó `~a'" c)
            (list->string (reverse a)))
        (itera (cons (read-char p) a)
               (peek-char p)))))
