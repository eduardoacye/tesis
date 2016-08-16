;;; -*- mode: racket; coding: utf-8 -*-

#lang racket/base

(require "estructuras.rkt"
         "lector.rkt"
         "escritor.rkt"
         "evaluador.rkt"
         racket/cmdline)

(define modo-escritura 'formal)

(define (repl)
  (parameterize ([current-prompt-read
                  (lambda ()
                    (display "λ> ")
                    (flush-output)
                    (let ([str (read-line)])
                      (cond [(eof-object? str)
                             (void)]
                            [(string=? str ",salir")
                             eof]
                            [(string=? str ",breve")
                             (set! modo-escritura 'breve)
                             'modo-escritura]
                            [(string=? str ",formal")
                             (set! modo-escritura 'formal)
                             'modo-escritura]
                            [else
                             (leer (open-input-string str))])))]
                 [current-eval
                  (lambda (e)
                    (printf "\n~a\n" (expresión? (cdr e)))
                    (if (expresión? e)
                        (evaluar-expresión (cdr e))
                        (cdr e)))]
                 [current-print
                  (lambda (e)
                    (if (expresión? e)
                        (if (eq? modo-escritura 'formal)
                            (printf "~a\n" (expresión->texto-plano e))
                            (printf "~a\n" (expresión->abuso-texto-plano e)))
                        (printf "~a\n" e)))])
    (display "El REPL está listo, presiona alguna tecla para iniciar...")
    (flush-output)
    (read-line)
    (read-eval-print-loop)))

(define argumentos-consola
  (command-line
   #:program "REPL"))

(repl)
