;;; -*- mode: racket; coding: utf-8 -*-

#lang racket

(require (for-syntax racket/syntax))

(struct variable (nombre) #:transparent)
(struct abstracción (argumento cuerpo) #:transparent)
(struct aplicación (operador operando) #:transparent)
(struct metainstrucción (nombre argumentos) #:transparent)
(struct hueco () #:transparent)

(define (crea-escritor-formal identificador
                             #:en-nombre [en-nombre (lambda (e x) x)]
                             #:en-paréntesis-abierto [en-paréntesis-abierto (lambda (e) "(")]
                             #:en-paréntesis-cerrado [en-paréntesis-cerrado (lambda (e) ")")]
                             #:en-corchete-abierto [en-corchete-abierto (lambda (e) "[")]
                             #:en-corchete-cerrado [en-corchete-cerrado (lambda (e) "]")]
                             #:en-lambda [en-lambda (lambda (e) (string (integer->char 955)))]
                             #:en-punto [en-punto (lambda (e) ".")]
                             #:en-coma [en-coma (lambda (e) ",")]
                             #:en-espacio [en-espacio (lambda (e) " ")]
                             #:concatena [concatena string-append])
  (define (escribe-variable e)
    (en-nombre e (variable-nombre e)))
  (define (escribe-abstracción e)
    (concatena (en-paréntesis-abierto e)
               (en-lambda e)
               (escribe-variable (abstracción-argumento e))
               (en-punto e)
               (escribe-expresión (abstracción-cuerpo e))))
  (define (escribe-aplicación e)
    (concatena (en-paréntesis-abierto e)
               (escribe-expresión (aplicación-operador e))
               (en-espacio e)
               (escribe-expresión (aplicación-operando e))
               (en-paréntesis-cerrado e)))
  (define (escribe-hueco e)
    (concatena (en-corchete-abierto e)
               (en-espacio e)
               (en-corchete-cerrado e)))
  (define (escribe-metainstrucción e)
    (let itera ([argumentos (metainstrucción-argumentos e)]
                [resultado (concatena (en-nombre e (metainstrucción-nombre e))
                                      (en-corchete-abierto e))])
      (cond [(null? argumentos)
             (concatena resultado
                        (en-corchete-cerrado e))]
            [(null? (cdr argumentos))
             (concatena resultado
                        (escribe-expresión (car argumentos))
                        (en-corchete-cerrado e))]
            [else
             (itera (cdr argumentos)
                    (concatena resultado
                               (escribe-expresión (car argumentos))
                               (en-coma e)
                               (en-espacio e)))])))
  (define (escribe-expresión e)
    (cond [(variable? e)        (escribe-variable e)]
          [(abstracción? e)     (escribe-abstracción e)]
          [(aplicación? e)      (escribe-aplicación e)]
          [(hueco? e)           (escribe-hueco e)]
          [(metainstrucción? e) (escribe-metainstrucción e)]
          [else
           (error identificador "La expresión ~a no es válida" e)]))
  (lambda (e)
    (escribe-expresión e)))

(define (crea-escritor-breve identificador
                             #:en-nombre [en-nombre (lambda (e x) x)]
                             #:en-paréntesis-abierto [en-paréntesis-abierto (lambda (e) "(")]
                             #:en-paréntesis-cerrado [en-paréntesis-cerrado (lambda (e) ")")]
                             #:en-corchete-abierto [en-corchete-abierto (lambda (e) "[")]
                             #:en-corchete-cerrado [en-corchete-cerrado (lambda (e) "]")]
                             #:en-lambda [en-lambda (lambda (e) (string (integer->char 955)))]
                             #:en-punto [en-punto (lambda (e) ".")]
                             #:en-coma [en-coma (lambda (e) ",")]
                             #:en-espacio [en-espacio (lambda (e) " ")]
                             #:concatena [concatena string-append])
  (define (escribe-variable e)
    (en-nombre e (variable-nombre e)))
  (define (escribe-abstracción e)
    (let itera ([cuerpo (abstracción-cuerpo e)]
                [resultado (concatena (en-lambda e)
                                      (escribe-variable (abstracción-argumento e)))])
      (if (not (abstracción? cuerpo))
          (concatena resultado (en-punto e) (escribe-expresión cuerpo))
          (itera (abstracción-cuerpo cuerpo)
                 (concatena resultado
                            (en-espacio e)
                            (escribe-variable (abstracción-argumento cuerpo)))))))
  (define (escribe-aplicación e)
    (let itera ([operador (aplicación-operador e)]
                [resultado (concatena (en-espacio e)
                                      (let ([e (aplicación-operando e)])
                                        (if (aplicación? e)
                                            (concatena (en-paréntesis-abierto e)
                                                       (escribe-aplicación e)
                                                       (en-paréntesis-cerrado e))
                                            (escribe-expresión e))))])
      (if (not (aplicación? operador))
          (concatena (escribe-expresión operador) resultado)
          (let ([e (aplicación-operando operador)])
            (itera (aplicación-operador operador)
                   (concatena (en-espacio operador)
                              (cond [(aplicación? e)
                                     (concatena (en-paréntesis-abierto e)
                                                (escribe-aplicación e)
                                                (en-paréntesis-cerrado e))]
                                    [(abstracción? e)
                                     (concatena (en-paréntesis-abierto e)
                                                (escribe-abstracción e)
                                                (en-paréntesis-cerrado e))]
                                    [else
                                     (escribe-expresión e)])
                              resultado))))))
  (define (escribe-hueco e)
    (concatena (en-corchete-abierto e)
               (en-espacio e)
               (en-corchete-cerrado e)))
  (define (escribe-metainstrucción e)
    (let itera ([argumentos (metainstrucción-argumentos e)]
                [resultado (concatena (en-nombre e (metainstrucción-nombre e))
                                      (en-corchete-abierto e))])
      (cond [(null? argumentos)
             (concatena resultado
                        (en-corchete-cerrado e))]
            [(null? (cdr argumentos))
             (concatena resultado
                        (escribe-expresión (car argumentos))
                        (en-corchete-cerrado e))]
            [else
             (itera (cdr argumentos)
                    (concatena resultado
                               (escribe-expresión (car argumentos))
                               (en-coma e)
                               (en-espacio e)))])))
  (define (escribe-expresión e)
    (cond [(variable? e) (escribe-variable e)]
          [(abstracción? e) (escribe-abstracción e)]
          [(aplicación? e) (escribe-aplicación e)]
          [(hueco? e) (escribe-hueco e)]
          [(metainstrucción? e) (escribe-metainstrucción e)]
          [else
           (error identificador "La expresión ~a no es válida" e)]))
  (lambda (e)
    (escribe-expresión e)))

(define-syntax (define-escritor stx)
  (syntax-case stx ()
    [(_ objetivo sym val ...)
     (with-syntax ([identificador/brevedad (format-id stx "expresión->~a/brevedad" #'objetivo)]
                   [identificador/formalidad (format-id stx "expresión->~a/formalidad" #'objetivo)])
       #'(begin
           (define identificador/brevedad
             (crea-escritor-breve 'identificador/brevedad sym val ...))
           (define identificador/formalidad
             (crea-escritor-formal 'identificador/formalidad sym val ...))))]))

(define-escritor texto-plano)

(define-escritor latex
  #:en-lambda (lambda (e) "\\lambda ")
  #:en-espacio (lambda (e)
                 (if (hueco? e)
                     "\\quad "
                     "\\, ")))

(define-escritor figura-texto
  #:en-nombre (lambda (e x) (text x))
  #:en-paréntesis-abierto (lambda (e) (text "("))
  #:en-paréntesis-cerrado (lambda (e) (text ")"))
  #:en-corchete-abierto (lambda (e) (text "["))
  #:en-corchete-cerrado (lambda (e) (text "]"))
  #:en-lambda (lambda (e) (text (string (integer->char 955))))
  #:en-punto (lambda (e) (text "."))
  #:en-coma (lambda (e) (text ","))
  #:en-espacio (lambda (e) (text " "))
  #:concatena hbl-append)

