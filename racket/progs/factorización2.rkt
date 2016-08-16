;;; -*- mode: racket; coding: utf-8 -*-

#lang racket/base

(require (for-syntax racket/base)
         racket/splicing)

(struct variable (nombre) #:transparent)
(struct abstracción (argumento cuerpo) #:transparent)
(struct aplicación (operador operando) #:transparent)
(struct metainstrucción (nombre argumentos) #:transparent)
(struct hueco () #:transparent)

(define-escritor texto-plano
  (en-nombre (e x)
    x)
  (en-paréntesis-abierto (e)
    "(")
  (en-paréntesis-cerrado (e)
    ")")
  (en-corchete-abierto (e)
    "[")
  (en-corchete-cerrado (e)
    "]")
  (en-lambda (e)
    "λ")
  (en-punto (e)
    ".")
  (en-coma (e)
    ", ")
  (en-espacio (e)
    " ")
  (concatena (x . xs)
    (apply string-append x xs)))

(define-syntax (define-escritor stx)
  (syntax-case stx ()
    [(_ objetivo cláusulas ...)
     #'(begin
         (define-escritor/brevedad objetivo cláusulas ...))]))

(define-syntax (define-escritor/brevedad stx)
  (syntax-case stx ()
    [(_ objetivo cláusulas ...)
     (with-syntax ([Texp (format-id stx "expresión->~a/brevedad" #'objetivo)]
                   [Tvar (format-id stx "variable->~a/brevedad" #'objetivo)]
                   [Tabs (format-id stx "abstracción->~a/brevedad" #'objetivo)]
                   [Tapl (format-id stx "aplicación->~a/brevedad" #'objetivo)]
                   [Thue (format-id stx "hueco->~a/brevedad" #'objetivo)]
                   [Tmet (format-id stx "metainstrucción->~a/brevedad" #'objetivo)])
       #'(splicing-let ([en-nombre
                         (lambda (e x)
                           (error Texp "No es posible traducir un nombre" e))]
                        [en-paréntesis-abierto
                         (lambda (e)
                           (error Texp "No es posible traducir un paréntesis abierto" e))]
                        [en-paréntesis-cerrado
                         (lambda (e)
                           (error Texp "No es posible traducir un paréntesis cerrado" e))]
                        [en-corchete-abierto
                         (lambda (e)
                           (error Texp "No es posible traducir un corchete abierto" e))]
                        [en-corchete-cerrado
                         (lambda (e)
                           (error Texp "No es posible traducir un corchete cerrado" e))]
                        [en-lambda
                         (lambda (e)
                           (error Texp "No es posible traducir una λ" e))]
                        [en-punto
                         (lambda (e)
                           (error Texp "No es posible traducir un punto" e))]
                        [en-coma
                         (lambda (e)
                           (error Texp "No es posible traducir una coma" e))]
                        [en-espacio
                         (lambda (e)
                           (error Texp "No es posible traducir un espacio" e))]
                        [concatena
                         (lambda (x . xs)
                           (error Texp "No es posible concatenar traducciones" x xs))])
           (define-escritor/brevedad* Texp Tvar Tabs Tapl Thue Tmet cláusulas ...)))]))

(define-syntax (define-escritor/brevedad* stx)
  (syntax-case stx ()
    [(_ Texp Tvar Tabs Tapl Thue Tmet)
     #'(begin
         (define (Tvar e)
           (en-nombre e (variable-nombre e)))
         (define (Tabs e)
           (let itera ([cu (abstracción-cuerpo e)]
                       [tr (concatena (en-lambda e) (Tvar (abstracción-argumento e)))])
             (if (not (abstracción? cu))
                 (concatena tr (en-punto e) (Texp cu))
                 (let ([v (abstracción-argumento cu)])
                   (itera (abstracción-cuerpo cu)
                          (concatena tr (en-espacio e) (Tvar v)))))))
         (define (Tapl e)
           (let itera ([op (aplicación-operador e)]
                       [tr (concatena (en-espacio e)
                                      (let ([e (aplicación-operando e)])
                                        (if (aplicación? e)
                                            (concatena (en-paréntesis-abierto e)
                                                       (Tapl e)
                                                       (en-paréntesis-cerrado e))
                                            (Texp e))))])
             (if (not (aplicación? op))
                 (concatena (Texp op) tr)
                 (let ([e (aplicación-operando op)])
                   (itera (aplicación-operador op)
                          (concatena (en-espacio op)
                                     (cond [(aplicación? e)
                                            (concatena (en-paréntesis-abierto e)
                                                       (Tapl e)
                                                       (en-paréntesis-cerrado e))]
                                           [(abstracción? e)
                                            (concatena (en-paréntesis-abierto e)
                                                       (Tabs e)
                                                       (en-paréntesis-cerrado e))]
                                           [else
                                            (Texp e)])
                                     tr))))))
         (define (Thue e)
           (concatena (en-corchete-abierto e)
                      (en-espacio e)
                      (en-corchete-cerrado e)))
         (define (Tmet e)
           (let itera [(args (metainstrucción-argumentos e))
                       (tr   (concatena (en-nombre e (metainstrucción-nombre e))
                                        (en-corchete-abierto e)))]
             (cond [(null? args)
                    (concatena tr (en-corchete-cerrado e))]
                   [(null? (cdr args))
                    (concatena tr (Texp (car args)) (en-corchete-cerrado e))]
                   [else
                    (itera (cdr args)
                           (concatena tr (Texp (car args)) (en-coma e)))])))
         (define (Texp e)
           (cond
             [(variable? e)        (Tvar e)]
             [(abstracción? e)     (Tabs e)]
             [(aplicación? e)      (Tapl e)]
             [(hueco? e)           (Thue e)]
             [(metainstrucción? e) (Tmet e)]
             [else
              (error id "La expresión ~a no es válida" e)])))]
    [(_ Texp Tvar Tabs Tapl Thue Tmet (nombre args cuerpos ...) cláusulas ...)
     #'(splicing-let ([nombre (lambda args cuerpos ...)])
         (define-escritor/brevedad* Texp Tvar Tabs Tapl Thue Tmet cláusulas ...))]))
