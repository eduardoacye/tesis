;;; -*- mode: racket; coding: utf-8 -*-
;;; Macrología para la factorización de escritores

#lang racket/base

(require "estructuras.rkt"
         racket/list
         racket/string
         (for-syntax racket/base))

#|
Hay dos algoritmos de escritura, uno que favorece la formalidad y otro que favorece la brevedad;
el lenguaje objetivo de un escritor determina las transformaciones que son aplicadas a las entidades
sintácticas de una expresión. Sea ``objetivo'' el nombre de la clase de resultados de la traducción
y sea ``modalidad'' el símbolo formalidad o brevedad. Toda implementación de escritor debe definir
los siguientes procedimientos:

* expresión->objetivo/modalidad : expresión? -> objetivo?
* variable->objetivo/modalidad : expresión? -> objetivo?
* abstracción->objetivo/modalidad : expresión? -> objetivo?
* aplicación->objetivo/modalidad : expresión? -> objetivo?
* hueco->objetivo/modalidad : expresión? -> objetivo?
* metainstrucción->objetivo/modalidad : expresión? -> objetivo?
objetivo/modalidad-nombre : expresión?, string? -> objetivo?
objetivo/modalidad-paréntesis-abierto : expresión? -> objetivo?
objetivo/modalidad-paréntesis-cerrado : expresión? -> objetivo?
objetivo/modalidad-corchete-abierto : expresión? -> objetivo?
objetivo/modalidad-corchete-cerrado : expresión? -> objetivo?
objetivo/modalidad-lambda : expresión? -> objetivo?
objetivo/modalidad-punto : expresión? -> objetivo?
objetivo/modalidad-coma : expresión? -> objetivo?
objetivo/modalidad-espacio : expresión? -> objetivo?
objetivo/modalidad-concatena : objetivo?, objetivo? ... -> objetivo?

|#

#|
Sea objetivo un formato objetivo para la traducción de expresiones λ:
  El punto de entrada del escritor a objetivo es el procedimiento
|#

(define (expresión->objetivo e)
  (cond
    [(variable? e)
     (variable->objetivo e)]
    [(abstracción? e)
     (abstracción->objetivo e)]
    [(aplicación? e)
     (aplicación->objetivo e)]
    [(hueco? e)
     (hueco->objetivo e)]
    [(metainstrucción? e)
     (metainstrucción->objetivo e)]
    [else
     (error 'expresión->objetivo
            "La expresión ~a no es válida" e)]))

#|
Sea tipo el nombre de la clase de expresión a traducir:
  La implementación del escritor especializado para tipo puede basarse en
  dos algoritmos:
    * Escritura formal (sin abuso de notación)
    * Escritura breve (con abuso de notación)
|#

(define (aplicación->objetivo/brevedad e)
  (let itera ([operador  (aplicación-operador e)]
              [traducción (objetivo/brevedad-concatena
                           (objetivo/brevedad-espacio e)
                           (if (aplicación? e)
                               (objetivo/brevedad-concatena
                                (objetivo/brevedad-paréntesis-abierto e)
                                (aplicación->objetivo/brevedad e)
                                (objetivo/brevedad-paréntesis-cerrado e))
                               (expresión->objetivo/brevedad e)))])
    (if (not (aplicación? operador))
        (objetivo/brevedad-concatena
         (expresión->objetivo/brevedad operador)
         traducción)
        (let ([e (aplicación-operando operador)])
          (itera (aplicación-operador operador)
                 (objetivo/brevedad-concatena
                  (objetivo/brevedad-espacio operador)
                  (cond [(aplicación? e)
                         (objetivo/brevedad-concatena
                          (objetivo/brevedad-paréntesis-abierto e)
                          (aplicación->objetivo/brevedad e)
                          (objetivo/brevedad-paréntesis-cerrado e))]
                        [(abstracción? e)
                         (objetivo/brevedad-concatena
                          (objetivo/brevedad-paréntesis-abierto e)
                          (abstracción->objetivo/brevedad e)
                          (objetivo/brevedad-paréntesis-cerrado e))]
                        [else
                         (expresión->objetivo/brevedad e)])
                  traducción))))))
































(require (for-syntax racket/syntax))

(define-syntax (our-struct stx)
  (syntax-case stx ()
    [(_ id (fields ...))
     (with-syntax ([pred-id (format-id stx "~a?" #'id)])
       #`(begin
           ; Define a constructor.
           (define (id fields ...)
             (apply vector (cons 'id  (list fields ...))))
           ; Define a predicate.
           (define (pred-id v)
             (and (vector? v)
                  (eq? (vector-ref v 0) 'id)))
           ; Define an accessor for each field.
           #,@(for/list ([x (syntax->list #'(fields ...))]
                         [n (in-naturals 1)])
                (with-syntax ([acc-id (format-id stx "~a-~a" #'id x)]
                              [ix n])
                  #`(define (acc-id v)
                      (unless (pred-id v)
                        (error 'acc-id "~a is not a ~a struct" v 'id))
                      (vector-ref v ix))))))]))

(define-syntax (define-escritor stx)
  (syntax-case stx ()
    [(_ objetivo def defs ...)
     (with-syntax ([expresión->objetivo/formalidad (format-id stx "expresión->~a/formalidad" #'objetivo)]
                   [variable->objetivo/formalidad (format-id stx "variable->~a/formalidad" #'objetivo)]
                   [abstracción->objetivo/formalidad (format-id stx "abstracción->~a/formalidad" #'objetivo)]
                   [aplicación->objetivo/formalidad (format-id stx "aplicación->~a/formalidad" #'objetivo)]
                   [hueco->objetivo/formalidad (format-id stx "hueco->~a/formalidad" #'objetivo)]
                   [metainstrucción->objetivo/formalidad (format-id stx "metainstrucción->~a/formalidad" #'objetivo)]
                   [objetivo/formalidad-nombre (format-id stx "~a/formalidad-nombre" #'objetivo)]
                   [objetivo/formalidad-paréntesis-abierto (format-id stx "~a/formalidad-paréntesis-abierto" #'objetivo)]
                   [objetivo/formalidad-paréntesis-cerrado (format-id stx "~a/formalidad-paréntesis-cerrado" #'objetivo)]
                   [objetivo/formalidad-corchete-abierto (format-id stx "~a/formalidad-corchete-abierto" #'objetivo)]
                   [objetivo/formalidad-corchete-cerrado (format-id stx "~a/formalidad-corchete-cerrado" #'objetivo)]
                   [objetivo/formalidad-lambda (format-id stx "~a/formalidad-lambda" #'objetivo)]
                   [objetivo/formalidad-punto (format-id stx "~a/formalidad-punto" #'objetivo)]
                   [objetivo/formalidad-coma (format-id stx "~a/formalidad-coma" #'objetivo)]
                   [objetivo/formalidad-espacio (format-id stx "~a/formalidad-espacio" #'objetivo)]
                   [objetivo/formalidad-concatena (format-id stx "~a/formalidad-concatena" #'objetivo)]
                   ))]))

(define-syntax (define-escritor-breve stx)
  (syntax-case stx ()
    [(_ objetivo def defs ...)
     (with-syntax ([expresión->objetivo/brevedad (format-id stx "expresión->~a/brevedad" #'objetivo)])
       #'(splicing-let ([en-nombre
                         (lambda (e x)
                           (error 'expresión->objetivo/brevedad
                                  "El escritor no admite traducir nombres"))]
                        [en-paréntesis-abierto
                         (lambda (e)
                           (error 'expresión->objetivo/brevedad
                                  "El escritor no admite traducir el paréntesis abierto"))]
                        [en-paréntesis-cerrado
                         (lambda (e)
                           (error 'expresión->objetivo/brevedad
                                  "El escritor no admite traducir el paréntesis cerrado"))]
                        [en-corchete-abierto
                         (lambda (e)
                           (error 'expresión->objetivo/brevedad
                                  "El escritor no admite traducir el corchete abierto"))]
                        [en-corchete-cerrado
                         (lambda (e)
                           (error 'expresión->objetivo/brevedad
                                  "El escritor no admite traducir el corchete cerrado"))]
                        [])))]))

(define-syntax (define-escritor-breve stx)
  (syntax-case stx ()
    [(_ objetivo symb val ...)
     (with-syntax ([expresión->objetivo/brevedad (format-id stx "expresión->~a/brevedad" #'objetivo)])
       #'(define expresión->objetivo/brevedad
           (crea-escritor-breve #:identificador expresión->objetivo/brevedad
                                symb val ...)))]))

(define (crea-escritor-breve #:identificador id
                             #:en-nombre Tn
                             #:en-paréntesis-abierto Tpa
                             #:en-paréntesis-cerrado Tpc
                             #:en-corchete-abierto Tca
                             #:en-corchete-cerrado Tcc
                             #:en-lambda Tl
                             #:en-punto Tp
                             #:en-coma Tc
                             #:en-espacio Te
                             #:concatena ><)
  (define (Tvar e)
    (Tn e (variable-nombre e)))
  (define (Tabs e)
    (let itera ([cu (abstracción-cuerpo e)]
                [tr (>< (Tl e) (Tvar (abstracción-argumento e)))])
      (if (not (abstracción? cu))
          (>< tr (Tp e) (Texp cu))
          (let ([v (abstracción-argumento cu)])
            (itera (abstracción-cuerpo cu)
                   (>< tr (Te e) (Tvar v)))))))
  (define (Tapl e)
    (let itera ([op (aplicación-operador e)]
                [tr (>< (Te e) (if (aplicación? e)
                                   (>< (Tpa e) (Tapl e) (Tpc))
                                   (Texp e)))])
      (if (not (aplicación? op))
          (>< (Texp op) tr)
          (let ([e (aplicación-operando op)])
            (itera (aplicación-operador op)
                   (>< (Te op)
                       (cond [(aplicación? e)  (>< (Tpa e) (Tapl e) (Tpc e))]
                             [(abstracción? e) (>< (Tpa e) (Tabs e) (Tpc e))]
                             [else             (Texp e)])
                       tr))))))
  (define (Thue e)
    (>< (Tca e) (Te e) (Tcc e)))
  (define (Tmet e)
    (let itera [(args (metainstrucción-argumentos e))
                (tr   (>< (Tn (metainstrucción-nombre e)) (Tca e)))]
      (cond [(null? args)       (>< tr (Tcc e))]
            [(null? (cdr args)) (>< tr (Texp (car args)) (Tcc e))]
            [else               (itera (cdr args)
                                       (>< tr (Texp (car args)) (Tc e)))])))
  (define (Texp e)
    (cond
      [(variable? e)        (Tvar e)]
      [(abstracción? e)     (Tabs e)]
      [(aplicación? e)      (Tapl e)]
      [(hueco? e)           (Thue e)]
      [(metainstrucción? e) (Tmet e)]
      [else
       (error id "La expresión ~a no es válida" e)]))
  Texp)

(define-escritor objetivo
  (en-nombre (lambda (e x) ...))
  (en-paréntesis-abierto (lambda (e) ...))
  (en-paréntesis-cerrado (lambda (e) ...))
  (en-corchete-abierto (lambda (e) ...))
  (en-corchete-cerrado (lambda (e) ...))
  (en-lambda (lambda (e) ...))
  (en-punto (lambda (e) ...))
  (en-coma (lambda (e) ...))
  (en-espacio (lambda (e) ...))
  (concatena
   (lambda (x . xs)
     ...)))

;;
;;
;;
;;
;;
;;

#lang racket

(require (for-syntax racket/syntax))

(struct variable (nombre) #:transparent)

(struct abstracción (argumento cuerpo) #:transparent)

(struct aplicación (operador operando) #:transparent)

(struct metainstrucción (nombre argumentos) #:transparent)

(struct hueco () #:transparent)

(define-syntax (define-escritor-breve stx)
  (syntax-case stx ()
    [(_ objetivo sym val ...)
     (with-syntax ([Texp (format-id stx "expresión->~a/brevedad" #'objetivo)])
       #'(define Texp
           (crea-escritor-breve 'Texp sym val ...)))]))

(define (crea-escritor-breve id
                             #:en-nombre Tn
                             #:en-paréntesis-abierto Tpa
                             #:en-paréntesis-cerrado Tpc
                             #:en-corchete-abierto Tca
                             #:en-corchete-cerrado Tcc
                             #:en-lambda Tl
                             #:en-punto Tp
                             #:en-coma Tc
                             #:en-espacio Te
                             #:concatena ><)
  (define (Tvar e)
    (Tn e (variable-nombre e)))
  (define (Tabs e)
    (let itera ([cu (abstracción-cuerpo e)]
                [tr (>< (Tl e) (Tvar (abstracción-argumento e)))])
      (if (not (abstracción? cu))
          (>< tr (Tp e) (Texp cu))
          (let ([v (abstracción-argumento cu)])
            (itera (abstracción-cuerpo cu)
                   (>< tr (Te e) (Tvar v)))))))
  (define (Tapl e)
    (let itera ([op (aplicación-operador e)]
                [tr (>< (Te e) (let ([e (aplicación-operando e)])
                                 (if (aplicación? e)
                                     (>< (Tpa e) (Tapl e) (Tpc e))
                                     (Texp e))))])
      (if (not (aplicación? op))
          (>< (Texp op) tr)
          (let ([e (aplicación-operando op)])
            (itera (aplicación-operador op)
                   (>< (Te op)
                       (cond [(aplicación? e)  (>< (Tpa e) (Tapl e) (Tpc e))]
                             [(abstracción? e) (>< (Tpa e) (Tabs e) (Tpc e))]
                             [else             (Texp e)])
                       tr))))))
  (define (Thue e)
    (>< (Tca e) (Te e) (Tcc e)))
  (define (Tmet e)
    (let itera [(args (metainstrucción-argumentos e))
                (tr   (>< (Tn e (metainstrucción-nombre e)) (Tca e)))]
      (cond [(null? args)       (>< tr (Tcc e))]
            [(null? (cdr args)) (>< tr (Texp (car args)) (Tcc e))]
            [else               (itera (cdr args)
                                       (>< tr (Texp (car args)) (Tc e)))])))
  (define (Texp e)
    (cond
      [(variable? e)        (Tvar e)]
      [(abstracción? e)     (Tabs e)]
      [(aplicación? e)      (Tapl e)]
      [(hueco? e)           (Thue e)]
      [(metainstrucción? e) (Tmet e)]
      [else
       (error id "La expresión ~a no es válida" e)]))
  (lambda (e) (Texp e)))

(define-escritor-breve texto-plano
  #:en-nombre (lambda (e x) x)
  #:en-paréntesis-abierto (lambda (e) "(")
  #:en-paréntesis-cerrado (lambda (e) ")")
  #:en-corchete-abierto (lambda (e) "[")
  #:en-corchete-cerrado (lambda (e) "]")
  #:en-lambda (lambda (e) "λ")
  #:en-punto (lambda (e) ".")
  #:en-coma (lambda (e) ", ")
  #:en-espacio (lambda (e) " ")
  #:concatena (lambda (x . xs)
                (apply string-append x xs)))
