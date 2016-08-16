;;; -*- mode: racket; coding: utf-8 -*-

(define (abstracción-argumentos/cuerpo e)
  (let itera ([cuerpo (abstracción-cuerpo e)]
              [argumentos (list (abstracción-argumento e))])
    (if (not (abstracción? cuerpo))
        (values (reverse argumentos) cuerpo)
        (itera (abstracción-cuerpo cuerpo)
               (cons (abstracción-argumento cuerpo)
                     argumentos)))))

(define (aplicación-aplicandos e)
  (let itera ([operador (aplicación-operador e)]
              [aplicandos (list (aplicación-operando e))])
    (if (not (aplicación? operador))
        (cons operador aplicandos)
        (itera (aplicación-operador operador)
               (cons (aplicación-operando operador)
                     aplicandos)))))

(define (abrevia-expresión e)
  (cond [(variable? e) e]
        [(abstracción? e)
         (let-values ([(argumentos cuerpo) (abstracción-argumentos/cuerpo e)])
           (let ([argumentos (map abrevia-expresión argumentos)]
                 [cuerpo (abrevia-expresión cuerpo)])
             (abstracción argumentos cuerpo)))]
        [(aplicación? e)
         (aplicación null (aplicación-aplicandos e))
         (cons 'aplicación (aplicación-aplicandos e))]
        [(hueco? e) e]
        [(metainstrucción? e)
         (list 'metainstrucción
               (metainstrucción-nombre e)
               (map abrevia-expresión (metainstrucción-argumentos e)))]))

