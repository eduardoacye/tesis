#lang racket
(provide (all-defined-out))
(define/contract (expresión? x)
  (any/c . -> . boolean?)
  (or (variable? x) (abstracción? x) (aplicación? x)
      (hueco? x) (metainstrucción? x)))
(define (término? x)
  (cond [(variable? x)    true]
        [(abstracción? x) (término? (abstracción-cuerpo x))]
        [(aplicación? x)  (and (término? (aplicación-operador x))
                               (término? (aplicación-operando x)))]
        [else             false]))
(define/contract (expresión=? e1 e2)
  (expresión? expresión? . -> . boolean?)
  (equal? e1 e2))

(define/contract (término=? t1 t2)
  (término? término? . -> . boolean?)
  (equal? t1 t2))
(define-struct/contract variable
  ([nombre string?])
  #:transparent
  #:mutable)
(define/contract (variable=? v1 v2)
  (variable? variable? . -> . boolean?)
  (eq? v1 v2))

(define/contract (átomo=? v1 v2)
  (variable? variable? . -> . boolean?)
  (equal? v1 v2))
(define-struct/contract abstracción
  ([argumento variable?] [cuerpo expresión?])
  #:transparent
  #:mutable)
(define/contract (argumento=? a1 a2)
  (abstracción? abstracción? . -> . boolean?)
  (átomo=? (abstracción-argumento a1)
           (abstracción-argumento a2)))

(define/contract (cuerpo=? a1 a2)
  (abstracción? abstracción? . -> . boolean?)
  (expresión=? (abstracción-cuerpo a1)
               (abstracción-cuerpo a2)))
(define-struct/contract aplicación
  ([operador expresión?] [operando expresión?])
  #:transparent
  #:mutable)
(define/contract (operador=? a1 a2)
  (aplicación? aplicación? . -> . boolean?)
  (expresión=? (aplicación-operador a1)
               (aplicación-operador a2)))

(define/contract (operando=? a1 a2)
  (aplicación? aplicación? . -> . boolean?)
  (expresión=? (aplicación-operando a1)
               (aplicación-operando a2)))
(define-struct/contract hueco
  ()
  #:transparent)
(define-struct/contract metainstrucción
  ([nombre string?] [argumentos (*list/c expresión?)])
  #:transparent
  #:mutable)
(define metainstrucciones (make-hash))

(define-syntax define-metainstrucción
  (syntax-rules ()
    [(define-metainstrucción (nombre . argumentos)
       cómputo cómputos ...)
     (begin
      (define (nombre . argumentos)
        cómputo cómputos ...)
      (hash-set! metainstrucciones
                 (symbol->string 'nombre)
                 nombre))]))
(define/contract (nombre=? m1 m2)
  (metainstrucción? metainstrucción? . -> . boolean?)
  (string=? (metainstrucción-nombre m1)
            (metainstrucción-nombre m2)))

(define/contract (cantidad-argumentos=? m1 m2)
  (metainstrucción? metainstrucción? . -> . boolean?)
  (= (length (metainstrucción-argumentos m1))
     (length (metainstrucción-argumentos m2))))

(define/contract (argumentos=? m1 m2)
  (metainstrucción? metainstrucción? . -> . boolean?)
  (equal? (metainstrucción-argumentos m1)
          (metainstrucción-argumentos m2)))
(define hay-lambda? true)

(define/contract (símbolo-lambda? x)
  (char? . -> . boolean?)
  (and hay-lambda? (char=? (integer->char 955) x)))
(define/contract (delimitador-final? x)
  ((or/c char? eof-object?) . -> . boolean?)
  (or (eof-object? x) (char=? #\] x) (char=? #\) x)
      (char=? #\. x)  (char=? #\, x) (char=? #\; x)))
(define/contract (parsear-paréntesis p)
  (input-port? . -> . (or/c abstracción? aplicación?))
  (let ([e (parsear-expresión p (peek-char p) null)])
    (unless (char=? #\) (read-char p))
      (error 'parsear-paréntesis "Se esperaba leer el caracter `)'"))
    e))
(define/contract (aplicación/identidad a e)
  ((or/c expresión? null?) expresión? . -> . expresión?)
  (if (null? a) e (aplicación a e)))
(define/contract (parsear-hueco p)
  (input-port? . -> . hueco?)
  (let ([c (peek-char p)])
    (cond [(char-whitespace? c)
           (read-char p)
           (parsear-hueco p)]
          [(char=? #\] c)
           (read-char p)
           (hueco)]
          [else
           (error 'parsear-hueco "Se esperaba leer el caracter `]'")])))
(define/contract (parsear-abstracción p)
  (input-port? . -> . abstracción?)
  (let itera ([c  (peek-char p)]
              [vs null])
    (cond [(and (char? c) (char-whitespace? c))
           (read-char p)
           (itera (peek-char p) vs)]
          [(and (char? c) (char=? #\. c))
           (read-char p)
           (if (null? vs)
               (error 'parsear-abstracción "Se esperaba al menos una variable")
               (let ([e (parsear-expresión p (peek-char p) null)])
                 (if (expresión? e)
                     (foldr abstracción e (reverse vs))
                     (error 'parsear-abstracción
                            "Se esperaba leer el cuerpo de una abstracción"))))]
          [(delimitador? c)
           (error 'parsear-abstracción "Se esperaba leer una abstracción completa")]
          [else
           (let ([v (variable (parsear-nombre p))])
             (itera (peek-char p) (cons v vs)))])))
(define/contract (delimitador? x)
  ((or/c char? eof-object?) . -> . boolean?)
  (or (delimitador-final? x)
      (char=? #\( x) (char=? #\[ x)
      (símbolo-lambda? x) (char=? #\\ x)
      (char-whitespace? x)))
(define/contract (parsear-nombre p)
  (input-port? . -> . string?)
  (let itera ([c  (peek-char p)]
              [cs null])
    (if (delimitador? c)
        (if (null? cs)
            (error 'parsear-nombre "Se intentó leer un nombre de longitud cero")
            (list->string (reverse cs)))
        (begin
         (read-char p)
         (itera (peek-char p) (cons c cs))))))
(define/contract (parsear-corchetes p)
  (input-port? . -> . (*list/c expresión?))
  (let itera ([e  (parsear-expresión p (peek-char p) null)]
              [es null])
    (cond [(eof-object? e)
           (error 'parsear-corchetes
                  "Se esperaba leer una metainstrucción completa")]
          [(char? e)
           (case e
             [(#\]) (reverse es)]
             [(#\,) (itera (parsear-expresión p (peek-char p) null) es)]
             [else
              (error 'parsear-corchetes
                     "Se esperaba leer una metainstrucción completa")])]
          [else
           (itera (parsear-expresión p (peek-char p) null)
                  (cons e es))])))
(define/contract (parsear-expresión p c a)
  (input-port? (or/c char? eof-object?) (or/c expresión? null?)
               . -> . (or/c expresión? eof-object? char?))
  (cond [(eof-object? c)
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
        [(char=? #\[ c)
         (read-char p)
         (let ([e (parsear-hueco p)])
           (parsear-expresión p (peek-char p) (aplicación/identidad a e)))]
        [(or (símbolo-lambda? c) (char=? #\\ c))
         (read-char p)
         (let ([e (parsear-abstracción p)])
           (parsear-expresión p (peek-char p) (aplicación/identidad a e)))]
        [else
         ((lambda (e)
            (parsear-expresión p (peek-char p) (aplicación/identidad a e)))
          (let* ([x (parsear-nombre p)]
                 [c (peek-char p)])
            (cond [(and (char? c) (char=? #\[ c))
                   (read-char p)
                   (metainstrucción x (parsear-corchetes p))]
                  [else
                   (variable x)])))]))
(define/contract (parsear p)
  (input-port? . -> . (or/c expresión? eof-object?))
  (let ([e (parsear-expresión p (peek-char p) null)])
    (cond [(eof-object? e) e]
          [(expresión? e)  e]
          [(char=? #\; e)  (parsear p)]
          [else
           (error 'parsear "No fue posible parsear una expresión válida")])))
(define/contract (parsear-cadena s)
  (string? . -> . (or/c expresión? eof-object?))
  (parsear (open-input-string s)))
(define/contract (escritor-formal e-variable e-abstracción e-aplicación
                                  e-hueco e-metainstrucción)
  ((variable? string? . -> . any/c)                 ; Escritor de variables
   (abstracción? variable? expresión? . -> . any/c) ; Escritor de abstracciones
   (aplicación? expresión? expresión? . -> . any/c) ; Escritor de aplicaciones
   (hueco? . -> . any/c)                            ; Escritor de huecos
   (metainstrucción? string? (*list/c expresión?)   ; Escritor de metainstrucciones
                     . -> . any/c)
   . -> . (expresión? . -> . any/c))      ; Escritor de expresiones especializado
  (lambda (e)
    (cond [(variable? e)
           (e-variable e (variable-nombre e))]
          [(abstracción? e)
           (e-abstracción e (abstracción-argumento e) (abstracción-cuerpo e))]
          [(aplicación? e)
           (e-aplicación e (aplicación-operador e) (aplicación-operando e))]
          [(hueco? e)
           (e-hueco e)]
          [(metainstrucción? e)
           (e-metainstrucción e (metainstrucción-nombre e)
                              (metainstrucción-argumentos e))])))
(define/contract (escritor-breve e-variable e-abstracción e-aplicación
                                 e-hueco e-metainstrucción)
  ((variable? string? . -> . any/c)
   (abstracción? (*list/c variable? variable?) expresión?
                 . -> . any/c)
   (aplicación? (*list/c expresión? expresión? expresión?)
                (*list/c boolean? boolean? boolean?)
                . -> . any/c)
   (hueco? . -> . any/c)
   (metainstrucción? string? (*list/c expresión?) . -> . any/c)
   . -> . (expresión? . -> . any/c))
  (lambda (e)
    (cond [(variable? e)
           (e-variable e (variable-nombre e))]
          [(abstracción? e)
           (let itera ([cuerpo (abstracción-cuerpo e)]
                       [argumentos (list (abstracción-argumento e))])
             (if (abstracción? cuerpo)
                 (itera (abstracción-cuerpo cuerpo)
                        (cons (abstracción-argumento cuerpo)
                              argumentos))
                 (e-abstracción e (reverse argumentos) cuerpo)))]
          [(aplicación? e)
           (let itera ([operador (aplicación-operador e)]
                       [operandos (list (aplicación-operando e))]
                       [paréntesis (list (aplicación? (aplicación-operando e)))])
             (if (aplicación? operador)
                 (let ([operando (aplicación-operando operador)])
                   (itera (aplicación-operador operador)
                          (cons operando operandos)
                          (cons (or (aplicación? operando)
                                    (abstracción? operando))
                                paréntesis)))
                 (e-aplicación e (cons operador operandos)
                               (cons (or (aplicación? operador)
                                         (abstracción? operador))
                                     paréntesis))))]
          [(hueco? e)
           (e-hueco e)]
          [(metainstrucción? e)
           (e-metainstrucción e (metainstrucción-nombre e)
                              (metainstrucción-argumentos e))]))
                  )
(define escribir-expresión-formal
  (escritor-formal
   (lambda (e nombre) nombre)
   (lambda (e argumento cuerpo)
     (format "(~a~a.~a)"
             (if hay-lambda? (integer->char 955) #\\)
             (escribir-expresión-formal argumento)
             (escribir-expresión-formal cuerpo)))
   (lambda (e operador operando)
     (format "(~a ~a)"
             (escribir-expresión-formal operador)
             (escribir-expresión-formal operando)))
   (lambda (e) "[ ]")
   (lambda (e nombre argumentos)
     (format "~a[~a]" nombre
             (apply string-append
                    (add-between (map escribir-expresión-formal argumentos)
                                 ", "))))))
(define escribir-expresión-breve
  (escritor-breve
   (lambda (e nombre) nombre)
   (lambda (e argumentos cuerpo)
     (format "~a~a.~a"
             (if hay-lambda? (integer->char 955) #\\)
             (apply string-append
                    (add-between (map escribir-expresión-breve argumentos)
                                 " "))
             (escribir-expresión-breve cuerpo)))
   (lambda (e operandos paréntesis)
     (apply string-append
            (add-between (map (lambda (o p)
                                (format "~a~a~a"
                                        (if p "(" "")
                                        (escribir-expresión-breve o)
                                        (if p ")" "")))
                              operandos paréntesis)
                         " ")))
   (lambda (e) "[ ]")
   (lambda (e nombre argumentos)
     (format "~a[~a]" nombre
             (apply string-append
                    (add-between (map escribir-expresión-breve argumentos)
                                 ", "))))))
(define escribir-latex-expresión-formal
  (escritor-formal
   (lambda (e nombre) nombre)
   (lambda (e argumento cuerpo)
     (format "(\\lambda ~a.~a)"
             (escribir-latex-expresión-formal argumento)
             (escribir-latex-expresión-formal cuerpo)))
   (lambda (e operador operando)
     (format "(~a\\, ~a)"
             (escribir-latex-expresión-formal operador)
             (escribir-latex-expresión-formal operando)))
   (lambda (e) "[\\quad ]")
   (lambda (e nombre argumentos)
     (format "~a[~a]" nombre
             (apply string-append
                    (add-between (map escribir-latex-expresión-formal
                                      argumentos)
                                 ",\\, "))))))
(define plantilla-latex
#<<DOCUMENTO-LATEX
  
\documentclass[preview]{standalone}
\usepackage{amsmath}

\begin{document}
\( ~a \)
\end{document}

DOCUMENTO-LATEX
  )
(define escribir-json-expresión-formal
  (escritor-formal
   (lambda (e nombre)
     (format "{ ~s : ~s, ~s : ~s }"
             "tipo" "variable"
             "nombre" nombre))
   (lambda (e argumento cuerpo)
     (format "{ ~s : ~s, ~s : ~a, ~s : ~a }"
             "tipo" "abstracción"
             "argumento" (escribir-json-expresión-formal argumento)
             "cuerpo" (escribir-json-expresión-formal argumento)))
   (lambda (e operador operando)
     (format "{ ~s : ~s, ~s : ~a, ~s : ~a }"
             "tipo" "aplicación"
             "operador" (escribir-json-expresión-formal operador)
             "operando" (escribir-json-expresión-formal operando)))
   (lambda (e)
     (format "{ ~s : ~s }"
             "tipo" "hueco"))
   (lambda (e nombre argumentos)
     (format "{ ~s : ~s, ~s : ~s, ~s : [~a] }"
             "tipo" "metainstrucción"
             "nombre" nombre
             "argumentos"
             (apply string-append
                    (add-between (map escribir-json-expresión-formal
                                      argumentos)
                                 ", "))))))
(define escribir-latex-expresión-breve
  (escritor-breve
   (lambda (e nombre) nombre)
   (lambda (e argumentos cuerpo)
     (format "\\lambda ~a.~a"
             (apply string-append
                    (add-between (map escribir-latex-expresión-breve
                                      argumentos)
                                 "\\, "))
             (escribir-latex-expresión-breve cuerpo)))
   (lambda (e operandos paréntesis)
     (apply string-append
            (add-between (map (lambda (o p)
                                (format "~a~a~a"
                                        (if p "(" "")
                                        (escribir-latex-expresión-breve o)
                                        (if p ")" "")))
                              operandos paréntesis)
                         "\\, ")))
   (lambda (e) "[\\quad ]")
   (lambda (e nombre argumentos)
     (format "~a[~a]" nombre
             (apply string-append
                    (add-between (map escribir-latex-expresión-breve
                                      argumentos)
                                 ",\\, "))))))
(define/contract (evaluar-expresión e)
  (expresión? . -> . any)
  (cond [(abstracción? e)
         (abstracción (evaluar-expresión (abstracción-argumento e))
                      (evaluar-expresión (abstracción-cuerpo e)))]
        [(aplicación? e)
         (aplicación (evaluar-expresión (aplicación-operador e))
                     (evaluar-expresión (aplicación-operando e)))]
        [(metainstrucción? e)
         (cond [(hash-ref metainstrucciones (metainstrucción-nombre e) #f) =>
                (lambda (f)
                  (apply f (map evaluar-expresión (metainstrucción-argumentos e))))]
               [else
                (metainstrucción (metainstrucción-nombre e)
                                 (map evaluar-expresión
                                      (metainstrucción-argumentos e)))])]
        [else e]))
(define/contract (evaluar-cadena s)
  (string? . -> . any)
  (evaluar-expresión (parsear-cadena s)))
