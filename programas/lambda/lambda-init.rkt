(define-metainstrucción (longitud M)
  (if (término? M)
      (let recurre ([M M])
        (match M
          [(variable x)
           1]
          [(abstracción x M)
           (+ 1 (recurre M))]
          [(aplicación M N)
           (+ (recurre M) (recurre N))]))
      -1))
(define-metainstrucción (variables-libres M)
  (match M
    [(variable x)
     (list (variable x))]
    [(abstracción x M)
     (remove x (variables-libres M))]
    [(aplicación M N)
     (remove-duplicates
      (append (variables-libres M)
              (variables-libres N)))]
    [_ null]))
(define-metainstrucción (sustituye M x N)
  (match M
    [(== x)
     N]
    [(variable y)
     M]
    [(aplicación P Q)
     (aplicación (sustituye P x N) (sustituye Q x N))]
    [(abstracción (== x) P)
     M]
    [(abstracción y P)
     (cond [(not (member x (variables-libres P)))
            (abstracción y P)]
           [(not (member y (variables-libres N)))
            (abstracción y (sustituye P x N))]
           [else
            (let ([z (variable-ajena (aplicación N P))])
              (abstracción z (sustituye (sustituye P y z) x N)))])]
    [_ M]))
(define-metainstrucción (variable-ajena M)
  (let ([variables (variables-libres M)])
    (let itera ([letra #\a])
      (if (char>? letra #\z)
          (let itera [(letra #\a) (número 2)]
            (if (char>? letra #\z)
                (itera #\a (+ número 1))
                (let ([v (variable (string-append (string letra)
                                                  (number->string número)))])
                  (if (member v variables)
                      (itera (integer->char (+ (char->integer letra) 1))
                             número)
                      v))))
          (let ([v (variable (string letra))])
            (if (member v variables)
                (itera (integer->char (+ (char->integer letra) 1)))
                v))))))
(define-metainstrucción (llenar-huecos M N)
  (match M
    [(variable x)
     M]
    [(abstracción x P)
     (abstracción x (llenar-huecos P N))]
    [(aplicación P Q)
     (aplicación (llenar-huecos P N)
                 (llenar-huecos Q N))]
    [(hueco)
     N]
    [(metainstrucción x Ps)
     (metainstrucción x (map (lambda (P)
                               (llenar-huecos P N)) Ps))]
    [_ M]))
(define-metainstrucción (T)
  (parsear-cadena "\\x y.x"))

(define-metainstrucción (F)
  (parsear-cadena "\\x y.y"))
(define-metainstrucción (decodifica-numeral M)
  (match M
    [(abstracción (variable f)
                  (abstracción (variable x) M))
     (let itera ([M M] [n 0])
       (match M
         [(== (variable x)) n]
         [(aplicación (== (variable f)) M)
          (itera M (+ n 1))]
         [_ -1]))]
    [_ -1]))
(define-metainstrucción (codifica-número M)
  (match M
    [(variable x)
     (let ([n (string->number x)])
       (if (and (number? n) (integer? n) (>= n 0))
           (abstracción (variable "f")
                        (abstracción (variable "x")
                                     (let itera ([n n])
                                       (if (zero? n)
                                           (variable "x")
                                           (aplicación (variable "f")
                                                       (itera (- n 1)))))))
           M))]
    [_ M]))
