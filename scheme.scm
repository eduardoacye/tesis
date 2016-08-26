;; 
;; Combinadores SKI
;;

(define :I
  ;; Combinador identidad
  (lambda (:x) :x))

(define :K
  ;; Combinador constante
  (lambda (:x) (lambda (:y) :x)))

(define :S
  ;; Combinador de sustitucion
  (lambda (:x) (lambda (:y) (lambda (:z) ((:x :z) (:y :z))))))

;; 
;; Algebra booleana
;;

(define :T
  ;; Combinador verdadero
  :K)

(define :F
  ;; Combinador falso
  (lambda (:x) (lambda (:y) :y)))

(define (boolean->church b)
  ;; procedimiento que toma un objeto booleano de scheme y regresa el
  ;; combinador que codifica al valor
  (if b :T :F))

(define (church->boolean :p)
  ;; Procedimiento que toma un combinador booleano y regresa el objeto
  ;; booleano de scheme que representa al valor
  ((:p #t) #f))

(define :IF
  ;; Combinador condicional
  (lambda (:p) (lambda (:m) (lambda (:n) ((:p :m) :n)))))

(define :NOT
  ;; Combinador negacion
  (lambda (:p) (((:IF :p) :F) :T)))

(define :OR
  ;; Combinador disyuncion
  (lambda (:p1) (lambda (:p2) (((:IF :p1) :T)(((:IF :p2) :T) :F)))))

(define :AND
  ;; Combinador conjuncion
  (lambda (:p1) (lambda (:p2) (((:IF :p1) (((:IF :p2) :T) :F) :F)))))

;; 
;; Aritmetica
;; 
(define :0
  ;; Combinador cero
  :F)

(define :0?
  ;; Combinador predicado para determinar si un termino lambda es el
  ;; combinador cero
  (lambda (:n) ((:n (:K :F)) :T)))

(define :SUCC
  ;; Combinador sucesor
  (lambda (:n) (lambda (:x) (lambda (:y) (:x ((:n :x) :y))))))

(define :1
  ;; Combinador uno
  (:SUCC :0))

(define :2
  ;; Combinador dos
  (:SUCC :1))

(define :3
  ;; Combinador tres
  (:SUCC :2))

(define :4
  ;; Combinador cuatro
  (:SUCC :3))

(define :+
  ;; Combinador adicion
  (lambda (:m) (lambda (:n) ((:n :SUCC) :m))))

(define :*
  ;; Combinador multiplicacion
  (lambda (:m) (lambda (:n) ((:n (:+ :m)) :0))))

(define :^
  ;; Combinador exponenciacion
  (lambda (:m) (lambda (:n) ((:n (:* :m)) :1))))

(define (number->church n)
  ;; Procedimiento que toma un objeto numerico de scheme y regresa el
  ;; combinador que codifica al valor
  (if (zero? n)
      :0
      (:SUCC (number->church (- n 1)))))

(define (church->number :n)
  ;; Procedimiento que toma un combinador numerico y regresa el valor de
  ;; scheme que representa el valor
  ((:n (lambda (x) (+ 1 x))) 0))

(define :0*
  ;; Combinador cero modificado (utilizado para computar el predecesor)
  (lambda (:x) (lambda (:y) (lambda (:z) :y))))

(define :SUCC*
  ;; Combinador sucesor modificado (utilizado para computar el predecesor)
  (lambda (:n*) (lambda (:x) (lambda (:y) (lambda (:z) (((:n* :x) (:z :y)) :x))))))

(define :PRED
  ;; Combinador predecesor
  (lambda (:n) (lambda (:x) (lambda (:y) (((((:n :SUCC*) :0*) :x) :y) :I)))))

(define :-
  ;; Combinador sustraccion
  (lambda (:m) (lambda (:n) ((:n :PRED) :m))))
