;;
;; Predicados y selectores para el árbol sintáctico
;;

(define (atom? x)
  (not (pair? x)))

(define (abst? x)
  (and (pair? x)
       (eq? 'lambda (car x))
       (= 3 (length x))
       (pair? (cadr x))
       (= 1 (length (cadr x)))
       (atom? (abst-arg x))))

(define (abst-arg x)
  (caadr x))

(define (abst-body x)
  (caddr x))

(define (app? x)
  (and (pair? x)
       (= 2 (length x))))

(define (app-left x)
  (car x))

(define (app-right x)
  (cadr x))

(define (def? x)
  (and (pair? x)
       (eq? 'define (car x))
       (= 3 (length x))
       (atom? (def-name x))))

(define (def-name x)
  (cadr x))

(define (def-val x)
  (caddr x))

;;
;; Entornos para los enlaces
;; 

(define gb '())

(define (extend b name val)
  (cons (cons name val) b))

(define (gextend! name val)
  (set! gb (extend gb name val))
  val)

(define (lookup a b)
  (cond ((or (assv a b) (assv a gb)) => cdr)
        (else a)))

;;
;; Variables de depuración
;; 

(define about? (make-parameter #t))

(define (about str . xs)
  (when (about?)
    (apply format #t str xs)))

(define (eval-error str . xs)
  (apply error 'eval str xs))

;;
;; Evaluador call-by-name
;;

(define (eval-cbn e b)
  (about ";; Evaluando ~a en el entorno ~a\n" e b)
  (cond ((atom? e)
         (let ((r (lookup e b)))
           (if (eqv? r e)
               (begin
                 (about ";;    Átomo auto-evaluado ~a\n" e)
                 r)
               (begin
                 (about ";;    Reemplazo ~a --> ~a\n" e r)
                 (eval-cbn r b)))))
        ((def? e)
         (let ((name (def-name e))
               (val  (eval-cbn (def-val e) b)))
           (about ";;    Definido el reemplazo ~a --> ~a\n" name val)
           (gextend! name val)))
        ((abst? e)
         (let ((e* (let recur ((e* e) (b* b))
                     (cond ((atom? e*)
                            (let ((r (lookup e* b*)))
                              (if (eqv? e* r) r (recur r b*))))
                           ((abst? e*)
                            (list 'lambda
                                  (list (abst-arg e*))
                                  (recur (abst-body e*) (extend b* (abst-arg e*) (abst-arg e*)))))
                           ((def? e*)
                            (eval-error "Abstracción ~a mal formada" e))
                           ((app? e*)
                            (list (recur (app-left e*) b*)
                                  (recur (app-right e*) b*)))
                           (else
                            (eval-error "Abstracción ~a mal formada" e))))))
           (if (equal? e e*)
               (begin
                 (about ";;    Abstracción auto-evaluada ~a\n" e)
                 e)
               (begin
                 (about ";;    Abstracción ~a evaluada con reemplazos conocidos a ~a\n" e e*)
                 e*))))
        ((app? e)
         (about ";;    El lado izquierdo de la aplicación será evaluado\n")
         (let ((op (eval-cbn (app-left e) b)))
           (if (abst? op)
               (begin
                 (about ";;    El lado izquierdo es una abstracción, la aplicación es reducible\n")
                 (eval-cbn (abst-body op) (extend b (abst-arg op) (app-right e))))
               (begin
                 (about ";;    La aplicación no es reducible, será auto-evaluada\n")
                 (list op (app-right e))))))
        (else
         (eval-error "Expresión mal formada" e))))

(define cbn-initial-defs
  '((define I (lambda (x) x))
    (define K (lambda (x) (lambda (y) x)))
    (define S (lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))
    (define T K)
    (define F (lambda (x) (lambda (y) y)))
    (define If (lambda (p) (lambda (m) (lambda (n) ((p m) n)))))
    (define Not (lambda (p) (((If p) F) T)))
    (define Or (lambda (p1) (lambda (p2) (((If p1) T) (((If p2) T) F)))))
    (define And (lambda (p1) (lambda (p2) (((If p1) (((If p2) T) F)) F))))
    (define 0 F)
    (define 0? (lambda (n) ((n (K F)) T)))
    (define 1+ (lambda (n) (lambda (x) (lambda (y) (x ((n x) y))))))
    (define 1 (1+ 0))
    (define 2 (1+ 1))
    (define 3 (1+ 2))
    (define 4 (1+ 3))
    (define + (lambda (m) (lambda (n) ((n 1+) m))))
    (define * (lambda (m) (lambda (n) ((n (+ m)) 0))))
    (define ^ (lambda (m) (lambda (n) ((n (* m)) 1))))
    (define 0* (lambda (x) (lambda (y) (lambda (z) y))))
    (define 1+* (lambda (n*) (lambda (x) (lambda (y) (lambda (z) (((n* x) (z y)) x))))))
    (define 1- (lambda (n) (lambda (x) (lambda (y) (((((n 1+*) 0*) x) y) I)))))
    (define - (lambda (m) (lambda (n) ((n 1-) m))))))

(define (repl-cbn . debug?)
  (format #t "Iniciando REPL con evaluación call-by-name...\n")
  (set! gb '())
  (about? #f)
  (format #t "Cargando entorno de ejecución...\n")
  (for-each (lambda (e) (eval-cbn e '())) cbn-initial-defs)
  (unless (null? debug?)
    (about? #t))
  (let loop ()
    (format #t "(CBN)> ")
    (let ((e (read)))
      (unless (null? e)
        (let ((r (eval-cbn e '())))
          (format #t "~a\n" r)
          (loop)))))
  (format #t "Limpiando entorno...\n")
  (set! gb '()))

;;
;; Evaluador normal-order
;;

(define (eval-no e b)
  (about ";; Evaluando ~a en el entorno ~a\n" e b)
  (cond ((atom? e)
         (let ((r (lookup e b)))
           (if (eqv? r e)
               (begin
                 (about ";;    Átomo auto-evaluado ~a\n" e)
                 r)
               (begin
                 (about ";;    Reemplazo ~a --> ~a\n" e r)
                 (eval-no r b)))))
        ((def? e)
         (let ((name (def-name e))
               (val  (eval-no (def-val e) b)))
           (about ";;    Definido el reemplazo ~a --> ~a\n" name val)
           (gextend! name val)))
        ((abst? e)
         (let ((e* (list 'lambda
                         (list (abst-arg e))
                         (eval-no (abst-body e) (extend b (abst-arg e) (abst-arg e))))))
           (if (equal? e e*)
               (begin
                 (about ";;    Abstracción auto-evaluada ~a\n" e)
                 e)
               (begin
                 (about ";;    Abstracción evaluada a ~a\n" e*)
                 e*))))
        ((app? e)
         (about ";;    El lado izquierdo de la aplicación será evaluado\n")
         (let ((op (eval-no (app-left e) b)))
           (if (abst? op)
               (begin
                 (about ";;    El lado izquierdo es una abstracción, la aplicación es reducible\n")
                 (eval-no (abst-body op) (extend b (abst-arg op) (app-right e))))
               (begin
                 (about ";;    La aplicación no es reducible, el lado derecho será evaluado\n")
                 (let ((right (eval-no (app-right e) b)))
                   (list op right))))))
        (else
         (eval-error "Expresión mal formada" e))))

(define no-initial-defs
  '((define I (lambda (x) x))
    (define K (lambda (x) (lambda (y) x)))
    (define S (lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))
    (define T K)
    (define F (lambda (x) (lambda (y) y)))
    (define If (lambda (p) (lambda (m) (lambda (n) ((p m) n)))))
    (define Not (lambda (p) (((If p) F) T)))
    (define Or (lambda (p1) (lambda (p2) (((If p1) T) (((If p2) T) F)))))
    (define And (lambda (p1) (lambda (p2) (((If p1) (((If p2) T) F)) F))))
    (define 0 F)
    (define 0? (lambda (n) ((n (K F)) T)))
    (define 1+ (lambda (n) (lambda (x) (lambda (y) (x ((n x) y))))))
    (define 1 (1+ 0))
    (define 2 (1+ 1))
    (define 3 (1+ 2))
    (define 4 (1+ 3))
    (define + (lambda (m) (lambda (n) ((n 1+) m))))
    (define * (lambda (m) (lambda (n) ((n (+ m)) 0))))
    (define ^ (lambda (m) (lambda (n) ((n (* m)) 1))))
    (define 0* (lambda (x) (lambda (y) (lambda (z) y))))
    (define 1+* (lambda (n*) (lambda (x) (lambda (y) (lambda (z) (((n* x) (z y)) x))))))
    (define 1- (lambda (n) (lambda (x) (lambda (y) (((((n 1+*) 0*) x) y) I)))))
    (define - (lambda (m) (lambda (n) ((n 1-) m))))))

(define (repl-no . debug?)
  (format #t "Iniciando REPL con evaluación normal-order...\n")
  (set! gb '())
  (about? #f)
  (format #t "Cargando entorno de ejecución...\n")
  (for-each (lambda (e) (eval-no e '())) no-initial-defs)
  (unless (null? debug?)
    (about? #t))
  (let loop ()
    (format #t "(NO)> ")
    (let ((e (read)))
      (unless (null? e)
        (let ((r (eval-no e '())))
          (format #t "~a\n" r)
          (loop))))
    (format #t "Limpiando entorno...\n")
    (set! gb '())))
