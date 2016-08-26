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

(define (alpha? x y)
  )
