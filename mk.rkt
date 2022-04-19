#lang racket

; (provide (all-defined-out))
(provide run* run exist conde succeed fail ==)

(define *display* #f)


;;-------------------------- substitution ---------------------------
(define var
  (lambda (name)
    (vector name)))

(define var?
  (lambda (x)
    (vector? x)))

(define empty-s '())

(define size-s length)

(define ext-s
  (lambda (x v s)
    (cons `(,x . ,v) s)))

(define walk
  (lambda (v s)
    (cond
      [(var? v)
       (let ([a (assq v s)])
         (cond
           [a (walk (cdr a) s)]
           [else v]))]
      [else v])))

(define walk*
  (lambda (w s)
    (let ((v (walk w s)))
      (cond
        ((var? v) v)
        ((pair? v)
         (cons
          (walk* (car v) s)
          (walk* (cdr v) s)))
        (else v)))))

(define unify
  (lambda (u v s)
    (let ((u (walk u s))
          (v (walk v s)))
      (cond
        [(eq? u v) s]
        [(var? u) (ext-s u v s)]
        [(var? v) (ext-s v u s)]
        [(and (pair? u) (pair? v))
         (let ([s (unify (car u) (car v) s)])
           (and s (unify (cdr u) (cdr v) s)))]
        [(equal? u v) s]
        [else #f]))))

(define reify-name
  (lambda (n)
    (string->symbol
     (string-append "_" (number->string n)))))

(define reify-s
  (lambda (v s)
    (let ([v (walk v s)])
      (cond
        [(var? v)
         (ext-s v (reify-name (size-s s)) s)]
        [(pair? v)
         (reify-s (cdr v)
                  (reify-s (car v) s))]
        [else s]))))

(define reify
  (lambda (v s)
    (let ([v1 (walk* v s)])
      (walk* v1 (reify-s v1 empty-s)))))


;;-------------------------- multiplexing ---------------------------

(define stream?
  (lambda (x)
    (and (pair? x) (procedure? (cdr x)))))

(define bind
  (lambda (ainf g)
    (cond
      [(not ainf)
       (mzero)]
      [(not (stream? ainf))
       (g ainf)]
      [else
       (mplus (g (car ainf))
              (lambda () (bind ((cdr ainf)) g)))])))

(define-syntax bind*
  (syntax-rules ()
    [(_ e) e]
    [(_ e g0 g ...)
     (bind* (bind e g0) g ...)]))

(define mzero
  (lambda () #f))

(define mplus
  (lambda (ainf f)
    (cond
      [(not ainf) (f)]
      [(not (stream? ainf))
       (cons ainf f)]
      [else
       (cons (car ainf) (lambda () (mplus (f) (cdr ainf))))])))

(define-syntax mplus*
  (syntax-rules ()
    [(_ e) e]
    [(_ e0 e ...)
     (mplus e0 (lambda () (mplus* e ...)))]))


;;------------------------ goal constructors ------------------------

(define ==
  (lambda (u v)
    (lambda (s)
      (unify u v s))))

(define succeed (== #f #f))
(define fail (== #f #t))

(define-syntax exist
  (syntax-rules ()
    [(_ (x ...) g0 g ...)
     (lambda (s)
       (let ([x (var 'x)] ...)
         (bind* s g0 g ...)))]))

(define-syntax conde
  (syntax-rules ()
    [(_ [g0 g ...]
        [g1 g^ ...] ...)
     (lambda (s)
       (mplus*
        (bind* s g0 g ...)
        (bind* s g1 g^ ...) ...))]))


;;---------------------------- top level ----------------------------

(define take
  (lambda (n f)
    (cond
      [(and n (zero? n))
       '()]
      [else
       (let ([fv (f)])
         (cond
           [(not fv) '()]
           [(not (stream? fv)) fv]
           [else
            (cons (car (car fv))
                  (take (and n (- n 1)) (cdr fv)))]))])))

(define-syntax run
  (syntax-rules ()
    ((_ n (x) g0 g ...)
     (begin
       (if *display*
           (if (eq? n #f)
               (pretty-print '(run* (x) g0 g ...))
               (pretty-print '(run n (x) g0 g ...)))
           (void))
       (take n
             (lambda ()
               ((exist (x) g0 g ...
                  (lambda (s)
                    (cons (reify x s) '())))
                empty-s)))))))

(define-syntax run*
  (syntax-rules ()
    [(_ (x) g ...)
     (run #f (x) g ...)]))
