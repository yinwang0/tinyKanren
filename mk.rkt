#lang racket

(provide run* run exist conde succeed fail == debug)


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
       (let ([p (assq v s)])
         (cond
           [(not p) v]
           [else
            (walk (cdr p) s)]))]
      [else v])))

(define walk*
  (lambda (v s)
    (let ([v (walk v s)])
      (cond
        ((var? v) v)
        ((pair? v)
         (cons
          (walk* (car v) s)
          (walk* (cdr v) s)))
        (else v)))))

(define unify
  (lambda (u v s)
    (let ([u (walk u s)]
          [v (walk v s)])
      (cond
        [(eq? u v) s]
        [(var? u) (ext-s u v s)]
        [(var? v) (ext-s v u s)]
        [(and (pair? u) (pair? v))
         (let ([s (unify (car u) (car v) s)])
           (and s (unify (cdr u) (cdr v) s)))]
        [(equal? u v) s]
        [else #f]))))

(define name
  (lambda (n)
    (string->symbol
     (string-append "_" (number->string n)))))

(define reify-s
  (lambda (v s)
    (let ([v (walk v s)])
      (cond
        [(var? v)
         (ext-s v (name (size-s s)) s)]
        [(pair? v)
         (reify-s (cdr v)
                  (reify-s (car v) s))]
        [else s]))))

(define reify
  (lambda (v s)
    (let ([v (walk* v s)])
      (walk* v (reify-s v empty-s)))))


;;-------------------------- multiplexing ---------------------------

(struct Stream (head tail))

(define-syntax delay
  (syntax-rules ()
    [(_ e ...) (lambda () e ...)]))

(define force
  (lambda (thunk)
    (thunk)))

(define bind
  (lambda (v g)
    (match v
      [#f #f]
      [(? procedure? v)
       (delay (bind (v) g))]
      [(Stream head tail)
       (mplus (g head)
              (delay (bind (force tail) g)))]
      [_ (g v)])))

(define bind*
  (lambda (v gs)
    (foldl (lambda (g v) (bind v g)) v gs)))

(define mplus
  (lambda (v f)
    (match v
      [#f (force f)]
      [(? procedure? v)
       (delay (mplus (force f) v))]
      [(Stream head tail)
       (Stream head
               (delay (mplus (force f) tail)))]
      [_ (Stream v f)])))

(define-syntax mplus*
  (syntax-rules ()
    [(_ e) e]
    [(_ e0 e ...)
     (mplus e0 (delay (mplus* e ...)))]))


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
       (delay
         (let ([x (var 'x)] ...)
           (bind* s (list g0 g ...)))))]))

(define-syntax conde
  (syntax-rules ()
    [(_ [g0 g ...]
        [g1 g^ ...] ...)
     (lambda (s)
       (delay
         (mplus*
          (bind* s (list g0 g ...))
          (bind* s (list g1 g^ ...)) ...)))]))


;;---------------------------- top level ----------------------------

(define take
  (lambda (n v)
    (cond
      [(zero? n) '()]
      [else
       (match v
         [#f '()]
         [(? procedure? v)
          (take n (v))]
         [(Stream head tail)
          (cons head (take (- n 1) tail))]
         [_ (list v)])])))

(define do-display #f)

(define debug
  (lambda (v)
    (set! do-display v)))

(define debug-display
  (lambda (n contents)
    (if do-display
        (if (= n +inf.0)
            (pretty-print `(run* ,@contents))
            (pretty-print `(run ,n ,@contents)))
        (void))))

(define-syntax run
  (syntax-rules ()
    [(_ n (x) g0 g ...)
     (begin
       (debug-display n '(x g0 g ...))
       (let ([top-g (exist (x)
                      g0 g ...
                      (lambda (s)
                        (reify x s)))])
         (take n (top-g empty-s))))]))

(define-syntax run*
  (syntax-rules ()
    [(_ (x) g ...)
     (run +inf.0 (x) g ...)]))
