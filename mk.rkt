#lang racket

(provide (all-defined-out))


;;--------------------- substitution ----------------------

(struct var (name) #:transparent)

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
        [(var? v) v]
        [(pair? v)
         (cons
          (walk* (car v) s)
          (walk* (cdr v) s))]
        [else v]))))

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

(define name-prefix "_")

(define set-name-prefix
  (lambda (s)
    (set! name-prefix s)))

(define name
  (lambda (n)
    (string->symbol
     (string-append name-prefix (number->string n)))))

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


;;---------------------- composition ----------------------

(struct stream (head tail) #:transparent)
(struct thunk (func) #:transparent)

(define-syntax delay
  (syntax-rules ()
    [(_ e ...)
     (thunk (lambda () e ...))]))

(define force
  (lambda (th)
    (match th
      [(thunk f) (f)]
      [_ th])))

(define bind
  (lambda (g v)
    (match v
      ['() '()]
      [(thunk _)
       (delay (bind g (force v)))]
      [(stream head tail)
       (mplus (g head) (delay (bind g tail)))])))

(define bind*
  (lambda (v . gs)
    (foldl bind v gs)))

(define mplus
  (lambda (v f)
    (match v
      ['() f]
      [(thunk _)
       (delay (mplus f (force v)))]
      [(stream head tail)
       (stream head (delay (mplus f tail)))])))

(define-syntax mplus*
  (syntax-rules ()
    [(_ e) e]
    [(_ e0 e ...)
     (mplus e0 (delay (mplus* e ...)))]))


;;------------------- goal constructors -------------------

(define succeed
  (lambda (s) (stream s '())))

(define fail
  (lambda (s) '()))

(define ==
  (lambda (u v)
    (lambda (s)
      (delay
        (let ([s (unify u v s)])
          (if s (succeed s) (fail s)))))))

(define-syntax exist
  (syntax-rules ()
    [(_ (x ...) g0 g ...)
     (lambda (s)
       (delay
         (let ([x (var 'x)] ...)
           (bind* (succeed s) g0 g ...))))]))

(define-syntax conde
  (syntax-rules ()
    [(_ [g0 g ...]
        [g1 g^ ...] ...)
     (lambda (s)
       (delay
         (mplus*
          (bind* (succeed s) g0 g ...)
          (bind* (succeed s) g1 g^ ...) ...)))]))

(define-syntax trace
  (syntax-rules ()
    [(_ msg v ...)
     (lambda (s)
       (printf "~a~a: ~a~n" msg 'v (reify v s)) ...
       (succeed s))]))


;;----------------------- top level -----------------------

(define take
  (lambda (n v)
    (cond
      [(zero? n) '()]
      [else
       (match v
         ['() '()]
         [(thunk _)
          (take n (force v))]
         [(stream head tail)
          (cons head (take (- n 1) tail))])])))

(define do-display #f)

(define display-code
  (lambda (v)
    (set! do-display v)))

(define debug-display
  (lambda (n contents)
    (cond
      [do-display
       (display "-----------------------------------\n")
       (if (= n +inf.0)
           (pretty-print `(run* ,@contents))
           (pretty-print `(run ,n ,@contents)))]
      [else (void)])))

(define-syntax run
  (syntax-rules ()
    [(_ n (x) g0 g ...)
     (begin
       (debug-display n '((x) g0 g ...))
       (let ([top-g (exist (x)
                      g0 g ...
                      (lambda (s)
                        (succeed (reify x s))))])
         (take n (delay (top-g empty-s)))))]))

(define-syntax run*
  (syntax-rules ()
    [(_ (x) g ...)
     (run +inf.0 (x) g ...)]))
