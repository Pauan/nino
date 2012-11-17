#lang racket/base

;; TODO: unsafe versions of math fl stuff

(provide (all-defined-out))

(require racket/flonum)
(require (except-in racket/unsafe/ops
                    unsafe-unbox*
                    unsafe-set-box*!
                    unsafe-box*-cas!))

(namespace-require 'racket/base)


(define box               make-placeholder)
(define unsafe-set-box*!  placeholder-set!)


;;
(define box-immutable     make-placeholder)
(define box?              placeholder?)
(define unsafe-unbox*     placeholder-get)

(define (unsafe-box*-cas! x o n)
  (placeholder-set! x n))


; TODO: code duplication with 01 nulan.rkt

; Generic equality predicate, based on egal
; http://home.pipeline.com/~hbaker1/ObjectIdentity.html
(define (is? x y)
  (if (number? x)
      (if (number? y)
          (= x y)
          #f)
      (if (immutable? x)
          ; TODO: do I need to check if y is immutable too?
          (equal? x y)
          ; need to use eqv? for characters
          (eqv? x y))))

; TODO: can probably use unsafe-car and unsafe-cdr
(define (hash-remove* x . a)
  (let loop ((x  x)
             (a  a))
    (if (null? a)
        x
        (loop (hash-remove x (car a))
              (cdr a)))))

(define (print x f)
  (cond ((box? x)
          (display "#<box")
          (let ((x (unsafe-unbox* x)))
            (when x
              (display " ")
              (print x f)))
          (display ">"))
        ((pair? x)
          (display "(")
          (let loop ((x x))
            (if (pair? x)
                (begin (print (unsafe-car x) f)
                       (when (pair? (unsafe-cdr x))
                         (display " "))
                       (loop (unsafe-cdr x)))
                (display ")"))))
        ((integer? x)
          (f (inexact->exact x)) ; TODO: fl->exact-integer
          )
        (else
          (f x)
          #|(let ((n (object-name x)))
            (if n
                (f n)
                (f x)))|#
          )))

(define (evalable? x)
  (symbol? x))

(define (pattern-car expected)
  (lambda (x)
    (if (pair? x)
        (unsafe-car x)
        (error (format "invalid pattern: expected ~s but got" expected) x))))


(define env     (make-parameter (hash)))

(struct type () #:transparent)

; (depends type)
(define %call   (type))
(define %macex  (type))
(define %f      #f)
(define %t      #t)

(define (update b . args)
  (b (apply hash-set* (b) args)))


; (depends print)
; TODO: rewrite to use unsafe-car
(define (pr . args)
  (let loop ((x args))
    (when (pair? x)
      (print (unsafe-car x) display)
      (loop (unsafe-cdr x))))
  (car args))

; (depends pr)
; TODO: rewrite to use unsafe-car
(define (prn . args)
  (apply pr args)
  (newline)
  (newline)
  (car args))

; (depends %macex)
(define & (hash %macex (lambda (x) x)))

; (depends %macex)
(define _quote
  (hash %macex (lambda (x) `',x)))

; (depends %call)
(define (call f . a)
  (let loop ((f f))
    (if (procedure? f)
        (apply f a)
        (loop (hash-ref f %call)))))

; (depends update env)
; TODO: rewrite to use unsafe-car and unsafe-cdr
(define (env-def . a)
  (let loop ((a a))
    (unless (null? a)
      (update env (car a) (box-immutable (cadr a)))
      (loop (cddr a)))))

; (depends env)
(define (lookup x)
  (hash-ref (env) x (lambda () (error "undefined variable:" x))))

; (depends lookup)
(define (lookup-val x)
  (if (symbol? x)
      (let ((x (lookup x)))
        (if (box? x)
            (unsafe-unbox* x)
            x))
      x))

; (depends %macex lookup-val)
(define (macex? x)
  (let ((x (lookup-val x)))
    (if (and (hash? x)
             (hash-has-key? x %macex))
        (hash-ref x %macex)
        #f)))

; (depends macex?)
(define (mapper ftop x)
  ; TODO: rewrite to use unsafe-car and unsafe-cdr
  (let ((f (macex? (car x))))
    (if f
        (apply f (cdr x))
        (let ((x (map ftop x)))
          (if (procedure? (car x))
              x
              (cons call x))))))


; (&compile (list dict %macex 5))
; (&compile (list var 'a %macex))
; (&compile (list dict 5 (uniq))) -> error
; (list dict %macex 5)
; (var foo (uniq)) foo

#|
{let u 5 u} -> (let ((u 5)) u)

{dict %macex 5} -> (dict (quote u) 5)

(uniqs {%macex %foo %bar}
  {list %macex %foo %bar}) -> (list (quote u) (quote u) (quote u))
|#

; (depends lookup mapper evalable? env)
(define (compile x)
  (cond ((symbol? x)
          (let ((x (lookup x)))
            (if (box? x)
                (if #t ;(immutable? x)
                    (let ((y (unsafe-unbox* x)))
                      (if (evalable? y)
                          `',y
                          y))
                    (list unsafe-unbox* x))
                x)))
        ((pair? x)
          (mapper compile x))
        ((number? x)
          (real->double-flonum x))
        (else x)))

; (depends %macex lookup compile)
(define _set!
  (hash %macex (lambda (n v)
    (let ((b  (lookup n))
          (u  (gensym)))
      `(let ((,u ,(compile v)))
         ,(if (box? b)
              (if (immutable? b)
                  (error "cannot use set! on immutable variable:" n)
                  ; TODO: this is probably safe, but I should still test it
                  `(,unsafe-set-box*! ,b ,u))
              `(set! ,b ,u))
         ,u)))))

; (depends %macex compile)
(define do
  (hash %macex (lambda args
    `(begin ,@(map compile args)))))

; (depends %macex update env compile)
(define (box-maker f)
  (hash %macex (lambda (n [v %f])
    (let ((b  (f #f))
          (u  (gensym)))
      (update env n b)
      `(let ((,u ,(compile v)))
         (,unsafe-box*-cas! ,b ,#f ,u)
         ;(,unsafe-set-box*! ,b ,u)
         ,u)))))

; (depends box-maker)
(define var   (box-maker box))
(define const (box-maker box-immutable))

; (depends %macex %f compile)
; TODO: can probably use unsafe-car and unsafe-cdr
(define _if
  (hash %macex (lambda a
    (let loop ((a a))
      (cond ((null? a)
              %f)
            ((null? (cdr a))
              (compile (car a)))
            (else
              `(if ,(compile (car a))
                   ,(compile (cadr a))
                   ,(loop (cddr a)))))))))

; (depends %macex update env compile)
(define _let
  (hash %macex (lambda (n v . body)
    (update env n n)
    `(let ((,n ,(compile v))) ,@(map compile body)))))

; (depends _if)
(define (pattern-match-if test expected got body)
  (list _if test
            body
            (list error (format "invalid pattern: expected ~s but got" expected) got)))

; (depends pattern-match-if is?)
(define (pattern-match-is x u body)
  (pattern-match-if (list is? u x) x u body))

; (mutual pattern-match)
; (depends _let update env)
(define (pattern-match1 x u body)
  (if (pair? x)
      (let ((v (gensym)))
        (update env v v)
        (list _let v u (pattern-match x v body)))
      (pattern-match x u body)))

; (mutual pattern-match1)
; (depends _let _quote pattern-car pattern-match-if pattern-match-is)
(define (pattern-match x u body)
  (cond ((symbol? x)
          (list _let x u body)
          #|(let ((b (box #f)))
            (update env x b)
            (displayln x)
            (displayln (lookup (cadr u)))
            (list do (list unsafe-box*-cas! b #f u)
                     body))|#
        )
        ((pair? x)
          (cond ((eq? (car x) 'list)
                  ; TODO: can probably use unsafe-car and unsafe-cdr
                  (let loop ((x (cdr x)))
                    (if (null? x)
                        body
                        (pattern-match1 (car x) (list (pattern-car x) u)
                          (pattern-match u
                            (list unsafe-cdr u)
                            (if (null? (cdr x))
                                (pattern-match-if (list null? u)
                                  null
                                  u
                                  (loop (cdr x)))
                                (loop (cdr x))))))))
                ; TODO: can probably use unsafe-car and unsafe-cdr
                ((eq? (car x) 'dict)
                  (let loop ((x (cdr x)))
                    (cond ((null? x)
                            body)
                          ((null? (cdr x))
                            (error "invalid pattern:" (car x)))
                          (else
                            (pattern-match1 (cadr x)
                              ;; TODO: replace hash-ref with get
                              (list hash-ref u (car x))
                              (loop (cddr x)))))))
                ((eq? (car x) 'quote)
                  (pattern-match-is (list _quote (cadr x)) u body)) ; TODO: a bit clunky
                (else (error "invalid pattern:" x))))
        (else
          (pattern-match-is x u body))))


; (depends %macex update env compile pattern-match do)
(define fn
  (hash %macex (lambda (args . body)
    (parameterize ((env (env)))
      (let ((u (gensym)))
        (update env u u)
        `(lambda ,u ,(compile (pattern-match args u (cons do body)))))
))))

#|
(eval `(set-box! ,(box 5) 10))

(eval `(placeholder-set! ,(make-placeholder 5) 10))

(eval `(,(make-parameter 5) 10))
|#

; TODO: this is only needed because Racket's foldl is busted
(define (reduce init f args)
  (let loop ([init  init]
             [args  args])
    (if (null? args)
        init
        (loop (f init (car args))
              (cdr args)))))

(define (0-or-more f i)
  (case-lambda
    [(x y) (f x y)]
    [args  (reduce i f args)]))

(define (1-or-more f)
  (case-lambda
    [(x y)      (f x y)]
    [(x . args) (reduce x f args)]))

; TODO: name isn't quite right
(define (2-or-more f i)
  (case-lambda
    [(x)        (f i x)]
    [(x y)      (f x y)]
    [(x . args) (reduce x f args)]))


(env-def
  ;; Macros
  'if         _if
  'do         do
  'fn         fn
  'var        var
  'const      const
  'set!       _set!
  '&          &
  'quote      _quote

  'let        _let

  ;; Functions
  '&compile   compile
  'eval       eval
  'uniq       gensym
  'type       type

  'dict       hash
  'get        hash-ref
  'has        hash-has-key?
  'set        hash-set*
  'rem        hash-remove*

  'is         is?

  'list       list

  'pr         pr
  'prn        prn

  '+          (0-or-more fl+ 0.0)
  '*          (0-or-more fl* 1.0)
  '-          (2-or-more fl- 0.0)
  '/          (1-or-more fl/)
  'min        (1-or-more flmin)
  'max        (1-or-more flmax)
  'abs        flabs
  'mod        modulo

  ;; Other
  '%f         %f
  '%t         %t
  '%call      %call
  '%macex     %macex
)


; TODO: code duplication with 01 nulan.rkt

;; Stuff for the "nulan" executable
(define (nu-eval-string s)
  (eval (compile (read (open-input-string s)))))

(define (nu-eval-file s)
  ;; This is so that it's possible to retrieve the column/line of an input port
  (parameterize ((port-count-lines-enabled #t))
    (call-with-input-file s
      (lambda (p)
        (let loop ()
          (let ((x (read p)))
            (unless (eof-object? x)
              (eval (compile x))
              (loop))))))))

(define (nu-repl)
  ;; http://arclanguage.org/item?id=10344
  (let ((interactive (terminal-port? (current-input-port))))
    (when interactive
      (namespace-require 'readline/rep-start))

    (let loop ()
      ;; This causes Ctrl+C to return to the REPL, rather than aborting.
      ;; Technique was taken from Racket's (read-eval-print-loop) which
      ;; I found in /usr/share/racket/collects/racket/private/misc.rkt
      (call-with-continuation-prompt
        (lambda ()
          ;; http://arclanguage.org/item?id=10344
          (let* ((it    (if interactive
                            ((current-prompt-read))
                            (read)))
                 (expr  (if (syntax? it)
                            (syntax->datum it)
                            it)))
            (if (eof-object? expr)
                (when interactive (newline))
                (begin (print (eval (compile expr)) write)
                       (newline)
                       (when interactive (newline))
                       ;; Abort to loop. (Calling `repl` directly would not be a tail call.)
                       (abort-current-continuation (default-continuation-prompt-tag))))))
        (default-continuation-prompt-tag)
        (lambda _ (loop))))))
