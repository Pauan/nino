#lang racket/base

(provide (all-defined-out))

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

(define (hash-remove* x . a)
  (let loop ((x  x)
             (a  a))
    (if (null? a)
        x
        (loop (hash-remove x (car a))
              (cdr a)))))


(define env     (make-parameter (hash)))

(define %call   (gensym))
(define %macex  (gensym))
(define %f      #f)
(define %t      #t)

(define (update b . args)
  (b (apply hash-set* (b) args)))


(define (call f . a)
  (let loop ((f f))
    (if (procedure? f)
        (apply f a)
        (loop (hash-ref f %call)))))

(define (lookup x)
  (hash-ref (env) x (lambda () (error "undefined variable:" x))))

(define (lookup-val x)
  (let ((x (lookup x)))
    (if (box? x)
        (unsafe-unbox* x)
        x)))

(define (macex? x)
  (let ((x (lookup-val x)))
    (if (and (hash? x)
             (hash-has-key? x %macex))
        (hash-ref x %macex)
        #f)))

(define (compile x)
  (cond ((symbol? x)
          (let ((x (lookup x)))
            (if (box? x)
                (if (immutable? x)
                    (unsafe-unbox* x)
                    (list unsafe-unbox* x))
                x)))
        ((pair? x)
          (let ((f (macex? (car x))))
            (if f
                (apply f (cdr x))
                (let ((x (map compile x)))
                  (if (procedure? (car x))
                      x
                      (cons call x))))))
        (else x)))


;; Pattern matching
(define (pattern-match-is u x body)
  (list _if (list is? u x)
            body
            (list error "invalid pattern:" x)))

(define (pattern-car x)
  (if (pair? x)
      (car x)
      (error "invalid pattern:" x)))

(define (pattern-match1 u x body)
  (if (pair? x)
      (let ((v (gensym)))
        (update env v v)
        (list _let v u (pattern-match v x body)))
      (pattern-match u x body)))

(define (pattern-match u x body)
  (cond ((symbol? x)
          (update env x x)
          (list _let x u body))
        ((pair? x)
          (cond ((eq? (car x) 'list)
                  (let loop ((x (cdr x)))
                    (if (null? x)
                        body
                        (pattern-match1 (list pattern-car u) (car x)
                          (pattern-match (list cdr u) ; TODO: replace with unsafe-cdr ?
                            (if (null? (cdr x))
                                ''()
                                u)
                            (loop (cdr x)))))))
                ((eq? (car x) 'dict)
                  (let loop ((x (cdr x)))
                    (cond ((null? x)
                            body)
                          ((null? (cdr x))
                            (error "invalid pattern:" (car x)))
                          (else
                            ;; TODO: replace hash-ref with get
                            (pattern-match1 (list hash-ref u (car x))
                              (cadr x)
                              (loop (cddr x)))))))
                ((eq? (car x) 'quote)
                  (pattern-match-is u (list _quote (cadr x)) body)) ; TODO: a bit clunky
                (else (error "invalid pattern:" x))))
        (else
          (pattern-match-is u x body))))


;; Nino stuff
(define do
  (hash %macex (lambda args
    `(begin ,@(map compile args)))))

(define fn
  (hash %macex (lambda (args . body)
    (parameterize ((env (env)))
      (let ((u (gensym)))
        (update env u u)
        `(lambda ,u ,(compile (pattern-match u args (cons do body)))))
      #|(cond ((symbol? args)
              (update env args args)
              `(lambda ,args ,@(map compile body)))
            ((and (pair? args)
                  (eq? (car args) 'list))
              )
            (else (error "invalid pattern:" args)))|#
))))

(define (box-maker f)
  (hash %macex (lambda (n v)
    (let ((b  (f #f))
          (u  (gensym)))
      (update env n b)
      `(let ((,u ,(compile v)))
         (,unsafe-box*-cas! ,b ,#f ,u)
         ;(,unsafe-set-box*! ,b ,u)
         ,u)))))

(define var   (box-maker box))
(define const (box-maker box-immutable))

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

(define & (hash %macex (lambda (x) x)))

#|
(eval `(set-box! ,(box 5) 10))

(eval `(placeholder-set! ,(make-placeholder 5) 10))

(eval `(,(make-parameter 5) 10))
|#

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

(define _let
  (hash %macex (lambda (n v . body)
    ;(compile (list* do (list var n v) body))
    (update env n n)
    `(let ((,n ,(compile v))) ,@(map compile body)))))

(define _quote
  (hash %macex (lambda (x) `',x)))


(define (env-def . a)
  (let loop ((a a))
    (unless (null? a)
      (update env (car a) (box-immutable (cadr a)))
      (loop (cddr a)))))

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

  ;; Functions
  '&compile   compile

  'dict       hash
  'get        hash-ref
  'has        hash-has-key?
  'set        hash-set*
  'rem        hash-remove*

  'is         is?

  'list       list

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
                (begin (write (eval (compile expr)))
                       (newline)
                       (when interactive (newline))
                       ;; Abort to loop. (Calling `repl` directly would not be a tail call.)
                       (abort-current-continuation (default-continuation-prompt-tag))))))
        (default-continuation-prompt-tag)
        (lambda _ (loop))))))
