#lang racket

;;
;; Run-of-the mill metacircular evaluator for (a small subset of) scheme.
;; Includes delimited continuations, and the tower of (positive) meta-levels.
;;

(struct closure [vars body environment])
(struct primitive [procedure])

;; env boilerplate
;;

(define symbol-not-found (gensym))
(define (symbol-found? x) (not (eq? x symbol-not-found)))

(define (empty-environment) (box #hasheq()))

(define (extend-environment env . assn)
  (box (for/fold ([env (unbox env)])
                 ([ass (in-list assn)] #:when #t [kv (in-list ass)])
    (hash-set env (car kv) (box (cdr kv))))))

(define (lookup-environment env sym)
  (cond [(hash-ref (unbox env) sym #f) => unbox]
        [else symbol-not-found]))

(define (set-environment! env sym val)
  (cond [(hash-ref (unbox env) sym #f)
         => (lambda (cell) (set-box! cell val))]
        [else symbol-not-found]))

(define (define-environment! env sym val)
  (set-box! env (hash-set (unbox env) sym (box val))))

(define (call-if-found result sym k)
  (if (symbol-found? result) (k result)
    (error 'scheme "undefined symbol: ~s" sym)))

;; evaluator + macro-expander
;;

(define (eval/k expr env k)
  (match expr
    [(? symbol? sym)
     (call-if-found
       (lookup-environment env sym) sym k)]
    [(? atom? expr) (k expr)]
    [`(quote ,expr) (k expr)]
    [`(if ,p ,c ,a)
      (eval/k p env
        (lambda (x) (if x (eval/k c env k) (eval/k a env k))))]
    [`(lambda ,vars . ,body) (k (closure vars body env))]
    [`(set! ,sym ,expr)
      (eval/k expr env
        (lambda (x) (call-if-found
                      (set-environment! env sym x) sym k)))]
    [`(define ,sym ,expr)
     (eval/k expr env
       (lambda (x) (k (define-environment! env sym x))))]
    [`(reset ,expr) (k (eval/k expr env identity))]
    [`(shift ,expr)
     (eval/k expr env
       (lambda (f) (apply/k f `(,(primitive k)) identity)))]
    [`(,f . ,args)
     (eval/k f env
       (lambda (f) (map-k (lambda (x k) (eval/k x env k)) args
                          (lambda (args) (apply/k f args k)))))]))

(define (apply/k proc args k)
  (match proc
    [(primitive f)
     (or (procedure-arity-includes? f (length args))
         (arity-error f args))
     (k (apply f args))]
    [(closure vars body env)
     (let ([env+ (args->env env vars args)])
       (fold-left-k (lambda (_ expr k) (eval/k expr env+ k))
                    (void) body k))]
    [f (error 'apply/k "not a procedure: ~s" f)]))

(define (args->env env vars args)
  (extend-environment env
    (let add ([v vars] [a args])
      (match* (v a)
        [(`(,v . ,vs) `(,a . ,as)) `((,v . ,a) . ,(add vs as))]
        [((? symbol? s) as) `((,s . ,as))]
        [('() '()) '()]
        [(_ _) (arity-error vars args)]))))

(define (arity-error x args)
  (error 'apply "arity mismatch: ~s ~s~n" x args))

(define (expand/tower expr tower)
  (define (expand/meta expr) (expand/tower expr (stream-rest tower)))
  (define-values (env menv) (car+cdr (stream-first tower)))

  (let expand/go ([expr expr] [env env] [menv menv])
    (match expr
      [`(lambda ,vars . ,body)
        (let ([env+ (extend-environment env)]
              [menv+ (extend-environment menv)])
          `(lambda ,vars .
             ,(map (lambda (e) (expand/go e env+ menv+)) body)))]
      [`(define-syntax ,sym ,expr)
        (define-environment! menv sym
          (eval/k (expand/meta expr) env identity)) (void)]
      [`(begin-for-syntax . ,body)
        (fold-left-k (lambda (_ e k) (eval/k (expand/meta e) env k))
                     #f body void)]
      [`(quote ,expr) `(quote ,expr)]
      [(and `(,sym . ,_) expr)
       (let ([macro (lookup-environment menv sym)])
         (if (symbol-found? macro)
           (expand/go (apply/k macro `(,expr) identity) env menv)
           (map (lambda (e) (expand/go e env menv)) expr)))]
      [a a])))

(define (expand code #:tower     [tower #f]
                     #:make-env  [env default-environment]
                     #:make-menv [menv default-macro-environment])
  (expand/tower code
    (or tower (let build () (stream-cons (cons (env) (menv)) (build))))))

(define (eval code [env (default-environment)] [tower #f])
  (eval/k (expand code #:tower tower) env identity))

;; bleh
;;

(define (fold-left-k f a l k)
  (if (null? l) (k a)
    (f a (car l) (lambda (x) (fold-left-k f x (cdr l) k)))))

(define (map-k f l k)
  (if (null? l) (k '())
    (f (car l) (lambda (x)
                 (map-k f (cdr l) (lambda (xs) (k (cons x xs))))))))

(define (atom? x)
  (not (or (pair? x) (null? x))))

(define (car+cdr x) (values (car x) (cdr x)))


;; boot code, builtins and tests
;;

(define-syntax pack-primitives
  (syntax-rules ()
    [(_) '()]
    [(_ (name defn) p ...)
     (cons `(name . ,(primitive defn)) (pack-primitives p ...))]
    [(_ name p ...)
     (cons `(name . ,(primitive name)) (pack-primitives p ...))]))

(define-syntax pack-definitions
  (syntax-rules ()
    [(_) '()]
    [(_ (name defn) p ...)
     (cons `(name . ,(eval/k 'defn (empty-environment) identity))
           (pack-definitions p ...))]))

(define (default-environment)
  (extend-environment (empty-environment)
    (pack-primitives + - * / add1 sub1 = < > <= >=)
    (pack-primitives eq?)
    (pack-primitives cons car cdr null? pair?)
    (pack-primitives
      (p (lambda (x) (printf "[p] ~s~n" x))))
    (pack-definitions
      (call/cc (lambda (f)
                 (shift (lambda (cc)
                          (cc (f (lambda (x)
                                   (shift (lambda (_) (cc x))))))))))
      (list (lambda list list))
      (not (lambda (x) (if x #f #t)))
      (void (lambda ()))
      (identity (lambda (x) x)))
    ))

(define (default-macro-environment)
  (define (bad-syntax sym) (error sym "bad syntax"))
  (define (let-args? vars)
    (and (list? vars)
         (for/and ([v vars])
                  (and (list? v) (= (length v) 2) (symbol? (car v))))))
  (define-syntax-rule (macro clause ...)
                      (lambda (expr) (match (cdr expr) clause ...)))

  (extend-environment (empty-environment)
    (pack-primitives
      (begin (macro [body `((lambda () . ,body))]))
      (letrec (macro
                [`(,(? let-args? vars) . ,body)
                  `(let ,(map (lambda (ve) `(,(car ve) #f)) vars)
                     ,@(map (lambda (ve) `(set! ,(car ve) ,(cadr ve))) vars)
                     ,@body)]
                [_ (bad-syntax 'letrec)]))
      (let (match-lambda
             [`(let ,(? symbol? name) ,(? let-args? vars) . ,body)
              `(letrec ((,name (lambda ,(map car vars) ,@body)))
                 (,name ,@(map cadr vars)))]
             [`(let ,(? let-args? vars) . ,body)
              `((lambda ,(map car vars) ,@body)
                ,@(map cadr vars))]
             [_ (bad-syntax 'let)]))
      (cond (macro
              ['() '(void)]
              [`((else ,c) . ,_) c]
              [`((,p1 ,c1) . ,pcs) `(if ,p1 ,c1 (cond . ,pcs))]
              [`((,p1 => ,c1) . ,pcs)
                `((lambda (x) (if x (,c1 x) (cond . ,pcs))) ,p1)]))
      (and (macro
             ['() #t] [`(,x) x]
             [`(,x . ,xs) `(if ,x (and . ,xs) #f)]))
      (or (macro
            ['(or) #f] [`(or ,x) x]
            [`(or ,x . ,xs) `((lambda (x) (if x x (or . ,xs))) ,x)]))
      
      )
    ))


(define code1 ;; environments and application
  '((lambda (f a b)
      (set! b (+ b 1))
      (cons (f a b) (cons a (cons b '()))))
    (lambda (x y) (+ x y))
    1 2))

(define code2 ;; varargs
  '((lambda (a . b) b) 'p 'q 'w))

(define code3 ;; continuation carving
  '(+ 1 (reset (* 2 (shift (lambda (k) 2))))))

(define code4 ;; continuation grafring
  '(+ 1 (reset (* 2 (shift (lambda (k) (k (k 2))))))))

(define code5 ;; macros, full continuations
  '(begin
     (cons (call/cc
             (lambda (cc) (cc 'win) (p "this is wrong") 2))
           3)))

(define code6 ;; conditional
  '(cons
     (if (identity #t) 1 2)
     (if (not #t) 1 2)))


(define code7 ;; macros
  '(let ([a 1])
     (+ a
        (let ([a 0]) (set! a 100) a)
        a)))

(define code8 ;; expand-time env manipulations
  '(begin
     (define-syntax wzz (lambda (syn) (foo + (cdr syn))))
     (begin-for-syntax
       (define foo (lambda (a b) (cons a b))))
     (cons
       (wzz 1 2)
       (begin
         (begin-for-syntax
           (define foo (lambda (a b) (cons b a))))
         (define-syntax wzz (lambda (syn) (foo (cdr syn) -)))
         (wzz 2 1)))))

(define code9 ;; tower of meta-levels
  '(begin
     (define-syntax doodad (lambda (syn1) (funn (cdr syn1))))
     (begin-for-syntax
       (define-syntax meta-macro (lambda (syn2) (meta-fun syn2)))
       (begin-for-syntax
         (define meta-fun
           (lambda (syn3)
             (list 'list ''quote
                   (list 'cons ''deep-magic (car (cdr syn3)))))))
       (define funn (lambda (syn4) (meta-macro syn4))))
     (doodad lurks here)))

(define code10 ;; why of why
  '((lambda (Y)
      ((Y (lambda (fib)
            (lambda (n)
              (if (<= n 1) n
                (+ (fib (- n 1)) (fib (- n 2)))))))
       20))
    (lambda (f)
      ((lambda (u)
         (f (lambda (x)
              ((u u) x))))
       (lambda (u)
         (f (lambda (x)
              ((u u) x))))))))

(define code11 ;; letrec
  '(letrec ([fib (lambda (n)
                   (if (<= n 1) n
                     (+ (fib (- n 1))
                        (fib (- n 2)))))])
     (fib 20)))

(define code12
  '(begin
     (define visit
       (lambda (sexp)
         (reset (let go ([sexp sexp])
                  (cond [(null? sexp) #f]
                        [(pair? sexp)
                         (begin (go (car sexp)) (go (cdr sexp)))]
                        [else (shift (lambda (cc)
                                       (cons sexp (lambda ()
                                                    (reset (cc #f))))))])))))
     (let loop ([res (visit
                       '(a (b . c) ((d) e . f) g))])
       (if (not res) '()
         (cons (car res) (loop ((cdr res))))))))


(define (check-eval code resp)
  (let ([result (eval code)])
    (cond [(equal? resp result) resp]
          [else 
            (printf "[check-eval] ERROR: ~s -> ~s~n" code result)
            (error 'check-eval "bad result")])))

(define (sanity-check)
  (map (lambda (ab) (apply check-eval ab))
       `((,code1 (4 1 3))
         (,code2 (q w))
         (,code3 3)
         (,code4 9)
         (,code5 (win . 3))
         (,code6 (1 . 2))
         (,code7 102)
         (,code8 (3 . 1))
         (,code9 (deep-magic lurks here))
         )))

(sanity-check)
