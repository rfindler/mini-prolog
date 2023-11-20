#lang racket
(module reader racket (require "reader.rkt") (provide (all-from-out "reader.rkt")))
(require (for-syntax syntax/parse syntax/id-table racket/dict))

(provide %-
         (rename-out [mini-prolog-module-begin #%module-begin]
                     [mini-prolog-top-interaction #%top-interaction]))
(define-syntax (%- stx)
  (raise-syntax-error '%- "must be used inside prolog" stx))

(begin-for-syntax
  
  (define-syntax-class decl
    #:literals (%-)
    #:attributes ((conclusion 0)
                  (conclusion.fname 0)
                  (conclusion.arg 1)
                  (premise 1))
    (pattern (%- conclusion:clause premise:clause ...)))

  (define-syntax-class clause
    #:attributes ((fname 0)
                  (arg 1))
    (pattern (fname:id arg:lv ...)))

  (define-syntax-class lv
    #:attributes ((x 0)
                  (var? 0))
    (pattern x:id
      #:when (regexp-match? #rx"^[A-Z]" (symbol->string (syntax-e #'x)))
      #:attr var? #t)
    (pattern x:id
      #:when (regexp-match? #rx"^[a-z]" (symbol->string (syntax-e #'x)))
      #:attr var? #f))
  )

(define-syntax (mini-prolog-module-begin stx)
  (syntax-parse stx
    [(_ d:decl ...)
     (define decls (make-free-id-table))
     (for ([del (in-list (syntax->list #'(d ...)))])
       (syntax-parse del
         [d:decl
          (define id #'d.conclusion.fname)
          (free-id-table-set! decls id
                              (cons #'d
                                    (free-id-table-ref decls id '())))]))
     (for/list ([id (in-list (dict-keys decls))])
       (free-id-table-set! decls id (reverse (dict-ref decls id))))
     #`(#%plain-module-begin
        #,@(for/list ([(id decl) (in-dict decls)])
             #`(relation #,id #,@decl)))]))

(define-syntax (relation stx)
  (syntax-parse stx
    [(_ name:id d:decl ...)
     (define arg-count #f)
     (define first-args #f)
     (for ([args (in-list (syntax->list #'((d.conclusion.arg ...) ...)))])
       (cond
         [arg-count
          (unless (= (length (syntax->list args)) arg-count)
            (raise-syntax-error (syntax-e #'name) "arity mismatch"
                                #'name
                                #f
                                (append first-args (syntax->list args))))]
         [else
          (set! arg-count (length (syntax->list args)))
          (set! first-args (syntax->list args))]))
     #`(begin
         (define-syntax (name stx)
           (syntax-parse stx
             [(name2 args (... ...))
              (unless (= (length (syntax->list #'(args (... ...)))) #,arg-count)
                (raise-syntax-error 'name2 "wrong arity" stx))
              #'(the-actual-function 'env 'succ 'fail args (... ...))]))
         (define (the-actual-function env succ fail x y)
           5))]))

(define-syntax (mini-prolog-top-interaction stx)
  (syntax-parse stx
    [(_ . stuff)
     #'(printf "query ~s\n" 'stuff)]))

(module+ main
  (mini-prolog-module-begin
   (%- (train koper ljubljana))
   (%- (bus koper ljubljana))
   (%- (bus ljubljana maribor))
   (%- (bus ljubljana kocevje))
   (%- (train kranj bled))
   (%- (bus bled bohinj))
   (%- (connection1 X Y) (train X Y))
   (%- (connection1 X Y) (bus X Y))
   (%- (connection X Y) (connection1 X Y))
   (%- (connection X Y) (connection1 X Z) (connection Z Y))))
