#lang racket
(module reader racket (require "reader.rkt") (provide (all-from-out "reader.rkt")))
(require (for-syntax syntax/parse))

(provide %- (rename-out [mini-prolog-module-begin #%module-begin]))
(define-syntax (%- stx)
  (raise-syntax-error '%- "must be used inside prolog" stx))

(begin-for-syntax
  
  (define-syntax-class decl
    #:literals (%-)
    (pattern (%- conclusion:clause premise:clause ...)))

  (define-syntax-class clause
    (pattern (fname:id arg:lv ...)))

  (define-syntax-class lv
    (pattern x:id
      #:when (regexp-match? #rx"^[A-Z]" (symbol->string (syntax-e #'x))))
    (pattern x:id
      #:when (regexp-match? #rx"^[a-z]" (symbol->string (syntax-e #'x)))))
  )

(define-syntax (mini-prolog-module-begin stx)
  (syntax-parse stx
    [(_ d:decl ...)
     #'(#%plain-module-begin (+ 1 2))]))

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
