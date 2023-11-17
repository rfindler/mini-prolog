#lang racket
(require (for-syntax syntax/parse))

(provide %-)
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

(define-syntax (prolog stx)
  (syntax-parse stx
    [(_ d:decl ...)
     #'(void)]))

(prolog
 (%- (train koper ljubljana))
 (%- (bus koper ljubljana))
 (%- (bus ljubljana maribor))
 (%- (bus ljubljana kocevje))
 (%- (train kranj bled))
 (%- (bus bled bohinj))
 (%- (connection1 X Y) (train X Y))
 (%- (connection1 X Y) (bus X Y))
 (%- (connection X Y) (connection1 X Y))
 (%- (connection X Y) (connection1 X Z) (connection Z Y)))
