#lang s-exp syntax/module-reader
mini-prolog

#:read mini-prolog-read
#:read-syntax mini-prolog-read-syntax
#:info mini-prolog-get-info
#:whole-body-readers? #t

(require parser-tools/lex
         parser-tools/yacc
         syntax/readerr
         (prefix-in : parser-tools/lex-sre))

(define-tokens basic-tokens (id comma elephant period open close eof))
  
(define source (make-parameter #f))
(define mp-lexer
  (lexer-src-pos
   [(:: alphabetic (:* (:or alphabetic numeric #\_))) (token-id (to-stx start-pos end-pos (string->symbol lexeme) #t))]
   ["(" (token-open (to-stx start-pos end-pos "("))]
   [")" (token-close (to-stx start-pos end-pos ")"))]
   ["," (token-comma (to-stx start-pos end-pos '|,|))]
   [":-" (token-elephant (to-stx start-pos end-pos '%-))]
   ["." (token-period (to-stx start-pos end-pos '|.|))]
   [(:: "%" (:* (:~ #\newline)) "\n") (position-token-token (mp-lexer input-port))]
   [(eof) (token-eof (to-stx start-pos end-pos eof))]
   [whitespace (position-token-token (mp-lexer input-port))]
   [(char-complement (union))
    (raise-read-error (format "could not tokenize at character `~a`" lexeme)
                      (source)
                      (position-line start-pos)
                      (position-col start-pos)
                      (position-offset start-pos)
                      (- (position-offset end-pos)
                         (position-offset start-pos)))]))

(define (to-stx start-pos end-pos val [original? #f])
  (define props (and original?
                     (read-syntax (source) (open-input-string (symbol->string val)))))
  (datum->syntax
   props
   val
   (srcloc
    (source)
    (position-line start-pos)
    (position-col start-pos)
    (position-offset start-pos)
    (- (position-offset end-pos)
       (position-offset start-pos)))
   props))

(module+ test (void))
(module+ test
  (require rackunit)
  (define (lex str)
    (define p (open-input-string str))
    (let loop ()
      (define t (mp-lexer p))
      (cond
        [(equal? (token-name (position-token-token t)) 'eof) '()]
        [else (cons
               (syntax-e (token-value (position-token-token t)))
               (loop))])))
  (check-equal? (lex "train(a,b).")
                (list 'train "(" 'a '|,| 'b ")" '|.|))
  (check-equal? (lex "r(X,X).")
                (list 'r "(" 'X '|,| 'X ")" '|.|))
  (check-equal? (lex "r(X,Y) :- r(X,Z), r(Z,Y).")
                (list 'r "(" 'X '|,| 'Y ")" '%- 'r "(" 'X '|,| 'Z ")" '|,| 'r "(" 'Z '|,| 'Y ")" '|.|)))

(define the-parser
  (parser
   [start decls]
   [end eof]
   [tokens basic-tokens]
   [error (lambda (_ name stx start end)
            (raise-read-error (format "parse error near ~a" name)
                              (syntax-source stx)
                              (syntax-line stx)
                              (syntax-column stx)
                              (syntax-position stx)
                              (syntax-span stx)))]
   [src-pos]
   [grammar
    [decls [(decl decls) (cons $1 $2)]
           [() '()]]
    [decl [(clause period) (list '%- $1)]
          [(clause elephant clauses period) (list* $2 $1 $3)]]
    [clauses [(clause comma clauses) (cons $1 $3)]
             [(clause) (list $1)]]
    [clause [(id open ids close) (cons $1 $3)]]
    [ids [(id comma ids) (cons $1 $3)]
         [(id) (list $1)]]]))

(define (mini-prolog-read-syntax source-name in)
  (parameterize ([source source-name])
    (the-parser
     (λ () (mp-lexer in)))))

(define (mini-prolog-read in)
  (syntax->datum (mini-prolog-read-syntax #f in)))

(define (mini-prolog-get-info key default default-filter)
  (case key
    [(color-lexer)
     (dynamic-require 'mini-prolog/lexer
                      'mini-prolog-lexer)]
    [else
     (default-filter key default)]))