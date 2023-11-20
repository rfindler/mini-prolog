#lang racket/base
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
  
(provide mini-prolog-lexer)
  
(define mini-prolog-lexer
  (lexer
   [(:: alphabetic (:* (:or alphabetic numeric #\_)))
    (values lexeme 'symbol #f (position-offset start-pos) (position-offset end-pos))]
   [(:or #\( #\) #\, #\. #\, (:seq #\: #\-))
    (values lexeme 'parenthesis #f (position-offset start-pos) (position-offset end-pos))]
   [(:+ whitespace)
    (values lexeme 'white-space #f (position-offset start-pos) (position-offset end-pos))]
   [(:: "%" (:* (:~ #\newline)) "\n")
    (values lexeme 'comment #f (position-offset start-pos) (position-offset end-pos))]
   [(special)
    (values "" 'no-color #f (position-offset start-pos) (position-offset end-pos))]
   [(special-comment)
    (values "" 'comment #f (position-offset start-pos) (position-offset end-pos))]
   [(char-complement (union))
    (values lexeme 'error #f (position-offset start-pos) (position-offset end-pos))]
   [(eof)
    (values lexeme 'eof #f #f #f)]))

(module+ test
  (require rackunit)
  (define (lex str)
    (define p (open-input-string str))
    (let loop ()
      (define-values (lexeme key _1 _2 _3) (mini-prolog-lexer p))
      (cond
        [(equal? key 'eof) '()]
        [else (cons (list lexeme key) (loop))])))
  (check-equal? (lex "train(koper,ljubljana).")
                '(("train" symbol)
                  ("(" parenthesis)
                  ("koper" symbol)
                  ("," parenthesis)
                  ("ljubljana" symbol)
                  (")" parenthesis)
                  ("." parenthesis)))
  (check-equal? (lex "a(X,Y) :- b(Y,X).")
                '(("a" symbol)
                  ("(" parenthesis)
                  ("X" symbol)
                  ("," parenthesis)
                  ("Y" symbol)
                  (")" parenthesis)
                  (" " white-space)
                  (":-" parenthesis)
                  (" " white-space)
                  ("b" symbol)
                  ("(" parenthesis)
                  ("Y" symbol)
                  ("," parenthesis)
                  ("X" symbol)
                  (")" parenthesis)
                  ("." parenthesis)))
  (check-equal? (lex "% a(X,Y) :- b(Y,X).\n")
                '(("% a(X,Y) :- b(Y,X).\n" comment)))
  (check-equal? (lex "f(X) ::-")
                '(("f" symbol)
                  ("(" parenthesis)
                  ("X" symbol)
                  (")" parenthesis)
                  (" " white-space)
                  (":" error)
                  (":-" parenthesis))))
