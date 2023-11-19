#lang racket/base
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
  
(provide mini-prolog-lexer)
  
(define mini-prolog-lexer
  (lexer
   [(:: alphabetic (:* (:or alphabetic numeric #\_)))
    (values lexeme 'symbol #f (position-offset start-pos) (position-offset end-pos))]
   [(:or #\( #\) #\, #\. #\, (:seq #\: #\-))
    (values lexeme 'parens #f (position-offset start-pos) (position-offset end-pos))]
   [(:+ whitespace)
    (values lexeme 'white-space #f (position-offset start-pos) (position-offset end-pos))]
   [(:: "%" (:* (:~ #\newline)) "\n")
    (values lexeme 'comment #f (position-offset start-pos) (position-offset end-pos))]
   [(special)
    (values "" 'no-color #f (position-offset start-pos) (position-offset end-pos))]
   [(special-comment)
    (values "" 'comment #f (position-offset start-pos) (position-offset end-pos))]
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
                  ("(" parens)
                  ("koper" symbol)
                  ("," parens)
                  ("ljubljana" symbol)
                  (")" parens)
                  ("." parens)))
  (check-equal? (lex "a(X,Y) :- b(Y,X).")
                '(("a" symbol)
                  ("(" parens)
                  ("X" symbol)
                  ("," parens)
                  ("Y" symbol)
                  (")" parens)
                  (" " white-space)
                  (":-" parens)
                  (" " white-space)
                  ("b" symbol)
                  ("(" parens)
                  ("Y" symbol)
                  ("," parens)
                  ("X" symbol)
                  (")" parens)
                  ("." parens)))
  (check-equal? (lex "% a(X,Y) :- b(Y,X).\n")
                '(("% a(X,Y) :- b(Y,X).\n" comment))))
