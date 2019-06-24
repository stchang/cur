#lang racket
(require (for-syntax crypto "pubkey.rkt" syntax/parse racket/syntax))
(define-syntax require/verify
  (syntax-parser
    [(_ lib name publickey)
     #'(begin
         (require (only-in lib name))
         (begin-for-syntax
           (define signed
             (cadr ; drop "quote"
              (syntax->datum
               (with-handlers
                 ([exn?
                   (λ (e) (error 'require/verify
                                 "err while verifying ~a" 'name))])
                 (local-expand #'(name verify) 'expression null)))))
           #;             (displayln signed)
           (unless (digest/verify publickey 'sha1
                                  (symbol->string (syntax->datum #'name))
                                  (list->bytes signed))
             (error 'require/verify
                    "can't verify ~a with key:\n~a"
                    'name (pk-key->datum publickey 'rkt-public)))))]))
             

(require/verify "provide-sign.rkt" + pubkey)
(+ 1 2)
(+ 3 4 5)
+
#;(require/verify "provide-sign.rkt" racket+ pubkey)
