#lang racket
(require (for-syntax crypto "privkey.rkt" syntax/parse racket/syntax))
(define-syntax provide/sign
  (syntax-parser
    [(_ name privatekey)
     #:with name/sign (format-id #'name "~a/sign" #'name)
     #'(begin
         (define-syntax name/sign
           (syntax-parser
             [(_ (~literal verify))
              #:do[(define bs
                     (bytes->list
                      (digest/sign
                       privatekey 'sha1
                       (symbol->string (syntax->datum #'name)))))
                   #;(displayln bs)]
              #`(quote #,bs)]
             [:id #'name]
             [(_ . args) #'(name . args)]))
         (provide (rename-out [name/sign name])))]))
(provide/sign + privkey)
(provide (rename-out [+ racket+]))
