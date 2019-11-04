#lang s-exp "../main.rkt"

; experimental cur library utilizing external (z3) solver
; uses Rosette to translate to smt-lib
(require syntax/parse/define
         (for-syntax syntax/parse
                     racket/pretty)
         (only-in turnstile/base make-variable-like-transformer assign-type)
         cur/stdlib/nat
         cur/stdlib/bool
         cur/stdlib/pair
         cur/stdlib/equality
         (only-in cur/stdlib/axiom im-an-axiom)
         (only-in racket/base module)
         (for-syntax
          syntax/stx
          macrotypes/stx-utils
          (prefix-in ro: rosette)))

(provide define-axiom/z3
         print-z3-assumptions
         (all-from-out cur/stdlib/nat
                       cur/stdlib/bool
                       cur/stdlib/pair
                       cur/stdlib/equality))

(define-syntax-parser define-axiom/z3
  [(_ name:id τ)
   #:with ro-term #`(ro:verify
                     (ro:assert
                      #,(τ->ro ((current-type-eval) #'τ))))
;   #:do[(pretty-print (stx->datum #'ro-term))]
   ;; #:do[(displayln (eval #'ro-term))]
   #:do[(define ro-res (eval #'ro-term))]
   #:fail-unless (ro:#%app ro:unsat? ro-res)
                 (format "Rosette could not prove ~a, counterexample: ~a\n"
                         (stx->datum #'τ) ro-res)
   #'(define-syntax name
       (make-variable-like-transformer
        (assign-type #`(#%plain-app
                        #,(assign-type #'(#%plain-app im-an-axiom 'name) #'τ)
                        #,(assign-type #'(#%plain-app im-a-z3-axiom 'name) #'τ))
                     #'τ)))])

(module m racket/base

  (provide print-z3-assumptions im-a-z3-axiom)
  
  (require (for-syntax racket/base syntax/parse)
           syntax/parse/define
           turnstile/base
           turnstile/typedefs)
  
  ;; enable distinguishing z3 axioms from regular axioms
  (define ((im-a-z3-axiom name) . _)
    (error name "encountered z3 axiom during evaluation"))

  (begin-for-syntax
    (define-syntax-class z3-axiom
      #:attributes (name)
      [pattern ({~literal #%plain-app}
                {~literal im-a-z3-axiom}
                ({~literal quote} name:id))])

    ;; syntax -> [hash symbol => type]
    (define (find-all-z3-axioms stx)
      (let find ([axioms (hasheq)]
                 [stx stx])
        (syntax-parse stx
          [a:z3-axiom
           (hash-update axioms
                        (syntax-e #'a.name)
                        values
                        (λ ()
                          (resugar-type (typeof stx))))]
          [(stuff ...)
           (for/fold ([ax axioms]) ([stx (in-list (attribute stuff))])
             (find ax stx))]
          [_ axioms]))))

  (define-syntax-parser print-z3-assumptions
    [(_ expr)
     #:with [(axiom-name . axiom-type) ...]
     (hash->list (find-all-z3-axioms (expand/df #'expr)))
     #'(begin
         (printf "Z3 Axioms used by \"~s\":\n" 'expr)
         (printf " - ~s : ~s\n" 'axiom-name 'axiom-type)
         ...)]))
(require 'm)

(begin-for-syntax
  ;; converts cur to rosette (which then translates to z3)
  (define τ->ro
    (syntax-parser
;      [debug #:do[(printf "τ->ro ~a\n" (syntax->datum #'debug))] #:when #f #'debug]
      [~Nat #'ro:integer?]
      [~Bool #'ro:boolean?]
      [(~Π [x : τ] body)
       #:when (syntax-property #'x 'tmp)
       #`(ro:implies #,(τ->ro #'τ) #,(τ->ro #'body))]
      [(~Π [x : ~Nat] body) ; nat case
       #`(ro:let ()
          (ro:define-symbolic x ro:integer?) ; rosette has no nats
          (ro:implies ; so need to restrict ints
           (ro:or (ro:#%app ro:zero? x) (ro:#%app ro:positive? x))
           #,(τ->ro #'body)))]
      [(~Π [x : (~prod t1 t2)] body) ; pair
       #:with (X Y) (generate-temporaries #'(t1 t2))
       #`(ro:let ()
          (ro:define-symbolic X #,(τ->ro #'t1))
          (ro:define-symbolic Y #,(τ->ro #'t2))
          (ro:define x (ro:#%app ro:cons X Y))
          #,(τ->ro #'body))]
      [(~Π [x : (~Π [_ : τ] body1)] body) ; fn case
       #:with (τro ...) (stx-map τ->ro #'(τ body1))
       #`(ro:let ()
          (ro:define-symbolic x (ro:#%app ro:~> τro ...))
          #,(τ->ro #'body))]
      [(~Π [x : τ] body)
       #`(ro:let ()
          (ro:define-symbolic x #,(τ->ro #'τ))
          #,(τ->ro #'body))]
      [(~== ty a b) #`(ro:#%app ro:equal? #,(τ->ro #'a) #,(τ->ro #'b))]
      [true #'ro:true]
      [false #'ro:false]
      [z #'(ro:#%datum . 0)]
      [(s n) #`(ro:#%app ro:+ #,(τ->ro #'n) (ro:#%datum . 1))]
      ;; nat ops
      [(~literal #%plain-app)
       #:with (~literal plus) (syntax-property this-syntax 'display-as)
       #'ro:+]
      [(~literal #%plain-app)
       #:with (~literal minus) (syntax-property this-syntax 'display-as)
       #'ro:-]
      [(~literal #%plain-app)
       #:with (~literal mult) (syntax-property this-syntax 'display-as)
       #'ro:*]
      [(~literal #%plain-app)
       #:with (~literal nat-equal?) (syntax-property this-syntax 'display-as)
       #'ro:=]
      [(~literal #%plain-app)
       #:with (~literal <=) (syntax-property this-syntax 'display-as)
       #'ro:<=]
      [(~literal #%plain-app)
       #:with (~literal even?) (syntax-property this-syntax 'display-as)
       #'ro:even?]
      ;; bool ops
      [(~literal #%plain-app)
       #:with (~literal not) (syntax-property this-syntax 'display-as)
       #'ro:not]
      [(~literal #%plain-app)
       #:with (~literal or) (syntax-property this-syntax 'display-as)
       #'ro:or]
      [(~literal #%plain-app)
       #:with (~literal and) (syntax-property this-syntax 'display-as)
       #'ro:and]
      ;; pairs
      [(pair x y) #`(ro:#%app ro:cons #,(τ->ro #'x) #,(τ->ro #'y))]
      [((~and (~literal #%plain-app) pa) _ _ x)
       #:with (~literal fst*) (syntax-property #'pa 'display-as)
       #`(ro:#%app ro:car #,(τ->ro #'x))]
      [((~and (~literal #%plain-app) pa) _ _ x)
       #:with (~literal snd*) (syntax-property #'pa 'display-as)
       #`(ro:#%app ro:cdr #,(τ->ro #'x))]
      [(~literal #%plain-app) #'ro:#%app]
      [(x ...) (stx-map τ->ro #'(x ...))]
      [_ this-syntax])))
