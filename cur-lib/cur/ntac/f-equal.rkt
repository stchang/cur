#lang cur/metantac

(require #;(for-syntax "../stdlib/equality.rkt")
         "./standard.rkt"
         "../stdlib/equality.rkt")

(begin-for-syntax

  (provide f-equal-tac)

  (define-syntax (f-equal-tac syn)
    (syntax-case syn ()
      [(_ #:with X Y f x1 x2) #'(fill (f-equal-fn #'X #'Y #'f #'x1 #'x2))]
      [:id #'(fill (f-equal-fn))]))

  ;; args must be either:
  ;; - null
  ;; - length 5: X Y f x1 x2
  (define ((f-equal-fn . args) ctxt pt)
    (match-define (ntt-hole _ goal) pt)

    (cond
      [(null? args)
       (syntax-parse goal
         [(~== Y (~and fx1 ((~literal #%plain-app) f1 x1-))
                 (~and fx2 ((~literal #%plain-app) f2 x2-)))
          #:fail-unless (free-id=? #'f1 #'f2)
          "tried to apply f-equal to application of different functions"

          (define/syntax-parse (f _) (datum->syntax goal (unexpand #'fx1)))
          (make-ntt-apply
           goal
           (list (make-ntt-hole #`(== #,(unexpand (typeof #'x1-))
                                      #,(unexpand #'x1-)
                                      #,(unexpand #'x2-))))
           (λ (pf)
             #`(f-equal #,(unexpand (typeof #'x1-))
                        #,(unexpand #'Y)
                        f
                        #,(unexpand #'x1-)
                        #,(unexpand #'x2-)
                        #,pf)))])]
      [else
       (match-define (list X Y f x1 x2) args)
       (make-ntt-apply
        goal
        (list (make-ntt-hole #`(== #,Y #,x1 #,x2)))
        (λ (pf)
          #`(f-equal #,X #,Y #,f #,x1 #,x2 #,pf)))]))

 #; (define-tactic f-equal-tac
    ; (match-define (ntt-hole _ goal) pt)
    [_
     (ntac-match
      $goal
      [(~== A x y)
       (begin
         (displayln "x and y:")
         (displayln #'x)
         (displayln #'y)
         ;(define x-norm #`#,(resugar-type (normalize #'x $ctxt)))
         ;(define y-norm #`#,(resugar-type (normalize #'y $ctxt)))
         ;(define x-norm #`#,(normalize #'x $ctxt))
         ;(define y-norm #`#,(normalize #'y $ctxt))
         (define x-norm  (normalize #'x $ctxt))
         (define y-norm  (normalize #'y $ctxt))
         (displayln "x-norm, y-norm:")
         (displayln x-norm)
         (displayln y-norm)

         (define the-xs-no-app
           (syntax-parse x-norm
             [((~literal #%plain-app) txna (... ...))
              #'(txna (... ...))]))
         (define the-ys-no-app
           (syntax-parse y-norm
             [((~literal #%plain-app) tyna (... ...))
              #'(tyna (... ...))]))

         (displayln the-xs-no-app)
         (define the-xs (syntax->list the-xs-no-app))
         (define the-ys (syntax->list the-ys-no-app))
         ;(define the-xs x-norm)
         ;(define the-ys y-norm)
         (displayln "the-x/ys:")
         (displayln the-xs)
         (displayln the-ys)
         (unless (= (length the-xs) (length the-ys))
           (raise-ntac-goal-exception "Both sides of the equality must have the same number of arguments"))
         (define subgoals
           (for/list ([x-expr the-xs]
                      [y-expr the-ys])
             (displayln "x-expr:")
             (displayln x-expr)
             (displayln "type of x-expr")
             (displayln (typeof x-expr))
             (displayln "xexpr keys")
             (displayln (syntax-property-symbol-keys x-expr))
             (define normxexpr (normalize x-expr $ctxt))
             (displayln "normalized x-expr")
             (displayln normxexpr)
             (displayln "type normalized x-expr")
             (displayln (typeof normxexpr))
             (displayln "xexpr normalized keys")
             (displayln (syntax-property-symbol-keys normxexpr))
             (make-ntt-hole #`(== #,(typeof x-expr) #,x-expr #,y-expr))))
         (displayln "subgoals:")
         (displayln subgoals)
         ; TODO solve some subgoals automatically
         ; If we have (== (a b c) (d e f)), start with (== a d), then (== (a b) (d e)) and so on
         (define (apply-func solved-subgoals)
           (for/fold ([full-goal (first solved-subgoals)])
                     ([this-subgoal (rest solved-subgoals)])
             #`(elim-== #,full-goal #,this-subgoal)
             ; TODO Use elim-== to apply (full-goal this-subgoal) and hope that currying doesn't break
             ))
         (displayln "Result:")
         (define result (make-ntt-apply $goal subgoals apply-func))
         (displayln result)
         result
         )]
      [_ (raise-ntac-goal-exception "Goal ~a must be of the form (== T x y) to use this tactic" $goal)])]))
