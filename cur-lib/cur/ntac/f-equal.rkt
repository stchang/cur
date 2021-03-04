#lang cur/metantac

(require #;(for-syntax "../stdlib/equality.rkt")
         "./standard.rkt"
         "../stdlib/equality.rkt")

(begin-for-syntax

  (provide f-equal-tac)
  
  (define-tactic f-equal-tac
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
             (make-ntt-hole #`(== #,(resugar-type (typeof x-expr)) #,x-expr #,y-expr))))
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