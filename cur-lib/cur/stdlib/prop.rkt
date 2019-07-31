#lang s-exp "../main.rkt"

(provide
 (rename-out [conj/i conj]
             [or_introL left]
             [or_introR right])
 I
 True
 elim-True
 False
 elim-False
 Not
 Or
 elim-Or
 And
 elim-And
 ∧ ∨ ¬
 iff iff-sym ↔
 thm:anything-implies-true
 pf:anything-implies-true
 thm:and-is-symmetric pf:and-is-symmetric
 thm:proj1 pf:proj1
 thm:proj2 pf:proj2
 (for-syntax ~And ~Or ~True ~False))

(require "sugar.rkt")

(define-datatype False : Type)

(define-datatype True : Type
  [I : True])

(define-datatype And [X : Type] [Y : Type] : Type
  [conj [x : X] [y : Y] : (And X Y)])

(define-datatype Or [X : Type] [Y : Type] : Type
  [or_introL [x : X] : (Or X Y)]
  [or_introR [y : Y] : (Or X Y)])

(define (Not [P : Prop])
  (-> P False))

(module* tauto #f
  (require turnstile
           (for-syntax syntax/parse)
           (only-in "../curnel/turnstile-impl/dep-ind-cur2+sugar.rkt" [#%app typed-app]))

  (provide tauto)

  (define-for-syntax tautology
    (syntax-parser
      ;      [_ #:do[(printf "tauto: ~a\n" (stx->datum this-syntax))] #:when #f #'debug]
      [~True #'I]
      [(~And X Y)
       #:when (and (tautology #'X) (tautology #'Y))
       #`(typed-app conj X Y (tauto X) (tauto Y))]
      [(~Or X Y)
       #:when (tautology #'X)
       #'(typed-app or_introL X Y (tauto X))]
      [(~Or X Y)
       #:when (tautology #'Y)
       #'(typed-app or_introR X Y (tauto Y))]
      [_ #f]))

  (define-syntax tauto
    (syntax-parser
      [(_ prop)
       #:do[(define res (tautology (expand/df #'prop)))]
       (or res (type-error #:src #'prop #:msg "no proof"))])))

;; Unicode symbols (\wedge, \vee)
(define-syntax ∧
  (syntax-parser
    [(_) #'True]
    [(_ a) #'a]
    [(_ a b ...) #'(And a (∧ b ...))]))

(define-syntax ∨
  (syntax-parser
    [(_) #'False]
    [(_ a) #'a]
    [(_ a b ...) #'(Or a (∨ b ...))]))

(define-syntax ¬
  (syntax-parser [(_ P) #'(Not P)]))

;; inferring version of conj
(define-implicit conj/i = conj 2)

(define thm:anything-implies-true (Π (P : Type) True))
(define pf:anything-implies-true (λ (P : Type) I))

(define thm:and-is-symmetric
  (Π (P : Type) (Q : Type) (ab : (And P Q)) (And Q P)))

(define pf:and-is-symmetric
  (lambda (P : Type) (Q : Type) (ab : (And P Q))
          (match ab #:return (And Q P)
            [(conj (x : P) (y : Q))
             (conj Q P y x)])))

(define thm:proj1
  (Π (A : Type) (B : Type) (c : (And A B)) A))

(define pf:proj1
  (lambda (A : Type) (B : Type) (c : (And A B))
          (match c #:return A
            [(conj (a : A) (b : B)) a])))

(define thm:proj2
  (Π (A : Type) (B : Type) (c : (And A B)) B))

(define pf:proj2
  (lambda (A : Type) (B : Type) (c : (And A B))
          (match c #:return B
            [(conj (a : A) (b : B)) b])))

(define (iff [A : Prop] [B : Prop])
  (And (-> A B) (-> B A)))

(define iff-sym
  (λ (P : Prop) (Q : Prop) (H : (iff P Q))
     (match H #:as H
            #:in (iff P Q)
            #:return (iff Q P)
      [(conj H1 H2) (conj (-> Q P) (-> P Q) H2 H1)])))

(define-syntax ↔
  (syntax-parser
    [(_ A B) #'(iff A B)]
    [(_ A B C ...) #'(And (iff A B) (↔ B C ...))]))
