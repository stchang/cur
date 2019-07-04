#lang cur
(require
 cur/stdlib/sugar
 cur/stdlib/nat
 cur/stdlib/bool
 cur/stdlib/equality
 cur/stdlib/prop
 rackunit/turnstile)

;; tests for dependent pattern matching; from Cockx thesis

(define-datatype Vec [A : Type] : (-> [i : Nat] Type)
  [nil : (Vec A z)]
  [cns [k : Nat] [x : A] [xs : (Vec A k)] : (Vec A (s k))])

(define/rec/match* replicate [A : Type] [x : A] : [n : Nat] -> (Vec A n)
  [z => (nil A)]
  [(s m) => (cns A m x (replicate A x m))])
  
(check-type (replicate Nat z z) : (Vec Nat z) -> (nil Nat))
(check-type (replicate Nat z (s z)) : (Vec Nat (s z))
            -> (cns Nat z z (nil Nat)))
(check-type (replicate Nat z (s (s z))) : (Vec Nat (s (s z)))
            -> (cns Nat (s z) z (cns Nat z z (nil Nat))))

(define/rec/match* not-not : [b : Bool] -> (== Bool (not (not b)) b)
  [true => (refl Bool true)]
  [false => (refl Bool false)])

(check-type (not-not true) : (== Bool (not (not true)) true))
(check-type (not-not false) : (== Bool (not (not false)) false))

(define/rec/match* absurd [A : Type] : [f : False] -> A)
(check-type absurd : (Π [A : Type] (-> False A)))

(define/rec/match* plus-zero : [n : Nat] -> (== Nat (plus n z) n)
  [z => (refl Nat z)]
  [(s m) => (f-equal Nat Nat s (plus m z) m (plus-zero m))])

; args are reversed (due to how def/rec/match* handles pat matching)
; concat ys xs -> xs ++ ys
(define/rec/match* concat [A : Type] [n : Nat] [ys : (Vec A n)] : [m : Nat] [xs : (Vec A m)] -> (Vec A (plus m n))
  [($inacc z) (nil _) => ys]
  [($inacc (s m-1)) (cns _ m-1 x xsrst) => (cns A (plus m-1 n) x (concat A n ys m-1 xsrst))])

(check-type (concat Nat z (nil Nat) z (nil Nat)) : (Vec Nat z) -> (nil Nat))
; last 2 args dont match up
(typecheck-fail (concat Nat z (nil Nat) (s z) (nil Nat)) 
  #:with-msg "expected \\(Vec Nat \\(s z\\)\\), given \\(Vec Nat z\\)")
(typecheck-fail (concat Nat z (nil Nat) z (cns Nat z z (nil Nat)))
  #:with-msg "expected \\(Vec Nat z\\), given \\(Vec Nat \\(s z\\)\\)")

; n is length of result
(define/rec/match* tail [A : Type] : [n : Nat] [xs : (Vec A (s n))] -> (Vec A n)
  [($inacc m) (cns _ m x xsrst) => xsrst])

;; nil case impossible
(typecheck-fail (tail Nat z (nil Nat))
 #:with-msg "expected \\(Vec Nat \\(s z\\)\\), given \\(Vec Nat z\\)")
(typecheck-fail (tail Nat (s z) (nil Nat))
 #:with-msg "expected \\(Vec Nat \\(s \\(s z\\)\\)\\), given \\(Vec Nat z\\)")

(check-type (tail Nat z (cns Nat z z (nil Nat))) : (Vec Nat z) -> (nil Nat))
(check-type (tail Nat (s z) (cns Nat (s z) z (cns Nat z z (nil Nat)))) : (Vec Nat (s z)) -> (cns Nat z z (nil Nat)))
