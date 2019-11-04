#lang cur
(require
 cur/stdlib/axiom
 cur/stdlib/sugar
 cur/stdlib/z3
 (only-in racket/base module)
 "../ntac/rackunit-ntac.rkt"
 rackunit/turnstile
 )

(define-axiom/z3 eq-refl
  (∀ [a : Nat] (-> (== a a))))

(typecheck-fail/toplvl
 (define-axiom/z3 eq-refl2
   (∀ [a b : Nat] (-> (== a a) (== a b))))
 #:with-msg "Rosette could not prove")

(define-axiom/z3 eq-nat-sym
  (∀ [a b : Nat] (-> (== a b) (== b a))))

(define-axiom/z3 eq-nat-trans
  (∀ [a b c : Nat] (-> (== a b) (== b c) (== a c))))

(require cur/ntac/base
         cur/ntac/standard
         cur/ntac/rewrite)

(define-theorem eq-nat-sym1
  (∀ [a b : Nat] (-> (== a b) (== b a)))
  (by-exact eq-nat-sym))

(define-axiom eq-nat-sym2
  (∀ [a b : Nat] (-> (== a b) (== b a))))

(module check-output racket/base
  (provide check-output)
  (require rackunit racket/port)
  (define-syntax-rule (check-output expect body ...)
    (check-equal? 'expect (with-output-to-string (λ () body ...)))))
(require 'check-output)

(check-output
 "Axioms used by \"eq-nat-sym\":\n - eq-nat-sym : (Π (a : Nat) (b : Nat) (→ (== Nat a b) (== Nat b a)))\n"
 (print-assumptions eq-nat-sym))
(check-output
 "Axioms used by \"eq-nat-sym1\":\n - eq-nat-sym : (Π (a : Nat) (b : Nat) (→ (== Nat a b) (== Nat b a)))\n"
 (print-assumptions eq-nat-sym1))

(check-output
 "Z3 Axioms used by \"eq-nat-sym2\":\n"
 (print-z3-assumptions eq-nat-sym2))

;; bools
(define-axiom/z3 or-test1
  (== (or true false) true))
(define-axiom/z3 or-test2
  (== (or false false) false))
(define-axiom/z3 or-test3
  (== (or false true) true))
(define-axiom/z3 or-test4
  (== (or true true) true))

(define-axiom/z3 or-test5
  (== (or (or false false) true) true))

(define-axiom/z3 plus-0-n
  (∀ [n : Nat] (== (plus 0 n) n)))

(define-axiom/z3 plus-n-0
  (∀ [n : Nat] (== (plus n 0) n)))

(define-axiom/z3 minus-diag
  (∀ [n : Nat] (== (minus n n) 0)))

(define-axiom/z3 mult-0-n
  (∀ [n : Nat] (== (mult 0 n) 0)))

(define-axiom/z3 mult-n-0
  (∀ [n : Nat] (== (mult n 0) 0)))

(define-axiom/z3 plus-id-example
  (forall [n m : Nat] (-> (== n m) (== (plus n n) (plus m m)))))

(define-axiom/z3 plus-id-exercise
  (forall [n m o : Nat]
          (-> (== n m) (== m o) (== (plus n m) (plus m o)))))

(define-axiom/z3 mult_0_plus
  (forall [n m : Nat] (== (mult (plus 0 n) m) (mult n m))))

(define-theorem mult_0_plus/plus0n
  (forall [n m : Nat] (== (mult (plus 0 n) m) (mult n m)))
  (by-intros n m)
  (by-rewrite plus-0-n)
  reflexivity)

(check-output
 "Axioms used by \"mult_0_plus/plus0n\":\n - plus-0-n : (Π (n : Nat) (== Nat n n))\n"
 (print-assumptions mult_0_plus/plus0n))
  
(define-axiom/z3 mult_S_1
  (∀ [n m : Nat]
     (-> (== m (s n))
         (== (mult m (plus 1 n)) (mult m m)))))

(define-axiom/z3 plus_1_neq_0
  (forall [n : Nat] (== (nat-equal? (plus n 1) 0) false)))

(define-axiom/z3 negb_involutive
  (forall [b : Bool] (== (not (not b)) b)))

(define-axiom/z3 andb_commutative
  (forall [b c : Bool] (== (and b c) (and c b))))

(define-axiom/z3 andb3_exchange
  (forall [b c d : Bool] (== (and (and b c) d) (and (and b d) c))))

(define-axiom/z3 andb_true_elim2
  (forall [b c : Bool] (-> (== (and b c) true) (== c true))))

(define-axiom/z3 zero_nbeq_plus_1
  (forall [n : Nat] (== (nat-equal? 0 (plus n 1)) false)))

;; can rosette verify this?
#;(define-axiom/z3 identity_fn_applied_twice
  (∀ (f : (-> Bool Bool))
     (∀ (x : Bool)
        (-> (== (f x) x)
            (∀ (b : Bool) (== (f (f b)) b))))))

(define-axiom/z3 andb_eq_orb
  (forall (b c : Bool)
          (-> (== (and b c) (or b c))
              (== b c))))

(define-axiom/z3 plus_n_Sm
  (forall [n m : Nat]
          (== (s (plus n m)) (plus n (s m)))))

(define-axiom/z3 plus_comm
  (forall [n m : Nat] (== (plus n m) (plus m n))))

(define-axiom/z3 plus_assoc
  (forall [n m p : Nat] (== (plus n (plus m p)) (plus (plus n m) p))))

(define-axiom/z3 plus_rearrange
  (forall [n m p q : Nat]
          (== (plus (plus n m) (plus p q))
              (plus (plus m n) (plus p q)))))

(define-axiom/z3 leb_refl
  (forall (n : Nat) (== true (<= n n))))

(define-axiom/z3 zero_nbeq_S
  (forall (n : Nat) (== (nat-equal? 0 (s n)) false)))

(define-axiom/z3 andb_false_r
  (forall [b : Bool] (== (and b false) false)))

(define-axiom/z3 plus_ble_compat_l
  (forall [n m p : Nat]
          (-> (== (<= n m) true)
              (== (<= (plus p n) (plus p m)) true))))
(define-axiom/z3 S_nbeq_0
  (forall [n : Nat]
          (== (nat-equal? (s n) 0) false)))

(define-axiom/z3 mult_1_l
  (forall [n : Nat] (== (mult 1 n) n)))

(define-axiom/z3 all3_spec
  (forall [b c : Bool]
          (== (or
               (and b c)
               (or (not b)
                   (not c)))
              true)))

(define-axiom/z3 mult_plus_distr_r
  (forall [n m p : Nat]
          (== (mult (plus n m) p)
              (plus (mult n p) (mult m p)))))

(define-axiom/z3 mult_assoc
  (forall [n m p : Nat]
          (== (mult n (mult m p))
              (mult (mult n m) p))))

(define-axiom/z3 eqb_refl
  (forall [n : Nat] (== true (nat-equal? n n))))

;; pairs
(define-axiom/z3 surjective_pairing
  (forall (n m : Nat)
          (== (pair n m)
              (pair (fst (pair n m))
                    (snd (pair n m))))))

(define-axiom/z3 surjective_pairing2
  (forall (p : (prod Nat Nat))
          (== p (pair (fst p) (snd p)))))
