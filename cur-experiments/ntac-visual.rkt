#lang cur
(require
  cur/ntac/base
  cur/ntac/standard
  cur/ntac/focus-tree
  cur/stdlib/nat
  cur/stdlib/bool
  cur/stdlib/equality
  cur/stdlib/prop
  cur/stdlib/sugar
  cur/ntac/base
  cur/ntac/standard
  cur/ntac/rewrite
  cur/ntac/navigate
  cur/stdlib/sigma
  cur/ntac/f-equal)

(define test 4)

#;(ntac Nat
        ;  display-focus-tree
        ; (fill (exact #'4))
        display-focus-tree)

#;(ntac/visual (Π (x : Type) (n : x) x)
        ;display-focus-tree
        ;(by-intros x n)
       
        ; (fill (exact #'n))
        ;(by-assumption)
        )

#;(ntac/visual (Π (x : Nat)
                (== x x)))

#;(ntac/visual (Π (x : Type) (y : Type)
                  (-> (Π (p : Type) (Or p (-> p False)))
                      (-> (-> (-> x y) y) (-> (-> y x) x))))
               (by-intros x y ex-mid xyy yx)
               (by-destruct (ex-mid x) #:as [(xval) (notxval)])
     
               ; display-focus-tree
               (fill (exact #'xval))
               (by-apply yx)
      
               (by-apply xyy)
               (by-intros xval)
      
               (by-destruct (notxval xval))
      
               ;display-focus
               )

#;(ntac/visual (-> False (== Nat (s 0) 0)))

#;(define-theorem two-neq-four
 (-> (== Nat (s 0) 0)
     (== Nat (s (s 0)) (s (s (s 0)))))
  (by-intro H)
  (by-inversion H)
  display-focus-tree)

; Little typer proofs
(define/rec/match double : Nat -> Nat
  [z => z]
  [(s x) => (s (s (double x)))])

(define (twice (n : Nat))
  (plus n n))

(define add1+=+add1
  (ntac/visual (Π (n : Nat)
           (j : Nat)
           (== Nat
               (s (plus n j))
               (plus n (s j))))
        (by-intros n j)
        (by-induction n)
        reflexivity
        ;(by-apply f-equal #:with Nat Nat s (s (plus X1 j)) (plus X1 (s j)))
        f-equal-tac
        by-assumption))

#;(define twice=double
  (ntac/visual (Π (n : Nat)
                  (== Nat (twice n)
                      (double n)))
               (by-intros n)
               (by-induction n)
               reflexivity
               (by-apply f-equal #:with Nat Nat s (plus X1 (s X1)) (s (double X1)))
               (by-rewriteL IH12)
               by-symmetry
               (by-apply add1+=+add1)))
