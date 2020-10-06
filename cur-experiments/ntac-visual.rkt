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
  cur/ntac/rewrite)

#;(ntac Nat
        display-focus-tree
        (fill (exact #'4))
        display-focus-tree)

(ntac (Π (x : Type) (n : x) x)
      display-focus-tree
        (by-intros x n)
       
        (fill (exact #'n))
        ;(by-assumption)
        display-focus-tree)

#;(ntac (Π (x : Type) (y : Type)
         (-> (Π (p : Type) (Or p (-> p False)))
             (-> (-> (-> x y) y) (-> (-> y x) x))))
      (by-intros x y ex-mid xyy yx)
      (by-destruct (ex-mid x) #:as [(xval) (notxval)])
      (fill (exact #'xval))
      (by-apply yx)
      (by-apply xyy)
      (by-intros xval)
      display-focus-tree
      (by-destruct (notxval xval))
      
      display-focus)
