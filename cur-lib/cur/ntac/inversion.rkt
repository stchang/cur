#lang s-exp "../main.rkt"
(provide
 (for-syntax
  by-inversion*
  inversion*))

(require
 "../stdlib/prop.rkt" ; for False (see inversion), And (rewrite)
 "../stdlib/sugar.rkt"
 "../stdlib/equality.rkt"
 "base.rkt"
 "standard.rkt"
  (for-syntax "ctx.rkt" "utils.rkt"
              (only-in macrotypes/typecheck-core subst substs)
              macrotypes/stx-utils
              racket/list
              racket/match
              racket/pretty
              syntax/stx
              (for-syntax racket/base syntax/parse)))


(begin-for-syntax

  (define-syntax (by-inversion* syn)
    (syntax-parse syn
      [(_ H) #'(fill (inversion* #'H))]
      [(_ H #:as namess) #'(fill (inversion* #'H #'namess))]))

  (define ((inversion* name [new-xss_ #f]) ctxt pt)
    (match-define (ntt-hole _ goal) pt)

    (define name-ty (ctx-lookup ctxt name))

    ;; get info about the datatype and its constructors
    ;; A = params
    ;; i = indices
    ;; x = non-recursive args to constructors
    ;; xrec = recrusive args to constructors
    ;; irec = indices to recursive args
    (define/syntax-parse
      (elim-TY ([A τA] ...)
               ([i τi] ...)
               Cinfo ...)
      (get-match-info name-ty))

    (define num-params
      (stx-length #'(A ...)))

    (define get-idxs
      (if (stx-null? #'(i ...))
        (λ (t) null)
        (λ (t) (stx-drop t (add1 num-params)))))

    (define new-xss
      (or new-xss_
          (stx-map (λ (_) #f) #'(Cinfo ...))))

    ;; infer from name-ty: params (Aval ...) and indices (ival ...)

    (define/syntax-parse ((Aval ...) (ival ...))
      (syntax-parse name-ty
        [((~literal #%plain-app) _ . name-ty-args)
         (stx-split-at #'name-ty-args num-params)]))

    ;; generate names for the equality hypothesis we will introduce

    (define/syntax-parse (eq-name ...)
      ((freshens name) (stx-map (λ (x) (format-id x "Heq~a" x))
                                #'(i ...))))

    ;; generate subgoals for each data constructor case

    (define-values [subgoals mk-elim-methods]
      (for/lists (subgoals mk-elim-methods)
                 ([Cinfo (in-stx-list #'(Cinfo ...))]
                  [new-xs (in-stx-list new-xss)])
        (syntax-parse Cinfo
          [[C ([x_ τx_] ... τout_)
              ([xrec_ . _] ...)]
           #:with (new-x ...) (or new-xs ((freshens name) #'(x_ ...)))
           #:with (xrec ...) ((freshens name) #'(xrec_ ...))
           #:with (τx ... τout) (substs #'(Aval ... new-x ...)
                                        #'(A    ... x_ ...)
                                        #'(τx_ ... τout_))

           ;; (eq ...) = equality types for the indices of this particular constructor
           #:with (iout ...) (get-idxs #'τout)
           #:with (eq ...) #'[(== τi ival iout) ...]

           #:do [(define (update-ctxt/xs ctxt)
                   (for/fold ([ctxt ctxt])
                             ([new-x (in-stx-list #'(new-x ...))]
                              [τx    (in-stx-list #'(τx ...))])
                     (ctx-add ctxt new-x (normalize τx ctxt))))
                 ; NOTE: xrec purposely not put in context; the user doesn't need it.
                 ; maybe should've used dependent matching instead of elim?
                 (define (update-ctxt/eqs ctxt)
                   (ctx-adds ctxt
                             #'(eq-name ...)
                             (stx-map (normalize/ctxt ctxt) #'(eq ...))))
                 (define update-ctxt
                   (compose update-ctxt/eqs
                            update-ctxt/xs))]

           (values
            (make-ntt-context ; subgoal
             update-ctxt
             (make-ntt-hole goal))
            (λ (pf)
              #`(λ new-x ... xrec ... eq-name ...
                   #,pf)))])))

    (make-ntt-apply
     goal
     subgoals
     (λ pfs ;; constructs proof term, from each subgoals' proof terms
       (quasisyntax/loc goal
         ((new-elim
           ; target
           #,name
           ; motive
           #,(with-syntax ([(i* ...) (generate-temporaries #'(ival ...))])
               (with-syntax ([(eq ...) (stx-map unexpand #'[(== τi ival i*) ...])])
                 #`(λ i* ... #,name
                      (-> eq ...
                          #,goal))))
           ; methods
           . #,(map (λ (mk pf) (mk pf)) mk-elim-methods pfs))
          ; arguments (refl proofs)
          (refl τi ival)
          ...)))))

  )
