#lang turnstile/lang
(require (except-in "dep-ind-cur2.rkt" λ #%app Π ~Π)
         (except-in "dep-ind-cur2+sugar.rkt" define)
         (only-in "dep-ind-cur2+data.rkt" pat->ctxt datacons)
         turnstile/eval turnstile/typedefs turnstile/more-utils)

;; a 2nd dep-ind-cur2 library implementing define-datatype
;; - uses define-type directly instead of define-cur-constructor
;; - so does not curry
;; - but is cleaner to explain

; Π  λ ≻ ⊢ ≫ → ∧ (bidir ⇒ ⇐) τ⊑ ⇑

(provide define-datatype
         (for-syntax pat->ctxt (rename-out [get-is get-idxs/unexp])))

;; define-data-constructor wraps define-type to enable currying of constructor,
;; differences with define-type*:
;; - expander name same as constructor name
(define-syntax define-data-constructor
  (syntax-parser
    [;; τ and τ-out have "unexpanded" shape, ie curried with single-applications
     ;; TODO: should they be unexpanded before or after passing to define-data-constructor?
     (_ name [A:id Atag:id τA] ... (~datum :) [i+x (~datum :) τ] ... (~datum ->) τ-out . rst)
     #:with name/internal (fresh #'name)
     #:with name/internal-expander (mk-~ #'name/internal)
     #:with name-expander #'name
     #:with (p ...) (generate-temporaries #'(A ... i+x ...))
      #'(begin-
         (define-type name/internal : [A Atag τA] ... [i+x : τ] ... -> τ-out . rst)
         (define-syntax name
           (datacons
            (λ (stx)
              ((make-variable-like-transformer
                (quasisyntax/loc stx
                  (λ [A Atag τA] ... [i+x : τ] ...
                     #,(syntax/loc stx (name/internal A ... i+x ...)))))
               stx))
            #;(make-variable-like-transformer
               #'(λ [A+i : τ] ... (name/internal A+i ...)))
            (λ (pat t) ; pat should have type t (unexpanded type)
              (syntax-parse pat
                #;[debug
                   #:do[(printf "pat->ctxt: ~a : ~a (expected ~a)\n"
                                (stx->datum #'debug) (stx->datum t) (stx->datum #'τ-out))]
                   #:when #f #'debug]
                [:id ; nullary constructor, no extra binders
                 #:fail-unless (if (id? t)
                                   (typecheck? ((current-type-eval) t)
                                               ((current-type-eval) #'τ-out))
                                   (free-id=? (stx-car t) (get-ty-name #'τ-out)))
                 (format "expected pattern for type ~a, given pattern for ~a: ~a\n"
                         ((current-resugar) t) ((current-resugar) #'τ-out) (syntax->datum pat))
                 null]
                [(_ p ...)
                 #:fail-unless (if (id? t)
                                   (typecheck? ((current-type-eval) t)
                                               ((current-type-eval) #'τ-out))
                                   (free-id=? (stx-car t) (get-ty-name #'τ-out)))
                 (format "expected pattern for type ~a, given pattern for ~a: ~a\n"
                         ((current-resugar) t) ((current-resugar) #'τ-out) (syntax->datum pat))
                 (let L ([res null]
                         ;; drop param pats, for now
                         ;; to include them, need to make them equiv to params in t?
                         [ps (stx-drop #'(p ...) (stx-length #'(A ...)))]
                         [xs (stx->list #'(i+x ...))]
                         [τs (stx->list
                              (if (stx-null? #'(A ...))
                                  #'(τ ...)
                                  (substs (if (id? t) #'() (stx-take (stx-cdr t) (stx-length #'(A ...))))
                                      #'(A ...)
                                      #'(τ ...))))])
                   (if (stx-null? ps)
                       res
                       (let ([x+tys (pat->ctxt (car ps) (car τs))])
                         (L (append res x+tys)
                            (cdr ps)
                            (cdr xs)
                            (if (and (not (null? x+tys))
                                     (id? (car ps))
                                     (free-id=? (car ps) (caar x+tys)))
                                (map (λ (t) (subst (car ps) (car xs) t)) (cdr τs))
                                (cdr τs))))))]))))
         (begin-for-syntax
           (define-syntax name-expander
             (make-rename-transformer #'name/internal-expander))))]))

;; define-type* wraps define-type to enable currying of constructor
(define-syntax define-type*
  (syntax-parser
    [(_ name (~datum :) [A+i:id Atag:id τ] ... (~datum ->) τ-out . rst)
     #:with name/internal (fresh #'name)
     #:with name/internal-expander (mk-~ #'name/internal)
     #:with name-expander (mk-~ #'name)
      #'(begin-
         (define-type name/internal : [A+i Atag τ] ... -> τ-out . rst)
         (define-syntax name
           #;(make-variable-like-transformer
            #'(λ [A+i : τ] ... (name/internal A+i ...)))
           (λ (stx)
             ((make-variable-like-transformer
               (quasisyntax/loc stx
                 (λ [A+i : τ] ...
                    #,(syntax/loc stx (name/internal A+i ...)))))
              stx)))
         (begin-for-syntax
           (define-syntax name-expander
             (make-rename-transformer #'name/internal-expander))))]))

(begin-for-syntax
  ;; x+τss = (([x τ] ...) ...)
  ;; returns subset of each (x ...) that is recursive, ie τ = (TY . args)
  ;; along with the indices needed by each recursive x
  ;; - ASSUME: the recursive arg has type (TY . args) where TY is unexpanded
  ;;   series of curried mono-apps, where indices are last num-is args to TY
  (define (find-recur/is TY num-is x+τss)
    (stx-map
     (λ (x+τs)
       (define xs (stx-map stx-car x+τs))
       (stx-filtermap
        (syntax-parser
          [(x t:id) (and (free-id=? #'t TY) (list #'x))] ; num-is should be = 0
          [(x t) #:when (free-id=? (get-ty-name #'t #;(+ num-As num-is)) TY)
                 (cons #'x (get-is #'t num-is))]
          [_ #f])
        x+τs))
     x+τss))
  ;; ty is unexpanded, series of curried mono-applications of type constructor
  (define (get-ty-name ty)
    (if (id? ty) ty (get-ty-name (stx-car ty))))
  ;; ty is unexpanded, series of curried mono-applications of type constructor
  (define (get-is ty num-is)
    (if (zero? num-is) null (append (get-is (stx-car ty) (sub1 num-is)) (stx-cdr ty))))
  )

;; TmpTy is a placeholder for undefined names
(struct TmpTy- ())
(define-syntax TmpTy
  (syntax-parser
    [:id (assign-type #'TmpTy- #'Type)]
    ;; TODO: orig will get stuck with eg, (TmpTy A)
    [(_ . args) (assign-type (syntax/loc this-syntax (app/eval TmpTy- . args)) #'Type)]))

;; use this macro to expand e, which contains references to unbound X
(define-syntax (with-unbound stx)
  (syntax-parse stx
    [(_ X:id e)
     ;swap in a tmp (bound) id `TmpTy` for unbound X
     #:with e/tmp (subst #'TmpTy #'X #'e)
     ;; expand with the tmp id
      (expand/df #'e/tmp)]))

;; experimental define-datatype : matches coq Inductive defs
(define-typed-syntax define-datatype
  [(_ TY:id [A_:id (~datum :) τA_] ... ; params: τA_ may reference preceding A
            (~datum :) τTY ; possibly declares indices, may reference params
            ;; constructors, full type of each C is (Π [A_ : τA_] ... x+τx ... τC)
            [C:id (~and (~not (~datum :)) x+τx) ...
                  (~optional (~seq (~datum :) τC) ; TODO: improve err with no τout and indices
;                                   (~parse τout-omitted? #'#t))
                             #:defaults ([τC #'(TY A_ ...)]))] ...) ≫
   ;; validate inputs
   [[A_ ≫ A : τA_] ...
    [TY ≫ TY- : (Π [A_ : τA_] ... τTY)] ; need TY in env for inductive args
                   ⊢ [τTY ≫ (~Π [i : τi_] ... τ_) ⇐ TypeTop]
                     [(Π x+τx ... τC) ≫ (~Π [i+x : τin_] ... τout_) ⇐ TypeTop] ...]

   ;; TODO: this err msg comes too late, ie the above already fails
   ;; problem is we dont know number of indices until after expanding
   ;; #:fail-when (and (attribute τout-omitted?) (not (zero? (stx-length #'(i ...)))))
   ;;             "must explicitly declare output types when indices > 0"

   ;; reconstruct surface tys (TODO: avoid this?), with proper binder and references
   #:with (τA ...) (substs #'(A ...) #'(A_ ...) #'(τA_ ...))
   #:with (τi ... τ) (datum->syntax #'TY (stx-map unexpand #'(τi_ ... τ_)))
   #:with ((τin ... τout) ...) (datum->syntax #'TY
                                 (stx-map
                                  (λ (ts) (stx-map unexpand ts))
                                  (subst #'TY #'TY- #'((τin_ ... τout_) ...))))

   ;; - each (xrec ...) is subset of (x ...) that are recur args,
   ;; ie, they are not fresh ids
   ;; - each xrec is accompanied with irec ...,
   ;;   which are the indices in i+x ... needed by xrec
   #:do[(define num-params (stx-length #'(A ...)))
        (define num-idxs (stx-length #'(i ...)))]
   #:with (((xrec irec ...) ...) ...)
          (find-recur/is #'TY num-idxs #'(([i+x τin] ...) ...))

   ;; below here is same as define-datatype ------------------------------
          
   ;; ---------- pre-generate other patvars; makes nested macros below easier to read
   ;; i* = inferred (concrete) i in elim
   #:with (i* ...) (generate-temporaries #'(i ...))
   ; dup (A ...) C times, for ellipses matching
   #:with ((AxC ...) ...) (stx-map (lambda _ #'(A ...)) #'(C ...))
   #:with ((τAxC ...) ...) (stx-map (λ _ #'(τA ...)) #'(C ...))
   #:with (m ...) (generate-temporaries #'(C ...))
   #:with (m- ...) (generate-temporaries #'(C ...))
   #:with TY-patexpand (mk-~ #'TY)
   #:with elim-TY (format-id #'TY "elim-~a" #'TY)
   #:with eval-TY (format-id #'TY "match-~a" #'TY)
   #:with (τm ...) (generate-temporaries #'(m ...))
   #:with (C-pat ...) (stx-map
                       (λ (C xs)
                         (if (and (zero? num-params) (stx-null? xs))
                             C ; must not be (C) pattern; unlike #%app, (C) \neq C due to id macro behavior
                             #`(#,C A ... . #,xs)))
                       #'(C ...)
                       #'((i+x ...) ...))

   ;; ASSUMING: τout is unexpanded curried single-apps
   ;; - this is the same "patvar trick" as re-using A below
   ;; - it makes sure that the method output types properly reference the right i
   #:with ((τouti ...) ...) (stx-map (λ (t) (get-is t num-idxs)) #'(τout ...))

   ;; these are all the generated definitions that implement the define-datatype
   #:with OUTPUT-DEFS
    #`(begin-
        ;; define the type
        (define-type* TY : [A : τA] ... [i : τi] ... -> τ
          #:extra elim-TY
                  ([A τA] ...)
                  ([i τi] ...)
                  (C ([i+x τin] ... τout) ((xrec irec ...) ...)) ...)

        ;; define the data constructors
        (define-data-constructor C [AxC : τAxC] ... : [i+x : τin] ... -> τout) ...

        ;; define eliminator-form elim-TY
        ;; v = target
        ;; - infer A ... from v
        ;; P = motive
        ;; - is a (curried) fn that consumes:
        ;;   - indices i ... with type τi
        ;;   - and TY A ... i ... 
        ;;     - where A ... args is A ... inferred from v
        ;;     - and τi also instantiated with A ...
        ;; - output is a type
        ;; m = branches
        ;; - each is a fn that consumes:
        ;;   - maybe indices i ... (if they are needed by args)
        ;;   - constructor args
        ;;     - inst with A ... inferred from v
        ;;   - maybe IH for recursive args
        (define-typerule/red (elim-TY v P m ...) ≫
          ;; target, infers A ...
          ;; this means every patvar in define-datatype input pattern that references A
          ;; is now instantiated with inferred A
          ;; - must unexpand all types that reference A ...
          ;; (see also comments below)
          #,(if (zero? (+ num-params num-idxs))
                #'[⊢ v ≫ v- ⇐ TY]
                #'[⊢ v ≫ v- ⇒ (TY-patexpand A ... i* ...)])

          ;; τi instantiated with A ... from v-
          ;; nb: without this conditional, sf/Poly.rkt fails with mysterious tyerr
          #,(if (zero? (+ num-params num-idxs))
                #'[⊢ P ≫ P- ⇐ (→ TY TypeTop)]
                #'[⊢ P ≫ P- ⇐ (Π [i : τi] ... ; TODO: unexpand τi? (may reference A ...?)
                                 (→ (TY #,@(stx-map unexpand #'(A ...)) i ...) TypeTop))])

          ;; each m is curried fn consuming 3 (possibly empty) sets of args:
          ;; 1,2) i+x  - indices of the tycon, and args of each constructor `C`
          ;;             the indices may not be included, when not needed by the xs
          ;; 3) IHs - for each xrec ... (which are a subset of i+x ...)
          #:with (τm ...)
          ;; #,(if (zero? (+ num-idxs num-params))
          ;;       #'#'((Π [i+x : τin] ...
          ;;             (→ (P- xrec) ... (P- (C i+x ...)))) ...)
          ;;       #'#`( (Π [i+x : τin] ... ; constructor args ; ASSUME: i+x includes indices
          ;;                (→ (P- irec ... xrec) ... ; IHs
          ;;                   ;; need to unexpand τouti again bc it may reference (new) A ...
          ;;                  (P- #,@(stx-map unexpand #'(τouti ...))
          ;;                      (C #,@(stx-map unexpand #'(AxC ...)) i+x ...))))
          ;;             ...))
          ;; TODO: unexpand τin ...? (may reference A ...?)
          #`( (Π [i+x : τin] ... ; constructor args ; ASSUME: i+x includes indices
                 (→ (P- irec ... xrec) ... ; IHs
                    ;; need to unexpand τouti again bc it may reference (new) A ...
                    (P- #,@(stx-map unexpand #'(τouti ...))
                        (C #,@(stx-map unexpand #'(AxC ...)) i+x ...))))
              ...)
          
          [⊢ m ≫ m- ⇐ τm] ...

          ;; #:with out-ty #,(if (zero? (+ num-params num-idxs))
          ;;                     #'#'(P- v-)
          ;;                     #'#`(P- #,@(stx-map unexpand #'(i* ...)) v-))
          #:with out-ty #`(P- #,@(stx-map unexpand #'(i* ...)) v-)
          -----------
          [⊢ (eval-TY v- P- m- ...) ⇒ out-ty]

          #:where eval-TY #:display-as elim-TY ; elim reduction rule
          [(#%plain-app C-pat P m ...) ; elim redex
           ~> (app/eval m i+x ... (eval-TY xrec P m ...) ...)] ...)
        )
;    #:do[(pretty-print (stx->datum #'OUTPUT-DEFS))]
   --------
   [≻ OUTPUT-DEFS]])

;; TODO: problem: use unexpanded A ... and τA ..., or expanded A2 and τA2 ?
;; - must expand:
;;   - ow var capture possible (eg binder types) due to patvar trick in output macros
;;     - manual subst will capture as well
;;     - (cant use function application to inst bc of mixing expanded (P-) and unexpanded terms)
;;   - eg (define-datatype TY [A : Type] [B : (Π [A : Type] A)] -> Type)
;;     - the 2nd A binding will get replaced with whatever the first A arg is
;;       - eg (TY (Π [X : Type] X) _) -> "non-id err" due to  (Π [(Π [X : Type] X) : Type] _)
;;   - see also dep-ind-cur2+data2-tests
;; - must not expand:
;;   - passing already-expanded types to define-type (and other forms)
;;     may result in loss of types of types when crossing module boundry
;;     - bc stx props not preserved deeply
;;   - eg, run stdlib/sigma tests when giving expanded types to define-type
;; - workaround:
;;   - dont expand, until expander is fixed
;;   - but manually check for captured binders (TODO)
#;(define-typed-syntax define-datatype
  ;; simple datatypes, eg Nat -------------------------------------------------
  ;; - ie, `TY` is an id with no params or indices
  [(_ TY:id (~datum :) τ [C:id (~datum :) τC] ...) ≫
   ----------
   [≻ (define-datatype1 TY : τ [C : τC] ...)]]
  [(_ TY:id (~and (~not (~datum :)) A) ...  (~datum ->) . rst) ≫ ; no indices
   ----------
   [≻ (define-datatype TY A ... : -> . rst)]]
  ;; --------------------------------------------------------------------------
  ;; defines inductive type family `TY`, with:
  ;; - params A ...
  ;; - indices i ...
  ;; - ie, TY is a type constructor with type (Π [A : τA] ... [i τi] ... τ)
  ;; --------------------------------------------------------------------------
  [(_ TY:id [A:id Atag:id τA] ... (~datum :) ; params: τA may reference preceding A
            [i:id itag:id τi] ... ; indices: τis may reference As and preceding i
            (~datum ->) τ
            ;; constructors: τin ... τout may reference A ... and preceding i+x ...
            (~or* (~and [C:id (~datum :) τout] ;; nullary constructor
                        (~parse ([i+x i+xtag τin]...) #'()))
                  (~and [C:id (~datum :)
                              [i+x1:id (~datum :) #;i+xtag1:id τin1] ... ; named args
                              (~and τin2 (~not [_:id (~datum :) _])) ... ; unnamed args
                              (~datum ->) τout]
                        (~parse ([i+x i+xtag τin] ...)
                                (append
                                 (syntax->list #'([i+x1 : #;i+xtag1 τin1] ...))
                                 (stx-map
                                  (λ (t)
                                    (list
                                     (syntax-property (generate-temporary) 'tmp #t)
                                     ':
                                     t))
                                  #'(τin2 ...))))))
            ...) ≫

   ;; validate types: use nested telescopes
   [[A ≫ _ Atag τA ≫ _] ... ⊢
    [[i ≫ _ itag τi ≫ _] ... ⊢ τ ≫ _ ⇐ TypeTop]
    [[i+x ≫ _ i+xtag (with-unbound TY τin) ≫ _] ... ⊢ (with-unbound TY τout) ≫ _ ⇐ TypeTop] ...]

   ;; - each (xrec ...) is subset of (x ...) that are recur args,
   ;; ie, they are not fresh ids
   ;; - each xrec is accompanied with irec ...,
   ;;   which are the indices in i+x ... needed by xrec
   ;; ASSUME: the indices are the first (stx-length (i ...)) args in i+x ...
   ;; ASSUME: indices cannot have type (TY ...), they are not recursive
   ;;         (otherwise, cannot include indices in args to find-recur/i)
   #:with (((xrec irec ...) ...) ...)
          (find-recur/is #'TY (stx-length #'(A ...)) (stx-length #'(i ...)) #'(([i+x τin] ...) ...))

   ;; ---------- pre-generate other patvars; makes nested macros below easier to read
   ;; i* = inferred (concrete) i in elim
   #:with (i* ...) (generate-temporaries #'(i ...))
   ; dup (A ...) C times, for ellipses matching
   #:with ((AxC ...) ...) (stx-map (lambda _ #'(A ...)) #'(C ...))
   #:with ((AtagxC ...) ...) (stx-map (lambda _ #'(Atag ...)) #'(C ...))
   #:with ((τAxC ...) ...) (stx-map (λ _ #'(τA ...)) #'(C ...))
   #:with (m ...) (generate-temporaries #'(C ...))
   #:with (m- ...) (generate-temporaries #'(C ...))
   #:with TY-patexpand (mk-~ #'TY)
   #:with elim-TY (format-id #'TY "elim-~a" #'TY)
   #:with eval-TY (format-id #'TY "match-~a" #'TY)
   #:with (τm ...) (generate-temporaries #'(m ...))
   #:with (C-expander ...) (stx-map mk-~ #'(C ...))

   ;; ASSUMING: τoutA has shape (TY A ... τouti ...), or id
   ;; - this is the same "patvar trick" as re-using A below
   ;; - it makes sure that the method output types properly reference the right i
   #:with ((τouti ...) ...) (stx-map
                             (λ (ts)
                               (or (and (stx-pair? ts) (stx-drop ts (add1 (stx-length #'(A ...)))))
                                   #'()))
                             #'(τout ...))

   ;; these are all the generated definitions that implement the define-datatype
   #:with OUTPUT-DEFS
    #'(begin-
        ;; define the type
        (define-type* TY : [A Atag τA] ... [i itag τi] ... -> τ
          #:extra elim-TY
                  ([A τA] ...)
                  ([i τi] ...)
                  (C ([i+x τin] ... τout) ((xrec irec ...) ...)) ...)

        ;; define the data constructors
        (define-data-constructor C : [AxC AtagxC τAxC] ... [i+x i+xtag τin] ... -> τout) ...

        ;; define eliminator-form elim-TY
        ;; v = target
        ;; - infer A ... from v
        ;; P = motive
        ;; - is a (curried) fn that consumes:
        ;;   - indices i ... with type τi
        ;;   - and TY A ... i ... 
        ;;     - where A ... args is A ... inferred from v
        ;;     - and τi also instantiated with A ...
        ;; - output is a type
        ;; m = branches
        ;; - each is a fn that consumes:
        ;;   - maybe indices i ... (if they are needed by args)
        ;;   - constructor args
        ;;     - inst with A ... inferred from v
        ;;   - maybe IH for recursive args
        (define-typerule/red (elim-TY v P m ...) ≫
          ;; target, infers A ...
          ;; this means every patvar in define-datatype input pattern that references A
          ;; is now instantiated with inferred A
          ;; (see also comments below)
          [⊢ v ≫ v- ⇒ (TY-patexpand A ... i* ...)]

          ;; τi instantiated with A ... from v-
          [⊢ P ≫ P- ⇐ (Π [i itag τi] ...
                         (→ (TY #,@(stx-map unexpand #'(A ... i ...))) TypeTop))]

          ;; each m is curried fn consuming 3 (possibly empty) sets of args:
          ;; 1,2) i+x  - indices of the tycon, and args of each constructor `C`
          ;;             the indices may not be included, when not needed by the xs
          ;; 3) IHs - for each xrec ... (which are a subset of i+x ...)
          #:with (τm ...)
                 #`( (Π [i+x i+xtag τin] ... ; constructor args ; ASSUME: i+x includes indices
                        (→ (P- irec ... xrec) ... ; IHs
                           (P- #,@(stx-map unexpand #'(τouti ...))
                               (C #,@(stx-map unexpand #'(AxC ... i+x ...))))))
                     ...)
          [⊢ m ≫ m- ⇐ τm] ...
          -----------
          [⊢ (eval-TY v- P- m- ...) ⇒ (P- #,@(stx-map unexpand #'(i* ...)) v-)]

          #:where eval-TY #:display-as elim-TY ; elim reduction rule
          [(#%plain-app (C AxC ... i+x ...) P m ...) ; elim redex
           ~> (app/eval m i+x ... (eval-TY xrec P m ...) ...)] ...)
        )
;    #:do[(pretty-print (stx->datum #'OUTPUT-DEFS))]
   --------
   [≻ OUTPUT-DEFS]])
