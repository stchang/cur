#lang s-exp "../main.rkt"
(provide
 (for-syntax
  prove-unify
  prove-unifys
  derived
  impossible))

(require
 (only-in "../stdlib/prop.rkt" True False I)
 "../stdlib/sugar.rkt"
 "../stdlib/equality.rkt"
 "../stdlib/nat.rkt"
 "base.rkt"
 "standard.rkt"
 (for-syntax "utils.rkt"
             ; (only-in macrotypes/typecheck-core subst substs)
             macrotypes/stx-utils
             ;racket/list
             racket/match
             racket/pretty
             syntax/stx))

(begin-for-syntax

  ;; ==s    : (listof type-stx)
  ;; proofs : (listof term-stx)
  ; Each term proves the corresponding equality type
  (struct derived [==s proofs])

  ;; contradiction : term-stx
  ; 'contradiction' has type False.
  (struct impossible [contradiction])

  ;; term-stx term-stx term-stx -> (or/c derived? impossible?)
  ; Either returns (derived ..) containing equality proofs for subexpressions, or
  ;   returns (impossible ..) if unification is impossible.
  ; NOTE: 'top-L' and 'top-R' must be terms of the same type.
  ;       'top-pf' must be a proof of '(== top-L top-R)'
  (define (prove-unify top-L top-R top-pf)
    (let/ec
        ; Escape cont allows us to immediately exit when we find a contradiction
        abort

      (define top-TY (typeof top-L))
      (define top-id (generate-temporary 'x))
      (define contra-id (generate-temporary 'y))

      (define (mk-proof tgt-TY tgt-term)
        (if (eq? tgt-TY top-TY)
          top-pf ; no proof needed
          #`(f-equal #,(unexpand top-TY)
                     #,(unexpand tgt-TY)
                     (λ #,top-id #,tgt-term)
                     #,(unexpand top-L)
                     #,(unexpand top-R)
                     #,top-pf)))

      (define (abort/contradiction false-pf)
        (abort (impossible false-pf)))

      (define ==s    '())
      (define proofs '())

      ; === Imperatively traverse L and R, looking for new assumptions or contradictions.
      ; Populates:
      ;   ==s     (list of equality types found)
      ;   proofs  (list of proofs of corresponding =='s)
      (let TRAV ([TY top-TY]
                 [L top-L]
                 [R top-R]
                 ; 'term' is a term of type type 'TY' that satisfies the f-equal proof in
                 ; 'mk-proof'.
                 [term top-id])
        (syntax-parse {list L R}
          [{({~literal #%plain-app} CL:id L* ...)
            ({~literal #%plain-app} CR:id R* ...)}
           #:when (has-type-info? TY)
           #:with (_ ([A _] ...) _ {~and Cinfo (C _ _)} ...) (get-match-info TY)

           ; === If constructors differ, build a contradiction
           (unless (and (free-identifier=? #'CL #'CR)
                        (stx-length=? #'(L* ...) #'(R* ...)))
             (define (mk-match-clause Cinfo)
               (syntax-parse Cinfo
                 [[C ([x _] ... _) _]
                  #`[(C x ...)
                     #,(if (stx-datum-equal? #'C #'CL)
                         #'True
                         #'False)]]))
             (abort/contradiction
              #`(elim-==
                 #,(mk-proof TY term)
                 (λ #,contra-id _
                    (match #,contra-id
                      #:return Type
                      #,@(stx-map mk-match-clause #'(Cinfo ...))))
                 I)))

           ; === Constructors match; recur into subexpressions
           (for ([i (in-naturals)]
                 [L* (in-list (stx-drop #'(L* ...) (stx-length #'(A ...))))]
                 [R* (in-list (stx-drop #'(R* ...) (stx-length #'(A ...))))])
             (define TY* (typeof L*))
             (define (mk-match-clause Cinfo)
               (syntax-parse Cinfo
                 [[C ([x _] ... _) _]
                  #`[(C x ...)
                     #,(if (stx-datum-equal? #'C #'CL)
                         (stx-list-ref #'(x ...) i)
                         (unexpand L*))]]))
             (TRAV TY* L* R*
                   #`(match #,term
                       #:return #,(unexpand TY*)
                       #,@(stx-map mk-match-clause #'(Cinfo ...)))))]

          [_
           ; === Can't traverse terms any further, so emit a proof now
           (define tgt-== #`(== #,(unexpand TY) #,(unexpand L) #,(unexpand R)))
           (define tgt-proof (mk-proof TY term))
           (set! ==s    (cons tgt-== ==s))
           (set! proofs (cons tgt-proof proofs))]))

      ; -- finished with TRAV, return the proofs --
      (derived ==s proofs)))

  (define (prove-unifys Ls Rs pfs)
    (define-values [==s derived-pfs imposs]
      (for/fold ([==s '()] [pfs '()] [abort? #f])
                ([L (in-list Ls)]
                 [R (in-list Rs)]
                 [pf (in-list pfs)]
                 #:break abort?)
        (match (prove-unify L R pf)
          [(derived ==s* pfs*)
           (values (append ==s* ==s)
                   (append pfs* pfs)
                   #f)]
          [(and im (impossible q))
           (values #f #f im)])))
    (or imposs (derived ==s derived-pfs)))

  )
