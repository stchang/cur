#lang cur/metantac

(require (for-syntax racket/base))

(begin-for-syntax
  (provide
    (struct-out path-down-context)
    (struct-out path-down-apply))

  (struct path-down-context [] #:transparent)
  (struct path-down-apply [n] #:transparent)

  (define (navigate-step step tz)
    (match step
      [(path-down-context) (nttz-down-context tz)]
      [(path-down-apply n) (nttz-down-apply tz n)]))

  ; This tactic should probably only be used by generated code
  (define-syntax (navigate syn)
    (syntax-case syn ()
      [(_ path-step ...)
       #`(λ (ptz)
           (parameterize [(current-tracing? #t)]
             (match-define (ntt-done _ _ top-ntt) (to-top ptz))
             (for/fold ([tz top-ntt])
                       ([step (list path-step ...)])
               (navigate-step step tz))))])))

