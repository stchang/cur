#lang s-exp "../main.rkt"

(require "./base.rkt"
         (for-syntax syntax/parse racket/async-channel racket/base))

(provide
 (for-syntax display-focus-tree
             nttz->focused-ntt))

(begin-for-syntax
  ; A focused-ntt is a ntt or a (ntt-focus bool type print-ntt)
  ; ntt-focus represents the currently focused subtree of a ntt
  ; Not useful for processing but nice for printing
  (struct ntt-focus ntt (subtree) #:transparent #:constructor-name _ntt-focus)
  (define (make-ntt-focus subtree)
    (_ntt-focus (ntt-contains-hole? subtree) (ntt-goal subtree) subtree))

  ; nttz -> (ntt, ntt)
  ; Returns the root of the tree, and the currently focused node
  ; One node in the tree should eq? the focus
  (define (get-full-tree-and-focus tz)
    (define focus (nttz-focus tz))
    (define top (to-top tz))
    (values (nttz-focus top) focus))

  ; ntt ntt -> focused-ntt
  ; Insert the focus into the ntt
  (define (insert-focus-ntt tree focus)
    (parameterize ([current-tracing? #t])
      (let loop [(pt tree)]
        (if (eq? pt focus)
            (make-ntt-focus pt) ; Don't recursively call
            (cond 
              [(ntt-hole? pt) pt]
              [(ntt-exact? pt) pt]
              [(ntt-context? pt) (make-ntt-context (ntt-context-env-transformer pt) (loop (ntt-context-subtree pt)))]
              [(ntt-apply? pt) (make-ntt-apply (ntt-goal pt) (map loop (ntt-apply-subterms pt)) (ntt-apply-tactic pt))]
              [(ntt-done? pt) (make-ntt-done (loop (ntt-done-subtree pt)))])))))

  ; nttz -> focused-ntt
  ; Get the whole tree, but with one part focused
  (define (nttz->focused-ntt nttz)
    (define-values (top focus) (get-full-tree-and-focus nttz))
    (insert-focus-ntt top focus))
  
  (define (display-focus-tree current-nttz)
    (define focused-ntt (nttz->focused-ntt current-nttz))
    (init-ns)
    (define ch (make-channel))
    (parameterize ([current-namespace gui-ns])
      (eval #`(parameterize ([current-eventspace es])
                (define frame (new frame% [label "Lorem Ipsum"]))
                (focused-ntt->hierarchical-list frame #,focused-ntt)
                (define btn (new button% [label "Close"] [parent frame] [callback (λ (b e) (#,channel-put #,ch #,current-nttz))]))
                (send frame show #t))))
    (channel-get ch))
  
  (define gui-ns (make-base-namespace))
  (define ns-initialized #f)
  (define (init-ns)
    (unless ns-initialized
      (parameterize ([current-namespace gui-ns])
        (eval #'(require racket/gui mrlib/hierlist racket/class))
        (eval #'(define set-text-mixin
                  (mixin (hierarchical-list-item<%>)
                    ((interface () set-text))
                    (inherit get-editor)
                    (super-new)
                    ; set-text: this sets the label of the item
                    (define/public (set-text str)
                      (define t (get-editor)) ; a text% object
                      (send t erase)
                      (send t insert str)))))
        (eval #'(define es (make-eventspace)))
        (eval #'(define (focused-ntt->compound-item parent-item ntt)
                  (define it (send parent-item new-item set-text-mixin))
                  (send it set-text "test")
                  #;(match )))
        (eval #'(define (focused-ntt->hierarchical-list parent ntt)
                  (define lst (new hierarchical-list% [parent parent]))
                  (focused-ntt->compound-item lst ntt)))
        )
      (set! ns-initialized #t))))
              
                



