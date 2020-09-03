#lang cur/metantac
;#lang s-exp "../main.rkt"

(require ; "./base.rkt"
         "./gui-visual/ntt-focus.rkt"
         "./gui-visual/gui.rkt"
         (for-syntax syntax/parse racket/base))

(provide
 (for-syntax display-focus-tree
             nttz->focused-ntt))

(begin-for-syntax
 
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
    ;(init-ns)
    ;(define ch (make-channel))
    
   ; (parameterize ([current-namespace gui-ns])
   #;   (eval #`(parameterize ([current-eventspace es])
                (define frame (new frame% [label "Lorem Ipsum"]))
                (focused-ntt->hierarchical-list frame #,focused-ntt)
                (define btn (new button% [label "Close"] [parent frame] [callback (λ (b e) (#,channel-put #,ch #,current-nttz))]))
                (send frame show #t)))
    ;)
    #;(channel-get ch)
    ;(define ns (module->namespace  "../cur-lib/cur/ntac/gui-visual/gui.rkt"))
    #;(test-frame current-nttz focused-ntt)
    (eval #`(require "../cur-lib/cur/ntac/gui-visual/gui.rkt")) ; WTF
    (eval #`(test-frame #,current-nttz #,focused-ntt)))


  #;(define (ty->str t)
    (define ty-datum (stx->datum (resugar-type t)))
    (define ostr (open-output-string))
    (display ty-datum ostr)
    (get-output-string ostr))
  
  ;(define gui-ns (make-base-namespace))
  ;(define ns-initialized #f)
  #;(define (init-ns)
    (unless ns-initialized
      (parameterize ([current-namespace gui-ns])
        (eval #`(require racket/match racket/gui mrlib/hierlist racket/class))
        (eval #`(define set-text<%> (interface () set-text)))
        (eval #`(define set-text-mixin
                  (mixin (hierarchical-list-item<%>)
                    (set-text<%>)
                    (inherit get-editor)
                    (super-new)
                    ; set-text: this sets the label of the item
                    (define/public (set-text str)
                      (define t (get-editor)) ; a text% object
                      (send t erase)
                      (send t insert str)))))
        (eval #`(define ntt-list-item<%>
                  (interface () set-ntt)))
        (eval #`(define ntt-exact-mixin
                  (mixin (hierarchical-list-item<%> set-text<%>) (ntt-list-item<%>)
                    (inherit set-text)
                    (super-new)
                    (define/public (set-ntt ntt)
                      (define ty (#,ntt-goal ntt))
                      (define val (#,ntt-exact-term ntt))
                      ;(match-define (ntt-exact ty _ val) ntt) ; WHY doesn't this work?
                      (set-text (string-append val " : " (#,ty->str ty)))))))
        (eval #`(define ntt-focus-mixin
                  (mixin (hierarchical-list-compound-item<%> set-text<%>) (ntt-list-item<%>)
                    (inherit set-text)
                    (super-new)
                    (define/public (set-ntt ntt)
                      (define subterm (#,ntt-focus-subtree ntt))
                      ; TODO Decide which item to use based off of the subterm
                      (set-text "FOCUSED HERE")
                      (define sub-item (send this new-list (compose ntt-done-mixin set-text-mixin)))
                      (send sub-item set-ntt subterm)))))
        (eval #`(define ntt-done-mixin
                  (mixin (hierarchical-list-compound-item<%> set-text<%>) (ntt-list-item<%>)
                    (inherit set-text)
                    (super-new)
                    (define/public (set-ntt ntt)
                      (define subterm (#,ntt-done-subtree ntt))
                      ; TODO Decide which item to use based off of the subterm
                      (set-text "DONE")
                      (define sub-item (send this new-item (compose ntt-exact-mixin set-text-mixin)))
                      (send sub-item set-ntt subterm)))))
        (eval #`(define es (make-eventspace)))
        (eval #`(define (focused-ntt->compound-item parent-item ntt)
                  (define it (send parent-item new-list (compose ntt-focus-mixin set-text-mixin)))
                  (send it set-ntt ntt)
                  #;(match )))
        (eval #`(define (focused-ntt->hierarchical-list parent ntt)
                  (define lst (new hierarchical-list% [parent parent]))
                  (focused-ntt->compound-item lst ntt))))
  
      (set! ns-initialized #t))))
              
                



