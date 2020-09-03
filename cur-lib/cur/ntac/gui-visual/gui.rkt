#lang racket/base

(provide focused-ntt->hierarchical-list es test-frame)

(require
  (for-template "../base.rkt" "./stx-str.rkt" "./ntt-focus.rkt"))

(require racket/match racket/gui mrlib/hierlist racket/class)
  
(define set-text<%> (interface () set-text))
(define set-text-mixin
  (mixin (hierarchical-list-item<%>)
    (set-text<%>)
    (inherit get-editor)
    (super-new)
    ; set-text: this sets the label of the item
    (define/public (set-text str)
      (define t (get-editor)) ; a text% object
      (send t erase)
      (send t insert str))))
(define ntt-list-item<%>
  (interface () set-ntt))
(define ntt-exact-mixin
  (mixin (hierarchical-list-item<%> set-text<%>) (ntt-list-item<%>)
    (inherit set-text)
    (super-new)
    (define/public (set-ntt ntt)
      (define ty (ntt-goal ntt))
      (define val (ntt-exact-term ntt))
      ;(match-define (ntt-exact ty _ val) ntt) ; WHY doesn't this work?
      (set-text (string-append (stx->str val) " : " (stx->str ty))))))
(define ntt-focus-mixin
  (mixin (hierarchical-list-compound-item<%> set-text<%>) (ntt-list-item<%>)
    (inherit set-text)
    (super-new)
    (define/public (set-ntt ntt)
      (define subterm (ntt-focus-subtree ntt))
      ; TODO Decide which item to use based off of the subterm
      (set-text "FOCUSED HERE")
      (define sub-item (send this new-list (compose ntt-done-mixin set-text-mixin)))
      (send sub-item set-ntt subterm))))
(define ntt-done-mixin
  (mixin (hierarchical-list-compound-item<%> set-text<%>) (ntt-list-item<%>)
    (inherit set-text)
    (super-new)
    (define/public (set-ntt ntt)
      (define subterm (ntt-done-subtree ntt))
      ; TODO Decide which item to use based off of the subterm
      (set-text "DONE")
      (define sub-item (send this new-item (compose ntt-exact-mixin set-text-mixin)))
      (send sub-item set-ntt subterm))))
(define es (make-eventspace))
(define (focused-ntt->compound-item parent-item ntt)
  (define it (send parent-item new-list (compose ntt-focus-mixin set-text-mixin)))
  (send it set-ntt ntt)
  #;(match ))
(define (focused-ntt->hierarchical-list parent ntt)
  (define lst (new hierarchical-list% [parent parent]))
  (focused-ntt->compound-item lst ntt))

(define (test-frame nttz ntt)
  (define ch (make-channel))
    
  (parameterize ([current-eventspace es])
    (define frame (new frame% [label "Lorem Ipsum"]))
    (focused-ntt->hierarchical-list frame ntt)
    (define btn (new button% [label "Close"] [parent frame] [callback (λ (b e) (channel-put ch nttz))]))
    (send frame show #t))
    
  (channel-get ch))
