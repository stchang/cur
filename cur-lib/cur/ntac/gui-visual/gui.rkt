#lang racket/base

(provide focused-ntt->hierarchical-list es test-frame)

(require
  (for-template "../base.rkt" "./stx-str.rkt" "./ntt-focus.rkt"))

(require racket/match racket/gui mrlib/hierlist racket/class)


(define ntt-hierlist-item<%> (interface ()
                               (set-text (->m string? void?))
                               (set-background-color (->m (or/c string? (is-a?/c color%)) void?))))

; Many thanks to https://docs.racket-lang.org/mrlib/Hierarchical_List_Control.html for the sample code
(define ntt-common-mixin
  (mixin (hierarchical-list-item<%>)
    (ntt-hierlist-item<%>)
    (inherit get-editor)
    (super-new)
    ; set-text: this sets the label of the item
    (define/public (set-text str)
      (define t (get-editor)) ; a text% object
      (send t erase)
      (send t insert str))

    (define/public (set-background-color color)
      (define delta (make-object style-delta% 'change-nothing))
      (send delta set-delta-background color)
      (define editor (get-editor))
      (send editor change-style delta))))

(define ntt-list-item<%>
  (interface ())) ; TODO selection handler injection?

(define ntt-exact-mixin
  (mixin (hierarchical-list-item<%> ntt-hierlist-item<%>) (ntt-list-item<%>)
    (inherit set-text set-background-color)
    (super-new)
    (define/public (init-ntt-exact ty val)
      (set-background-color "MediumGoldenrod")
      (set-text (string-append (stx->str val) " : " (stx->str ty))))))

(define ntt-focus-mixin
  (mixin (hierarchical-list-compound-item<%> ntt-hierlist-item<%>) (ntt-list-item<%>)
    (inherit set-text set-background-color)
    (super-new)
    (define/public (init-ntt-focus subterm)
      (set-background-color "LightCyan") ; Needs to be before inserting text- setting background color is like highlighting
      (set-text "Proof Focus")
      (send this open) ; The subterm can't open before the parent does
      (focused-ntt->compound-item this subterm))))

(define ntt-done-mixin
  (mixin (hierarchical-list-compound-item<%> ntt-hierlist-item<%>) (ntt-list-item<%>)
    (inherit set-text set-background-color)
    (super-new)
    (define/public (init-ntt-done subterm)
      (set-text "Top Level")
      (send this open)
      (focused-ntt->compound-item this subterm))))

(define ntt-hole-mixin
  (mixin (hierarchical-list-item<%> ntt-hierlist-item<%>) (ntt-list-item<%>)
    (inherit set-text set-background-color)
    (super-new)
    (define/public (init-ntt-hole ty)
      (set-background-color "MistyRose")
      (set-text (stx->str ty)))))

(define-syntax-rule (ntt-init-match parent-item ntt
                                    [match-clause (list-type mixin-type) (method-name args ...)] ...)
  (match ntt
    [match-clause (begin
                    (define sub-item (send parent-item list-type (compose mixin-type ntt-common-mixin)))
                    (send sub-item method-name args ...))] ...))

(define es (make-eventspace))
(define (focused-ntt->compound-item parent-item ntt)
  (ntt-init-match parent-item ntt
                  [(ntt-exact _ ty val) (new-item ntt-exact-mixin) (init-ntt-exact ty val)]
                  [(ntt-focus _ _ subtree) (new-list ntt-focus-mixin) (init-ntt-focus subtree)]
                  [(ntt-done _ _ subtree) (new-list ntt-done-mixin) (init-ntt-done subtree)]
                  [(ntt-hole _ ty) (new-item ntt-hole-mixin) (init-ntt-hole ty)]))

(define (focused-ntt->hierarchical-list parent ntt)
  (define lst (new hierarchical-list% [parent parent]))
  (focused-ntt->compound-item lst ntt))

(define (test-frame nttz ntt)
  (define ch (make-channel))
    
  (parameterize ([current-eventspace es])
    (define frame (new frame% [label "Lorem Ipsum"]))
    (focused-ntt->hierarchical-list frame ntt)
    (define btn (new button% [label "Close"] [parent frame] [callback (λ (b e) (channel-put ch frame))]))
    (send frame show #t))
    
  (define frame (channel-get ch))
  (send frame show #f)
  nttz)
