#lang racket/base

(provide focused-ntt->hierarchical-list es test-frame)

(require
  (for-template "../base.rkt" "./stx-str.rkt" "./ntt-focus.rkt"))

(require racket/match racket/gui mrlib/hierlist racket/class)

(define ntt-hierlist-common-item<%> (interface (hierarchical-list-item<%>)
                                      (set-text (->m string? void?)) ; If current node is focused, adds (Focused) to beginning
                                      (set-background-color (->m (or/c string? (is-a?/c color%)) void?)))) ; Call this before setting text
                                      
(define ntt-hierlist-item<%> (interface (ntt-hierlist-common-item<%>)                            
                               (open-on-path-to-focus (->m void?)) ; If this node is on the path to the focus, open it, otherwise close it. Returns whether node is on path.
                               (open-all (->m void?)) ; Recursively open all nodes
                               (initialize (->m void?))))

#;(define ntt-hierlist-compound-item<%> (interface (ntt-hierlist-item<%> hierarchical-list-compound-item<%>)
                                          (add-child-ntt (->m ntt? (is-a?/c ntt-hierlist-item<%>)))))

; Many thanks to https://docs.racket-lang.org/mrlib/Hierarchical_List_Control.html for the sample code
(define (ntt-common-mixin ntt-ex)
  (mixin (hierarchical-list-item<%>)
    (ntt-hierlist-item<%>)
    (inherit get-editor)
    (super-new)
    
    ; set-text: this sets the label of the item
    (define/public (set-text str)
      (define t (get-editor)) ; a text% object
      (send t erase)
      (define focus-str (if (ntt-ext-is-focus? ntt-ex) (string-append "(Focused) " str) str))
      (send t insert focus-str))

    ; Must be called before setting the text, acts like the text background function in word
    (define/public (set-background-color color)
      (define delta (make-object style-delta% 'change-nothing))
      (send delta set-delta-background color)
      (define editor (get-editor))
      (send editor change-style delta))))

(define (ntt-common-node-mixin ntt-ex-node)
  (compose
   (mixin (hierarchical-list-compound-item<%> ntt-hierlist-common-item<%>)
     (ntt-hierlist-item<%>)
     (inherit open close)
     (super-new)

     (field [child-nodes '()])
    
     (define/public (open-on-path-to-focus)
       (if (ntt-ext-on-path-to-focus? ntt-ex-node)
           (open)
           (close))
       (for [(child child-nodes)]
         (send child open-on-path-to-focus)))

     (define/public (open-all)
       (open)
       (map (λ (child) (send child open-all))))

     (define/public (initialize)
       (define ch-node (focused-ntt->compound-item this ntt))
       (set! child-nodes (cons ch-node child-nodes))))
   (ntt-common-mixin ntt-ex-node)))

(define (ntt-common-leaf-mixin ntt-ex-leaf)
  (compose 
   (mixin (hierarchical-list-item<%> ntt-hierlist-common-item<%>)
     (ntt-hierlist-item<%>)
     (inherit open close)
     (super-new)

     (define/override (open-on-path-to-focus)
       (void))

     (define/override (open-all)
       (void))

     (define/public (initialize)
       (void)))
   (ntt-common-mixin ntt-ex-leaf)))

(define ntt-list-item<%>
  (interface (ntt-hierlist-item<%>))) ; TODO selection handler injection?

#;(define ntt-exact-mixin
    (mixin (ntt-hierlist-item<%>) (ntt-list-item<%>)
      (inherit set-text set-background-color)
      (super-new)
      (define/public (init-ntt-exact ty val)
        (set-background-color "MediumGoldenrod"); Needs to be before inserting text- setting background color is like highlighting
        (set-text (string-append (stx->str val) " : " (stx->str ty))))))

#;(define ntt-done-mixin
    (mixin (ntt-hierlist-compound-item<%>) (ntt-list-item<%>)
      (inherit set-text set-background-color add-child-ntt)
      (super-new)
      (define/public (init-ntt-done subterm)
        (set-text "Top Level")
        (send this open) ; The subterm can't open before the parent does
        (add-child-ntt subterm))))

#;(define ntt-hole-mixin
    (mixin (ntt-hierlist-item<%>) (ntt-list-item<%>)
      (inherit set-text set-background-color)
      (super-new)
      (define/public (init-ntt-hole ty)
        (set-background-color "MistyRose")
        (set-text (stx->str ty)))))

; TODO finish this
#;(define ntt-context-mixin
    (mixin (ntt-hierlist-compound-item<%>) (ntt-list-item<%>)
      (inherit set-text set-background-color add-child-ntt)
      (super-new)
      (define/public (init-ntt-context ))))

(define-syntax-rule (ntt-init-match parent-item ntt focused?
                                    [match-clause is-compound mixin-type (method-name args ...)] ...)
  (match ntt
    [match-clause (begin
                    (define sub-item
                      (if is-compound
                          (send parent-item new-list (compose mixin-type ntt-common-compound-mixin (ntt-common-mixin focused?)))
                          (send parent-item new-item (compose mixin-type (ntt-common-mixin focused?)))))
                    (send sub-item method-name args ...))] ...))

(define es (make-eventspace))
(define (focused-ntt->compound-item parent-item ntt)
  (define-values (focused? ntt-unfocused) (values #t ntt)
    #;(if (ntt-focus? ntt)
          (values #t (ntt-focus-subtree ntt))
          (values #f ntt)))
  (ntt-init-match parent-item ntt-unfocused focused?
                  [(ntt-exact _ ty val) #f ntt-exact-mixin (init-ntt-exact ty val)]
                  [(ntt-done _ _ subtree) #t ntt-done-mixin (init-ntt-done subtree)]
                  [(ntt-hole _ ty) #f ntt-hole-mixin (init-ntt-hole ty)]))

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
