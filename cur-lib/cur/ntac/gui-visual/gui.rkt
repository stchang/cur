#lang racket/base

(provide ntt-ext->hierarchical-list es test-frame)

(require
  (for-template "../base.rkt" "./stx-str.rkt" "./ntt-focus.rkt" "../standard.rkt"))

(require racket/match racket/gui mrlib/hierlist racket/class "../ctx.rkt")

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
    (ntt-hierlist-common-item<%>)
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

     (match-define (ntt-ext-node is-focus? on-path-to-focus? path-to-here this-nttz subtrees) ntt-ex-node)

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
       (open)  ; The subterm can't open before the parent does
       (for [(subtree subtrees)]
         (define ch-node (ntt-ext->compound-item this subtree))
         (set! child-nodes (cons ch-node child-nodes)))))
   (ntt-common-mixin ntt-ex-node)))

(define (ntt-common-leaf-mixin ntt-ex-leaf)
  (compose 
   (mixin (hierarchical-list-item<%> ntt-hierlist-common-item<%>)
     (ntt-hierlist-item<%>)
     (super-new)

     (define/public (open-on-path-to-focus)
       (void))

     (define/public (open-all)
       (void))

     (define/public (initialize)
       (void)))
   (ntt-common-mixin ntt-ex-leaf)))

(define ntt-list-item<%>
  (interface (ntt-hierlist-item<%>))) ; TODO selection handler injection?

(define (ntt-exact-mixin ntt-ext)
  (compose
   (mixin (ntt-hierlist-item<%>) (ntt-list-item<%>)
     (inherit set-text set-background-color)
     (super-new)
     (match-define (ntt-ext-leaf is-focus? on-path-to-focus? path-to-here this-nttz) ntt-ext)
     (match-define (nttz _ ntt _) this-nttz)
     (match-define (ntt-exact _ ty val) ntt)
     (define/override (initialize)
       (super initialize)
       (set-background-color "MediumGoldenrod"); Needs to be before inserting text- setting background color is like highlighting
       (set-text (string-append (stx->str val) " : " (stx->str ty)))))
   (ntt-common-leaf-mixin ntt-ext)))

(define (ntt-done-mixin ntt-ext)
  (compose
   (mixin (ntt-hierlist-item<%> hierarchical-list-compound-item<%>) (ntt-list-item<%>)
     (inherit set-text set-background-color)
     (super-new)
     (match-define (ntt-ext-node is-focus? on-path-to-focus? path-to-here this-nttz subtrees) ntt-ext)
     (match-define (nttz _ ntt _) this-nttz)
     (match-define (ntt-done _ ty subterm) ntt)
     (define/override (initialize)
       (super initialize)
       (set-text "Top Level")))
   (ntt-common-node-mixin ntt-ext)))

(define (ntt-hole-mixin ntt-ext)
  (compose
   (mixin (ntt-hierlist-item<%>) (ntt-list-item<%>)
     (inherit set-text set-background-color)
     (super-new)
     (match-define (ntt-ext-leaf is-focus? on-path-to-focus? path-to-here this-nttz) ntt-ext)
     (match-define (nttz _ ntt _) this-nttz)
     (match-define (ntt-hole _ ty) ntt)
     (define/override (initialize)
       (super initialize)
       (set-background-color "MistyRose")
       (set-text (stx->str ty))))
   (ntt-common-leaf-mixin ntt-ext)))


(define (ntt-context-mixin ntt-ext)
  (compose
   (mixin (ntt-hierlist-item<%>) (ntt-list-item<%>)
     (inherit set-text set-background-color)
     (super-new)
     (match-define (ntt-ext-node is-focus? on-path-to-focus? path-to-here this-nttz subtrees) ntt-ext)
     (match-define (nttz this-ctxt ntt up) this-nttz)
     
     (match-define (ntt-context _ _ transformer _) ntt)
     (match-define child-ctxt (transformer this-ctxt))

     (define new-ctx-items (ctx-difference child-ctxt this-ctxt))
     (define/override (initialize)
       (super initialize)
       (set-text  (string-append "Context: " (ctx->str new-ctx-items))))) ; TODO What context?
   (ntt-common-node-mixin ntt-ext)))

(define (ntt-apply-mixin ntt-ext)
  (compose
   (mixin (ntt-hierlist-item<%>) (ntt-list-item<%>)
     (inherit set-text set-background-color)
     (super-new)
     (match-define (ntt-ext-node is-focus? on-path-to-focus? path-to-here this-nttz subtrees) ntt-ext)
     (match-define (nttz _ ntt _) this-nttz)
     (match-define (ntt-apply _ _ _ _) ntt)
     (define/override (initialize)
       (super initialize)
       (set-text "Apply")))
   (ntt-common-node-mixin ntt-ext)))

; Add a ntt to a hierarchical list. Parent-item is a hierarchical-list% or hierarchical-list-compound-item<%>
(define (ntt-ext->compound-item parent-item ntt-ext)
  (define this-nttz (ntt-ext-this-nttz ntt-ext))
  (match-define (nttz _ foc _) this-nttz)
  (define item
    (match foc
      [(ntt-exact _ _ _) (send parent-item new-item (ntt-exact-mixin ntt-ext))]
      [(ntt-done _ _ _) (send parent-item new-list (ntt-done-mixin ntt-ext))]
      [(ntt-hole _ _) (send parent-item new-item (ntt-hole-mixin ntt-ext))]
      [(ntt-context _ _ _ _) (send parent-item new-list (ntt-context-mixin ntt-ext))]
      [(ntt-apply _ _ _ _) (send parent-item new-list (ntt-apply-mixin ntt-ext))]))
  (send item initialize))

; Add a hierarchical list containing the representaiton of an ntt. Parent is a frame, dialog, panel, or pane.
(define (ntt-ext->hierarchical-list parent ntt-ext)
  (define lst (new hierarchical-list% [parent parent]))
  (ntt-ext->compound-item lst ntt-ext))

; Hack so the close button works
(define (test-frame-mixin chan)
  (mixin (top-level-window<%>) ()
    (super-new)
    (define/augment (on-close)
      (channel-put chan this))))

(define es (make-eventspace))

; The whole channel and eventspace thing is necessary because we pause execution of the program while
; the window is open. The gui library is really not meant for that, so this is a workaround.
(define (test-frame nttz ntt-ext)
  (define ch (make-channel))
    
  (parameterize ([current-eventspace es])
    (define frame (new ((test-frame-mixin ch) frame%) [label "Lorem Ipsum"] [width 800] [height 600]))
    (ntt-ext->hierarchical-list frame ntt-ext)
    (define btn (new button% [label "Close"] [parent frame] [callback (λ (b e) (channel-put ch frame))]))
    (send frame show #t))
    
  (define frame (channel-get ch))
  (send frame show #f)
  nttz)
