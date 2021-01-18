#lang racket/base

(provide ntt-ext->hierarchical-list es test-frame
         (struct-out interaction-history))

(require
  (for-template "../base.rkt" "./stx-str.rkt" "./ntt-focus.rkt" "../standard.rkt" "../navigate.rkt" "./ntt-tag.rkt"))

(require racket/exn racket/match racket/gui mrlib/hierlist racket/class "./interaction-history.rkt" "../ctx.rkt")

(define ntt-hierlist-common-item<%> (interface (hierarchical-list-item<%>)
                                      (set-text (->m string? void?)) ; If current node is focused, adds (Focused) to beginning
                                      (set-background-color (->m (or/c string? (is-a?/c color%)) void?))  ; Call this before setting text
                                      (get-ntt-ext-here (->m ntt-ext?))
                                      (append-text (->m string? void?))))
                                      
(define ntt-hierlist-item<%> (interface (ntt-hierlist-common-item<%>)                            
                               (open-on-path-to-focus (->m void?)) ; If this node is on the path to the focus, open it, otherwise close it. Returns whether node is on path.
                               (open-all (->m void?)) ; Recursively open all nodes
                               (initialize (->m void?))
                               (select-focus (->m void?))))

; String [String -> (void)] [String -> (void)] -> (void)
; Somewhat of a hack to color subterms differently
(define (split-on-subterm-text text on-not-match on-match)
  (define re "\\(Subterm [0-9]+\\)")
  (define matches (regexp-match* re text))
  (match-define (cons before-first-match after-first-matches) (regexp-split re text))
  (on-not-match before-first-match)
  (for ([match matches] [after-match after-first-matches])
    (on-match match)
    (on-not-match after-match)))

(define (make-bg-color-style-delta color)
  (define delta (make-object style-delta% 'change-nothing))
  (send delta set-delta-background color)
  delta)

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

    (define/public (append-text str)
      (define t (get-editor))
      (send t insert str))

    ; Must be called before setting the text, acts like the text background function in word
    (define/public (set-background-color color)
      (define editor (get-editor))
      (send editor change-style (make-bg-color-style-delta color)))

    (define/public (get-ntt-ext-here)
      ntt-ex)))

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
       (map (λ (child) (send child open-all)) child-nodes)
       (void))

     (define/public (initialize)
       (open)  ; The subterm can't open before the parent does
       (for [(subtree subtrees)]
         (define ch-node (ntt-ext->compound-item this subtree))
         (set! child-nodes (cons ch-node child-nodes))))
     
     (define/public (select-focus)
       (when on-path-to-focus?
         (if is-focus?
             (send this select #t)
             (for [(ch child-nodes)] (send ch select-focus))))
       (void)))
   (ntt-common-mixin ntt-ex-node)))

(define (ntt-common-leaf-mixin ntt-ex-leaf)
  (compose 
   (mixin (hierarchical-list-item<%> ntt-hierlist-common-item<%>)
     (ntt-hierlist-item<%>)
     (super-new)

     (match-define (ntt-ext-leaf is-focus? on-path-to-focus? path-to-here this-nttz) ntt-ex-leaf)

     (define/public (open-on-path-to-focus)
       (void))

     (define/public (open-all)
       (void))

     (define/public (initialize)
       (void))

     (define/public (select-focus)
       (when is-focus?
         (send this select #t))
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
       (set-background-color "LightCyan")
       (set-text (string-append "Context: " (ctx->str new-ctx-items))))) ; TODO What context?
   (ntt-common-node-mixin ntt-ext)))

(define (ntt-apply-mixin ntt-ext)
  (compose
   (mixin (ntt-hierlist-item<%>) (ntt-list-item<%>)
     (inherit set-text set-background-color append-text)
     (super-new)
     (match-define (ntt-ext-node is-focus? on-path-to-focus? path-to-here this-nttz subtrees) ntt-ext)
     (match-define (nttz _ ntt _) this-nttz)
     (match-define (ntt-apply _ _ subterms tactic) ntt)
     (define/override (initialize)
       (super initialize)
       (set-text "")
       (split-on-subterm-text (apply->str subterms tactic)
                              (λ (not-match)
                                (set-background-color "White")
                                (append-text not-match))
                              (λ (match)
                                (set-background-color "WhiteSmoke")
                                (append-text match)))))
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
  (send item initialize)
  item)

(define (hierarchical-list-sel-handler-mixin %)
  (class %
    (super-new)
    (init [selection-handler (λ (i) (void))])
    (field [sel-handler selection-handler])
    (define/override (on-select i)
      (super on-select i)
      (sel-handler i))))

; Add a hierarchical list containing the representaiton of an ntt. Parent is a frame, dialog, panel, or pane.
; Returns the list itself, plus its (singular) top-level item
; Frame ntt-ext (-> hierarchical-list-item<%> (void)) -> (values hierarchical-list% hierarchical-list-item%)
(define (ntt-ext->hierarchical-list parent ntt-ext selection-handler)
  (define lst (new (hierarchical-list-sel-handler-mixin hierarchical-list%) [parent parent] [selection-handler selection-handler]))
  (define top-item (ntt-ext->compound-item lst ntt-ext))
  (values lst top-item))

; Hack so the close button works
(define (test-frame-mixin chan init-nttz)
  (mixin (top-level-window<%>) ()
    (super-new)
    (field [current-nttz init-nttz])

    (define/public (set-nttz new-nttz)
      (set! current-nttz new-nttz))
    
    (define/augment (on-close)
      (channel-put chan (list 'done current-nttz)))))

; Nice indentation and not limited to 200 chars
(define pretty-message%
  (class editor-canvas%
    (inherit set-editor)
    
    (init label)
    (init [highlight-subterms? #f]) 
    (super-new)

    ; pretty-format doesn't work on strings (or rather, it just puts it in one line)
    ; read creates a s-exp, and pretty-format _can_ deal with that
    (define label-s-exp (with-input-from-string label read))
    (define label-val (pretty-format label-s-exp 80 #:mode 'display))
    
    (define t (new
               (class text%
                 (super-new)
                 (field [is-initial-insert? #t])

                 ; Insert once and nevermore
                 (define/augment (can-insert? s l)
                   is-initial-insert?)
                 (define/augment (can-delete? s l) #f)

                 ; Monospace
                 (send this change-style (make-object style-delta% 'change-family 'modern))
                 (if highlight-subterms?
                     (split-on-subterm-text label-val
                                            (λ (not-match)
                                              (send this change-style (make-bg-color-style-delta "White"))
                                              (send this insert not-match))
                                            (λ (match)
                                              (send this change-style (make-bg-color-style-delta "WhiteSmoke"))
                                              (send this insert match)))
                     (send this insert label-val))
                 (set! is-initial-insert? #f))))

    (set-editor t)))
    

; Some GUI components only support up to 200 characters, truncate a string to fit
(define (trunc-label str)
  (if (<= (string-length str) 200)
      str
      (string-append (substring str 0 197) "...")))

; Create a table pairing IDs with a type
(define (id+ty->list-box id-strs ty-strs parent)
  (define lst-box (new list-box%
                       [label #f]
                       [parent parent]
                       [choices '()]
                       [columns (list "ID" ":" "Type")]))
  (define ids (map trunc-label id-strs))
  (define colons (map (λ (_) ":") id-strs))
  (define tys (map trunc-label ty-strs))
  (send lst-box set ids colons tys)
  lst-box)

; Table for the context at a given point of the tree
(define (ctx->list-box ctx parent)
  (id+ty->list-box (map stx->str (ctx-ids ctx))
                   (map (compose stx->str) (ctx-types ctx))
                   parent))

; Make a box that has additional info about an apply node of the tree
(define (make-apply-box subterms tactic parent)
  (define apply-box (new group-box-panel%
                         [parent parent]
                         [label "Apply"]))
  
  (define apply-subterms-box (new group-box-panel%
                                  [parent apply-box]
                                  [label "Subterms"]))
  (id+ty->list-box (map number->string (range (length subterms)))
                   (map (compose stx->str ntt-goal) subterms)
                   apply-subterms-box)
  
  (define apply-result-box (new group-box-panel%
                                [parent apply-box]
                                [label "Result"]))
  (new pretty-message% [parent apply-result-box]
       [label (apply->str subterms tactic)]
       [highlight-subterms? #t]))

(define (make-hole-box parent ntt-ext new-ntt-ext-callback)
  (define hole-box (new group-box-panel%
                        [parent parent]
                        [label "Hole"]
                        [stretchable-height 0]))
  (define button (new button%
                      [parent hole-box]
                      [label "Focus here"]
                      [enabled (not (ntt-ext-is-focus? ntt-ext))]
                      [callback (λ (b e) (new-ntt-ext-callback ntt-ext))]))
  hole-box)

; Get the panel that shows info about a selected nttz
; CONSTRAINT: This must run on the main thread
; ntt-ext panel [ntt-ext -> void] -> panel
(define (get-panel-content-for-ntt-ext this-ntt-ext parent new-ntt-ext-callback)
  (define this-nttz (ntt-ext-this-nttz this-ntt-ext))
  (define panel (new vertical-panel% [parent parent]))
  
  (define context-panel (new group-box-panel%
                             (parent panel)
                             (label "Context")))
  (match-define (nttz context focus prev) this-nttz)
  (ctx->list-box context context-panel)

  (define goal-panel (new group-box-panel%
                          (parent panel)
                          (label "Goal")))
  (define goal (ntt-goal focus))
  (new pretty-message% (parent goal-panel) (label (stx->str goal)))

  (match focus
    [(ntt-apply _ _ subterms tactic) (make-apply-box subterms tactic panel)]
    [(ntt-hole _ _) (make-hole-box panel this-ntt-ext new-ntt-ext-callback)]
    [_ (void)])

  (unless (ntt-contains-hole? focus)
    (define complete-proof-term
      (with-handlers ([exn:fail? (λ (e) (displayln e)
                                   "\"Proof tree has no holes,\nbut encountered error constructing complete term.\nSee console output for details.\"")])
        (proof-tree->complete-term focus)))
    (define complete-term-panel (new group-box-panel%
                                     (parent panel)
                                     (label "Complete Proof Term")))
    (new pretty-message% (parent complete-term-panel) (label (stx->str complete-proof-term))))

  (when (is-tagged-ntt? focus)
    (define tag-panel (new group-box-panel%
                           (parent panel)
                           (label "Tag")))
    (new pretty-message% (parent tag-panel) (label (ntt-get-tag focus))))
 
  panel)

; Panel that mutates an inner child when a new ntt-ext is provided
(define single-ntt-ext-child-panel%
  (class panel%
    (init initial-ntt-ext)
    (inherit delete-child)
    (super-new)

    (field [current-ntt-ext initial-ntt-ext]
           [current-content (inner (void) new-ntt-ext current-ntt-ext)])

    (define/pubment (new-ntt-ext ntt-ext)
      (set! current-ntt-ext ntt-ext)
      (delete-child current-content)
      (set! current-content (inner (void) new-ntt-ext ntt-ext))
      current-content)))

; Parent panel for the currently selected node info
; This panel will change its contents when new-nttz is given
(define current-nttz-info-panel%
  (class single-ntt-ext-child-panel%
    (init new-ntt-ext-callback)

    (field [ntt-ext-callback new-ntt-ext-callback])

    (super-new)
    
    (define/augment (new-ntt-ext ntt-ext)
      (get-panel-content-for-ntt-ext ntt-ext this ntt-ext-callback))))

; Parent panel for the tree view
(define current-tree-panel%
  (class single-ntt-ext-child-panel%
    (init on-ntt-tree-select)
    (inherit-field current-content)
    (field [ntt-tree-select on-ntt-tree-select]
           [top-item #f])
    
    (super-new) ; This calls new-ntt-ext, which will use the fields- so this must go after the fields are initialized
    
    (define/augment (new-ntt-ext ntt-ext)
      (define-values (lst t-item) (ntt-ext->hierarchical-list this ntt-ext ntt-tree-select))
      (set! top-item t-item)
      lst)

    (define/public (select-focus)
      (when top-item (send top-item select-focus)))

    (define/public (expand-all)
      (send top-item open-all))

    (define/public (expand-path-to-focus)
      (send top-item open-on-path-to-focus))))

; Decorate tree with buttons to manage expanding/collapsing
(define tree-panel-with-buttons%
  (class vertical-panel%
    (init initial-ntt-ext on-ntt-tree-select)
    (super-new)
    (field [tree-panel (new current-tree-panel%
                            [parent this]
                            [initial-ntt-ext initial-ntt-ext]
                            [on-ntt-tree-select on-ntt-tree-select])])
    (define button-panel (new horizontal-panel%
                              [parent this]
                              [stretchable-height #f]))
    
    (define button-expand-all (new button%
                                   [label "Expand all"]
                                   [parent button-panel]
                                   [stretchable-width #t]
                                   [callback (λ (b e) (send tree-panel expand-all))]))

    (define button-expand-focus (new button%
                                     [label "Collapse all except focus"]
                                     [parent button-panel]
                                     [stretchable-width #t]
                                     [callback (λ (b e) (send tree-panel expand-path-to-focus))]))

    (define/public (select-focus)
      (send tree-panel select-focus))

    (define/public (new-ntt-ext ntt-ext)
      (send tree-panel new-ntt-ext ntt-ext))))

(define interaction-panel%
  (class vertical-panel%
    (init initial-nttz)
    (init preload-interaction-history)
    (init-field main-thread-runner new-nttz-callback)
    (field (current-nttz initial-nttz)
           (undo-buffer preload-interaction-history)  ; List of interaction-history, first in list is most recent
           (redo-buffer '())) ; List of interaction-history, first in list is next to redo
    
    (super-new)



    (define tactic-input
      (new text-field%
           [parent this]
           [label "Tactic"]
           [callback (λ (text-field event)
                       (when (symbol=? (send event get-event-type) 'text-field-enter)
                         (define entered-text (send text-field get-value))
                         (send text-field set-value "")
                         (define next-nttz
                           (main-thread-runner
                            (λ ()
                              (with-handlers ([exn:fail? (λ (e) (displayln (exn->string e)) current-nttz)])
                                (define nttz-untagged ((eval (with-input-from-string entered-text read-syntax)) current-nttz))
                                (tag-untagged-nttz-with nttz-untagged entered-text))))) ; TODO Tag with something better than just the text
                         (unless (eq? current-nttz next-nttz)
                           (set! undo-buffer (cons (interaction-history current-nttz next-nttz entered-text #f) undo-buffer))
                           (set! redo-buffer '())
                           (new-nttz-callback next-nttz 'interaction-panel))))]))

    (define (handle-undo-redo-press the-list the-buffer the-func reverse?)
      (define selections (send the-list get-selections))
      (when (cons? selections)
        (define sel-num (first selections))
        (define dist-from-end (if reverse? (- (length the-buffer) sel-num) (add1 sel-num)))
        (the-func dist-from-end)))
    
    (define undo-list
      (new list-box%
           [parent this]
           [label #f]
           [choices '()]))
           
    (define undo-button
      (new button%
           [parent this]
           [label "Undo"]
           [enabled #f]
           [callback (λ (b e) (handle-undo-redo-press undo-list undo-buffer (λ (n) (undo n)) #t))])) ; Since undo is not a real function, need to wrap in lambda

    (define redo-list
      (new list-box%
           [parent this]
           [label #f]
           [choices '()]))

    (define redo-button
      (new button%
           [parent this]
           [label "Redo"]
           [enabled #f]
           [callback (λ (b e) (handle-undo-redo-press redo-list redo-buffer (λ (n) (redo n)) #f))]))

    (define (update-undo-or-redo the-button the-list the-buffer reverse?)
      (define labels (map (λ (elem)
                            (trunc-label (interaction-history-tactic-str elem)))
                          the-buffer))
      (send the-list set (if reverse? (reverse labels) labels))
      (define buf-len (length the-buffer))
      (send the-button enable (> buf-len 0))
      (when (> buf-len 0)
        (define to-select (if reverse? (sub1 buf-len) 0))
        (send the-list set-first-visible-item to-select)
        (send the-list select to-select #t)))
    
    (define (update-undo-redo-buttons)
      (update-undo-or-redo undo-button undo-list undo-buffer #t)
      (update-undo-or-redo redo-button redo-list redo-buffer #f))

    (define/public (set-nttz new-nttz reason)  
      (match reason
        [(list 'hole-selected path)
         (define path-str (path->navigate-str path))
         (set! undo-buffer (match undo-buffer
                             [(cons (interaction-history before-prev _ _ #t) r)
                              (cons (interaction-history before-prev new-nttz path-str #t) r)]
                             [_ (cons (interaction-history current-nttz new-nttz path-str #t) undo-buffer)]))
         (set! redo-buffer '())]
        [_ (void)])
      (update-undo-redo-buttons)
      (set! current-nttz new-nttz))

    (define/public (undo num-times) ; TODO abstract these two functions
      (when (> num-times 0)
        (define new-nttz current-nttz)
        (for ([x (in-range (min (length undo-buffer) num-times))])
          (match undo-buffer
            [(cons f r) (set! new-nttz (interaction-history-before-tactic f))
                        (set! undo-buffer r)
                        (set! redo-buffer (cons f redo-buffer))]
            [_ (void)]))
        (new-nttz-callback new-nttz 'undo)))

    (define/public (redo num-times)
      (when (> num-times 0)
        (define new-nttz current-nttz)
        (for ([x (in-range (min (length redo-buffer) num-times))])
          (match redo-buffer
            [(cons f r) (set! new-nttz (interaction-history-after-tactic f))
                        (set! redo-buffer r)
                        (set! undo-buffer (cons f undo-buffer))]
            [_ (void)]))
        (new-nttz-callback new-nttz 'redo)))

    (define/public (print-tactics)
      (when (ormap interaction-history-is-change-focus undo-buffer)
        (displayln "NOTE: You may need to add (require cur/ntac/navigate) when using the below tactics."))
      (displayln "---Tactics from interaction:---")
      (for ([history (reverse undo-buffer)])
        (displayln (interaction-history-tactic-str history)))
      (displayln "------"))

    (update-undo-redo-buttons)))

(define es (make-eventspace))

; The whole channel and eventspace thing is necessary because we pause execution of the program while
; the window is open. The gui library is really not meant for that, so this is a workaround.
(define (test-frame nttz preload-interaction-history)
  (define ch (make-channel))
  (define ch-resp (make-channel))

  (define in-main-thread-runner? #f)
  (define (run-sync-on-main-thread thunk)
    (if in-main-thread-runner?
        (thunk)
        (begin
          (set! in-main-thread-runner? #t)
          (channel-put ch (list 'run-on-main-thread thunk))
          (let ([retval (channel-get ch-resp)]) ; For some reason define isn't allowed here?
            (set! in-main-thread-runner? #f)
            retval))))
  
  (define init-ntt-ext (nttz->ntt-ext nttz))
  ;(displayln ((eval (with-input-from-string "(fill (exact #'n))" read)) nttz))

  (define frame (void))
    
  (parameterize ([current-eventspace es])
    (set! frame (new ((test-frame-mixin ch nttz) frame%) [label "GUI Proof Explorer"] [width 800] [height 600]))
    (define main-panel (new horizontal-panel% [parent frame]))

    ; Updates the node that the user is focusing on, without modifying the underlying tree
    (define (on-ntt-tree-select new-ntt-ex)
      (send info-panel new-ntt-ext new-ntt-ex))

    ; Call when the proof tree itself must change
    ; reason is one of:
    ; - (list 'hole-selected path) (if a hole is selected as the new focus in the tree)
    ; - 'interaction-panel (if the user typed in the interaction panel)
    ; - 'undo (from interaction panel)
    ; - 'redo
    (define (set-new-nttz new-nttz reason)
      (run-sync-on-main-thread
       (λ ()
         (define top-ntt-ext (nttz->ntt-ext new-nttz))
      
         (send frame set-nttz new-nttz)
      
         (send tree-panel new-ntt-ext top-ntt-ext)
         (send tree-panel select-focus)
         (send interactions-panel set-nttz new-nttz reason)
         )))

    ; Same as set-new-nttz, but with ntt-exts
    (define (set-new-ntt-ext new-ntt-ext reason) ; Note that new-ntt-ext is not the root
      (set-new-nttz (ntt-ext-this-nttz new-ntt-ext) reason))
    
    (define tree-panel (new tree-panel-with-buttons%
                            [parent main-panel]
                            [initial-ntt-ext init-ntt-ext]
                            [on-ntt-tree-select (λ (selected-node) (on-ntt-tree-select (send selected-node get-ntt-ext-here)))]))
        
    ; (define close-btn (new button% [label "Close"] [parent frame] [callback (λ (b e) (channel-put ch frame))]))
    #;(define text-in-editor (new text% [auto-wrap #t]))
    #;(define editor-canvas (new editor-canvas% [parent frame] [editor text-in-editor]))

    (define info-panel (new current-nttz-info-panel%
                            [parent main-panel]
                            [initial-ntt-ext (ntt-ext-find-focus init-ntt-ext)]
                            [new-ntt-ext-callback (λ (ntt-ext) (set-new-ntt-ext ntt-ext (list 'hole-selected (ntt-ext-path-to-here ntt-ext))))]))

    (define interactions-panel
      (new interaction-panel%
           [parent main-panel]
           [initial-nttz nttz]
           [preload-interaction-history preload-interaction-history]
           [main-thread-runner run-sync-on-main-thread]
           [new-nttz-callback (λ (nttz reason) (set-new-nttz nttz reason))]))
    
    (send frame show #t)
    (send tree-panel select-focus)

    (define new-nttz
      (let loop
        ()
        (match (channel-get ch)
          [(list 'run-on-main-thread thunk) (channel-put ch-resp (thunk))
                                            (loop)] ; Some operations need to be run on the main thread or else cur throws an error
          [(list 'done tz) tz])))
    (send interactions-panel print-tactics)
    
    (send frame show #f)
    new-nttz))
