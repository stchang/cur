#lang racket

(provide (struct-out interaction-history))

(struct interaction-history [before-tactic after-tactic tactic-str is-change-focus])
; (interaction-history nttz nttz string bool) represents the state of the proof before a given interaction, the text of the tactic itself, and whether it is a simple focus change
; Two simple focus changes in a row can be combined into one