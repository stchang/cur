#lang info
(define collection 'multi)
(define deps '())
(define build-deps
  '("base"
    "rackunit-lib"
    ("cur-lib" #:version "0.42")
    "sweet-exp-lib"
    "chk-lib"
    "rackunit-macrotypes-lib"))
(define update-implies '("cur-lib"))
(define pkg-desc "Tests for \"cur\".")
(define pkg-authors '(wilbowma stchang))
