#lang racket

(require odysseus)

(provide (all-defined-out))

(define ROOT "../worlds")

(define (evalpath astr)
  (string-replace astr "$ROOT" ROOT))
