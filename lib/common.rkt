#lang racket

(require odysseus)

(provide (all-defined-out))

(define ROOT "/home/denis/projects/worlds")

(define (evalpath astr)
  (string-replace astr "$ROOT" ROOT))
