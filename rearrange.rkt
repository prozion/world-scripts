#lang racket

(require odysseus)
(require tabtree)
(require tabtree/utils)

(define-namespace-anchor anchor)
(define ns-own (namespace-anchor->namespace anchor))

(define ROOT "../worlds")

(define settings (parse-tab-tree "settings.tree"))
(define global-rules ($2 settings.rearrange.globals settings))
(define files ($3 settings.rearrange.files settings))

(for ((file files))
  (tabtree-sort-and-print
    #:tabtree-file (evalpath ($ file file))
    #:old-treefile (evalpath ($ move-original-to file))
    #:ns ns-own
    #:keys-order (or
                    ($ keys-order global-rules)
                    ($ keys-order file))
    #:sort-by-numeric-desc
                  (or
                    ($ sort-by-numeric-desc global-rules)
                    ($ sort-by-numeric-desc file))
    #:sort-by-numeric-asc
                  (or
                    ($ sort-by-numeric-asc global-rules)
                    ($ sort-by-numeric-asc file))))
