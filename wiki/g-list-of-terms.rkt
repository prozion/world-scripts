#lang racket

(require odysseus)
(require tabtree)
(require tabtree/utils)
(require "../lib/common.rkt")

(provide (all-defined-out))

(define settings (parse-tab-tree "../settings.tree"))
(define all-terms-filepath ($1 settings.wiki.terms.generated-files.all-terms-path settings))

(define all-terms-path (evalpath ($1 settings.wiki.terms.data.all-terms-tabtree settings)))

(define all-terms ($3 terms (parse-tab-tree all-terms-path)))

(define (build-article-name item)
  (cond
    (($ deabbr item) (format "~a (~a)" (namefy ($ deabbr item)) (namefy ($ id item))))
    (($ abbr item) (format "~a (~a)" (namefy ($ id item)) ($ abbr item)))
    (else (namefy ($ id item)))))

(define multicolumn-template-format
  #<<T
{{columns-list|gap=1em|class=plainlist|style=font-style: italic;|~a}}
T
)

(define (g-all-terms)
  (let* (
        (all-terms-wiki (for/fold
                            ((res ""))
                            ((term all-terms))
                            (format "~a* [[~a]]~n"
                              res
                              (build-article-name term))))
        (all-terms-wiki (format "==Список статей на создание==~n~a" (format multicolumn-template-format all-terms-wiki)))
        )
    (write-file all-terms-filepath all-terms-wiki)
    #t
))
