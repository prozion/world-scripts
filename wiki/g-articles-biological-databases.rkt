#lang racket

(require odysseus)
(require tabtree)
(require tabtree/utils)
(require "../lib/common.rkt")

(provide (all-defined-out))

(define settings (parse-tab-tree "../settings.tree"))
(define by-type-filepath ($1 settings.wiki.bdb.generated-files.by-type-path settings))
(define all-bdb-filepath ($1 settings.wiki.bdb.generated-files.all-bdb-path settings))

(define all-bdb-path (evalpath ($1 settings.wiki.bdb.data.all-bdb-tabtree settings)))
(define bdb-by-type-path (evalpath ($1 settings.wiki.bdb.data.by-type-bdb-tabtree settings)))
(define translations-path (evalpath ($1 settings.wiki.bdb.data.translations-tabtree settings)))

(define all-bdb ($3 databases (parse-tab-tree all-bdb-path)))
(define bdb-by-type (parse-tab-tree bdb-by-type-path))
(define translations ($3 databases_translations (parse-tab-tree translations-path)))

(define (g-bdb-by-type)
  (let* (
        (types ($3 databases_by_types bdb-by-type))
        (bdb-by-type-wiki (for/fold
                            ((res1 ""))
                            ((type types))
                            (let ((bdbs (get-$3 (list "databases_by_types" ($ id type)) bdb-by-type)))
                              (format "~a~n~a~n~a"
                                res1
                                (format "===~a===" (titlefy ($ ru type)))
                                (for/fold
                                  ((res2 ""))
                                  ((bdb bdbs))
                                  (let* ((bdb-id ($ id bdb))
                                        (bdb-url (and ($$ bdb-id all-bdb) (httpify ($ url ($$ bdb-id all-bdb)))))
                                        (description-ru (titlefy (or ($ ru ($$ bdb-id translations)) ""))))
                                    (format "~a* [[~a]] ~a− ~a~n"
                                            res2
                                            bdb-id
                                            (if bdb-url (format "[~a] " bdb-url) "")
                                            description-ru)))))))
          (bdb-by-type-wiki (format "==Типы баз данных==~n~a" bdb-by-type-wiki))
        )
    (write-file by-type-filepath bdb-by-type-wiki)
    #t
))

(define (g-all-bdb)
  (let* (
        (all-bdb-wiki (for/fold
                            ((res ""))
                            ((bdb all-bdb))
                            (let* ((bdb-id ($ id bdb))
                                  (bdb-url (and ($$ bdb-id all-bdb) (httpify ($ url ($$ bdb-id all-bdb)))))
                                  (description-ru (titlefy (or ($ ru ($$ bdb-id translations)) ""))))
                              (format "~a* [[~a]] ~a− ~a.~n"
                                      res
                                      bdb-id
                                      (if bdb-url (format "[~a] " bdb-url) "")
                                      description-ru))))
          (all-bdb-wiki (format "==Список биологических баз данных==~n~a" all-bdb-wiki))
        )
    (write-file all-bdb-filepath all-bdb-wiki)
    #t
))
