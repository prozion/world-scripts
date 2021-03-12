#lang racket

(require odysseus)
(require tabtree)
(require tabtree/utils)
(require "lib/common.rkt")

(provide (all-defined-out))

(define settings (parse-tab-tree "settings.tree"))
(define filepath ($1 settings.wiki.patchworks.file.path settings))
(define filename ($1 settings.wiki.patchworks.file.filename settings))
(define tabtree-file (evalpath ($1 settings.wiki.patchworks.data.file settings)))
(define COLUMNS (->number ($1 settings.wiki.patchworks.parameters.columns settings)))

(define people ($3 people.russia (parse-tab-tree tabtree-file)))
(define items (sort
                people
                (λ (item1 item2)
                  (a-z ($ id item1) ($ id item2)))))

(define start-table
  #<<T
=Лица российского трансгуманизма=
{|class="gallery_table"
T
)
(define cell
  #<<T
|{{person_thumbnail
  |image=~a
  |name=[[~a]]
  |place=~a
  |job=~a
  |projects=~a
}}
T
)
(define new-row "|-")
(define end-table "|}")

(define (get-surname id)
  (let ((splitted (string-split id "_")))
    (match splitted
      ((list name surname _ ...) surname)
      ((list name) name)
      (_ (errorf "wrong id pattern for person's name: ~a" id)))))

(define (get-image-name item)
  (or (and ($ img item) (str ($ img item) ".jpg"))
      (format "~a2.jpg" (get-surname ($ id item)))))

(define (get-place item)
  (or ($ place item) ($ ht item)))

(define (get-job item)
  (let* ((former? (or ($ former-prof item) ($ former-job item)))
        (job (or ($ job item) ($ prof item))))
    (cond
      (job (namefy job))
      (former? (namefy former?)) ; Need to think what to write about former job places, now it shows like a current job
      (else #f))))

(define (get-projects item)
  (let ((res
          (cleanmap
            (flatten
              (map
                (λ (val) (and val (string-split val ",")))
                (list
                  ($ projects item)
                  ($ editor item)
                  ($ founder item)
                  ($ co-founder item)))))))
    (if (empty? res) #f res)))

(define (valid-person? item)
  (let* (
        (ignore ($ i item))
        (place (get-place item))
        (job (get-job item))
        (ddate (or ($ ddate item) ($ crdate item)))
        (projects (get-projects item)))
    (and
      (not ddate)
      (not ignore)
      (or
        (and place projects)
        (and place job)))))

(define (refery val)
  (cond
    ((not val) #f)
    ((string? val)
      (let ((vals (string-split val ",")))
        (cond
          ((empty-string? val) val)
          ((and (list? vals) (several-elements? vals)) (string-join (map refery (map namefy vals)) ", "))
          (else (format "[[~a]]" (namefy val))))))
    ((list? val)
      (string-join (map refery val) ", "))))

(define (g-template-patchwork)
  (let* ((wiki-table (for/fold
                  ((res start-table))
                  ((item (filter valid-person? items)) (i (in-naturals)))
                  (let* (
                        (image (get-image-name item))
                        (name (namefy ($ id item)))
                        (place (or (get-place item) ""))
                        (place (refery place))
                        (job (or (get-job item) ""))
                        (job (refery job))
                        (projects (or (get-projects item) ""))
                        (projects (refery projects))
                        (last-item? (last? item items))
                        (first-cell-in-a-row? (equal? (remainder (+ i 1) COLUMNS) 1))
                        )
                      (format
                        "~a~a~n~a"
                        res
                        (cond
                          (first-cell-in-a-row? (str "\n" new-row))
                          (else ""))
                        (format cell image name place job projects)))))
        (wiki-table (str wiki-table "\n" end-table)))
    (--- (str filepath "/" filename))
    (write-file (str filepath "/" filename) wiki-table)
    ))
