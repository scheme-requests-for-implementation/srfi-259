;;; SPDX-FileCopyrightText: 2025 Daphne Preston-Kendal
;;; SPDX-License-Identifier: MIT

(library (srfi :259 tagged-procedures)
  (export define-procedure-tag)
  (import (chezscheme))

  (define-record-type (tag-set remake-tag-set tag-set?)
    (fields tags)
    (nongenerative tag-set-ptDJzarolIlYyArpTOknFRbNx5M)
    (opaque #t)
    (sealed #t))

  (define (make-tag-set) (remake-tag-set (make-eq-hashtable)))

  (define (make-tagged-procedure underlying-proc tag-key tag-value)
    (assert (procedure? underlying-proc))
    (if (and (wrapper-procedure? underlying-proc)
             (tag-set? (wrapper-procedure-data underlying-proc)))
        (let ((underlying-proc
               (wrapper-procedure-procedure underlying-proc))
              (tag-set
               (wrapper-procedure-data underlying-proc)))
          (make-wrapper-procedure underlying-proc
                                  (procedure-arity-mask underlying-proc)
                                  (add-procedure-tag tag-set tag-key tag-value)))
        (make-tagged-procedure (make-wrapper-procedure underlying-proc
                                                       (procedure-arity-mask underlying-proc)
                                                       (make-tag-set))
                               tag-key
                               tag-value)))

  (define (add-procedure-tag tag-set tag-key tag-value)
    (remake-tag-set
     (hashtable-copy (let ((tag-set-table
                            (hashtable-copy (tag-set-tags tag-set) #t)))
                       (hashtable-set! tag-set-table tag-key tag-value)
                       tag-set-table)
                     #f)))

  (define-syntax define-procedure-tag
    (syntax-rules ()
      ((_ constructor-name predicate-name accessor-name)
       (and (identifier? #'constructor-name)
            (identifier? #'predicate-name)
            (identifier? #'accessor-name))
       (begin
         (define tag-key (cons 'constructor-name '()))
         (define (constructor-name tag proc)
           (make-tagged-procedure proc tag-key tag))
         (define (predicate obj)
           (and (wrapper-procedure? obj)
                (tag-set? (wrapper-procedure-data obj))
                (hashtable-contains? (tag-set-tags
                                      (wrapper-procedure-data obj))
                                     tag-key)))
         (define predicate-name predicate)
         (define (accessor-name proc)
           (if (not (predicate proc))
               (assertion-violation 'accessor-name
                                    "not a tagged procedure in the protocol for this accessor"
                                    proc))
           (hashtable-ref (tag-set-tags (wrapper-procedure-data proc))
                          tag-key
                          #f)))))))
