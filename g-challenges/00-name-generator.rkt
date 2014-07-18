#lang racket/base

(define first-names '("John" "Benjamin" "Alexander" "Thomas" "James" "George"))
(define last-names '("Adams" "Franklin" "Hamilton" "Jay" "Jefferson" "Washington"))

(define (create-name) (fprintf (current-output-port)
                      "~a ~a ~%" (list-ref first-names (random (length first-names)))
                                 (list-ref last-names (random (length last-names)))))

;(create-name)

