#lang racket/base

(define (higher-lower n guess)
  (cond [(= guess n) (displayln "You win!")]
        [(< guess n) (begin
                       (displayln "Higher!")
                       (higher-lower n (read)))]
        [(> guess n) (begin
                       (displayln "Lower!")
                       (higher-lower n (read)))]))

(define (play)
  (begin
    (displayln "Enter a value from 0 to 100!")
    (higher-lower (random 100) (read))))

;(play)
