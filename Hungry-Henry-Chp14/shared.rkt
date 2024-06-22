#lang racket

(provide (struct-out player) (struct-out body) WIDTH HEIGHT GOTO GOTO-LENGTH SERIALIZE SCORE id? id=? PLAYER-SIZE PLAYER-FATTEN-DELTA get-score)

(struct player (id body waypoints) #:prefab)
(struct body (size loc) #:prefab #:mutable)

(define WIDTH 700)
(define HEIGHT 500)

(define GOTO 'goto)
(define GOTO-LENGTH 3)
(define SERIALIZE 'state)
(define SCORE 'score)

(define id? string?)
(define id=? string=?)

(define PLAYER-SIZE 30)
(define PLAYER-FATTEN-DELTA 5)

(define (get-score f)
  (/ (- f PLAYER-SIZE) PLAYER-FATTEN-DELTA))
