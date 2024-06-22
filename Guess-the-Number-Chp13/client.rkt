#lang racket

(require 2htdp/universe 2htdp/image "shared.rkt")

(provide launch-guess-client)

(define clientstate0 "none available")
(define SIZE 24)
(define COLOR "black")
(define MT-SC (empty-scene 800 600))

(define (launch-guess-client n)
  (big-bang clientstate0
            (to-draw draw-guess)
            (on-key handle-keys)
            (name n)
            (register LOCALHOST)
            (on-receive handle-msg)))

(define (handle-msg c msg)
  (number->string msg))

(define (handle-keys w key)
  (cond [(key=? key "up")   (make-package w "up")]
        [(key=? key "down") (make-package w "down")]
        [(key=? key "q")    (stop-with w)]
        [(key=? key "=")    (stop-with w)]
        [else w]))

(define (draw-guess c)
  (overlay (text c SIZE COLOR) MT-SC))