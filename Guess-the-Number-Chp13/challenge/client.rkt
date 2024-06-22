#lang racket

(require 2htdp/universe 2htdp/image "shared.rkt")

(provide launch-guess-client)

(struct clientstate (type msg))
(define clientstate0 (clientstate "" "none available"))
(define SIZE 34)
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
  (cond [(string=? "receive" msg)
          (clientstate "" "Recebido")]
        [(string=? "Parabens" msg)
         (clientstate "" "Você venceu!!")]
        [else
         (clientstate (clientstate-type c) msg)]))

(define (handle-keys w key)
  (cond [(member key '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))
         (clientstate (string-append (clientstate-type w) key) (clientstate-msg w))]
        [(key=? key "\b")
         (clientstate (remove-last (clientstate-type w)) (clientstate-msg w))]
        [(key=? key "\r")
         (make-package (clientstate "" (clientstate-msg w)) (string->number (clientstate-type w)))]
        [else w]))

(define (remove-last str)
  (define lgt (string-length str))
  (substring str 0 (- lgt 1)))

(define (draw-guess c)
  (define type (text (clientstate-type c) SIZE COLOR))
  (define msg (text (clientstate-msg c) SIZE COLOR))
  (define instruction (above msg type))
  (overlay instruction MT-SC))