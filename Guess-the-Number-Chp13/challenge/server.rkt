#lang racket

;Os comentários foram os testes feitos para achar erros e bugs ao longo da implementação dos desafios easy e medium

(require 2htdp/universe "shared.rkt")

(provide launch-guess-server)

(struct game (players number))
;; (struct interval (small big))
;; 
;; (define u0 (interval LOWER UPPER))
;; 
;; (define (smaller w)
;;  (interval (interval-small w)
;;            (max (interval-small w) (sub1 (guess w)))))
;; 
;; (define (bigger w)
;;  (interval (min (interval-big w) (add1 (guess w)))
;;            (interval-big w)))
;; 
;; (define (guess w)
;;  (quotient (+ (interval-small w) (interval-big w)) 2))
;; 
;; (define (single? w)
;;  (= (interval-small w) (interval-big w)))

(define (launch-guess-server)
  (universe (game #f #f)
            (on-new connect)
            (on-msg handle-msg)))

(define (connect u client)
  (cond [(false? (game-players u))
         (make-bundle (game 1 #f) (list (make-mail client "Escolha um número de 1 a 100")) '())]
        [(and (= (game-players u) 1) (not (false? (game-number u))))
         (make-bundle (game 2 (game-number u)) (list (make-mail client "Advinhe um número de 1 a 100")) '())]
        [else (make-bundle u empty (list client))]))

(define (handle-msg u client msg)
  (cond [(= (game-players u) 1)
         (make-bundle (game 1 msg) (list (make-mail client "receive")) '())]
        [(= (game-number u) msg)
         (make-bundle u (list (make-mail client "Parabens")) '())]
        [else
         (make-bundle u (list (make-mail client (help (game-number u) msg))) '())]
        ))

(define (help number guess)
  (if (> number guess) "Maior" "Menor"))

;; (define (next-interval u msg)
;;   (cond [(not (string=? msg)) u]
;;         [(string=? "up" msg) (bigger u)]
;;         [(string=? "down" msg) (smaller u)]
;;         [else u]))