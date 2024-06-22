#lang racket

(require 2htdp/universe 2htdp/image)

(struct orc-world (player lom attack# target) #:mutable #:transparent)
(struct player (health agility strength) #:mutable #:transparent)
(struct monster (image [health #:mutable]) #:transparent)
(struct orc monster (club) #:transparent)
(struct hydra monster () #:transparent)
(struct slime monster (sliminess) #:transparent)
(struct brigand monster () #:transparent)


(define MAX-HEALTH 35)
(define MAX-AGILITY 35)
(define MAX-STRENGTH 35)

(define MONSTER-HEALTH0 20)
(define HEALING 5)
(define CLUB-STRENGTH 15)
(define STAB-DAMAGE 5)
(define FLAIL-DAMAGE 3)
(define SLIMINESS 6)
(define PLAYER-IMAGE (bitmap "graphics/player.bmp"))
(define SLIME-IMAGE (bitmap "graphics/slime.bmp"))
(define HYDRA-IMAGE (bitmap "graphics/hydra.png"))
(define ORC-IMAGE (bitmap "graphics/orc1.gif"))
(define BRIGAND-IMAGE (bitmap "graphics/brigand.bmp"))
(define HEALTH-DAMAGE -2)
(define STRENGTH-DAMAGE -2)
(define AGILITY-DAMAGE -2)
(define PER-ROW 2)
(define MONSTER# 6)
(define ATTACKS# 4)
(define LOSE "YOU LOST")
(define WIN "YOU WON!!")
(define STRENGTH-COLOR "red")
(define AGILITY-COLOR "blue")
(define HEALTH-COLOR "green")
(define MONSTER-COLOR "black")
(define STRENGTH "Força")
(define AGILITY "Agilidade")
(define HEALTH "Vida")
(define INSTRUCTION-TEXT-SIZE 20)
(define HEALTH-BAR-WIDTH 200)
(define HEALTH-BAR-HEIGHT 50)
(define ATTACK-COLOR "red")
(define MESSAGES-SIZE 20)
(define HEALTH-SIZE 20)
(define MESSAGE-COLOR "black")
(define INSTRUCTION-TEXT (text "INSTRUCTIONS" 10 "black"))
(define V-SPACER (rectangle 0 10 "solid" "white"))
(define H-SPACER (rectangle 10 0 "solid" "white"))
(define TARGET (text "Target" 20 "black"))


(define (player-update! setter selector mx)
  (lambda (player delta)
    (setter player (interval+ (selector player) delta mx))))

(define player-health+
  (player-update! set-player-health! player-health MAX-HEALTH))

(define player-agility+
  (player-update! set-player-agility! player-agility MAX-AGILITY))

(define player-strength+
  (player-update! set-player-strength! player-strength MAX-STRENGTH))


;; (define (player-health+ player delta)
;;   (define new-health (interval+ (player-health player) delta HEALTH))
;;   (set-player-health! player new-health))

(define (start)
  (big-bang (initialize-orc-world)
            (on-key player-acts-on-monsters)
            (to-draw render-orc-battle)
            (stop-when end-of-orc-battle? render-the-end)))

(define (initialize-orc-world)
  (define player0 (initialize-player))
  (define lom0 (initialize-monsters))
  (orc-world player0 lom0 (random-number-of-attacks player0) 0))

(define (end-of-orc-battle? w)
  (or (win? w) (lose? w)))

(define (render-orc-battle w)
  (render-orc-world w (orc-world-target w) (instructions w)))

(define (render-the-end w)
  (render-orc-world w #f (message (if (lose? w) LOSE WIN))))

(define (player-acts-on-monsters w k)
  (cond
    [(zero? (orc-world-attack# w)) (void)]
    [(key=? "s" k) (stab w)]
    [(key=? "h" k) (heal w)]
    [(key=? "f" k) (flail w)]
    [(key=? "e" k) (end-turn w)]
    [(key=? "n" k) (initialize-orc-world)]
    [(key=? "right" k) (move-target w +1)]
    [(key=? "left" k) (move-target w -1)]
    [(key=? "down" k) (move-target w (+ PER-ROW))]
    [(key=? "up" k) (move-target w (- PER-ROW))])
  (give-monster-turn-if-attack#=0 w)
  w)

(define (initialize-player)
  (player MAX-HEALTH MAX-AGILITY MAX-STRENGTH))

(define (random-number-of-attacks p)
  (random-quotient (player-agility p) ATTACKS#))

(define (random-quotient x y)
  (define div (quotient x y))
  (if (> 0 div) 0 (random+ (add1 div))))

(define (random+ n) (add1 (random n)))

(define (initialize-monsters)
  (build-list
   MONSTER#
   (lambda (_)
     (define health (random+ MONSTER-HEALTH0))
     (case (random 4)
       [(0) (orc ORC-IMAGE health (random+ CLUB-STRENGTH))]
       [(1) (hydra HYDRA-IMAGE health)]
       [(2) (slime SLIME-IMAGE health (random+ SLIMINESS))]
       [(3) (brigand BRIGAND-IMAGE health)]
       ))))

(define (instructions w)
  (define na (number->string (orc-world-attack# w)))
  (define ra (string-append "Restantes: " na))
  (define txt (text ra INSTRUCTION-TEXT-SIZE ATTACK-COLOR))
  (above txt INSTRUCTION-TEXT))

(define (message str) (text str MESSAGES-SIZE MESSAGE-COLOR))

(define (render-orc-world w t additional-text)
  (define i-player (render-player (orc-world-player w)))
  (define i-monster (render-monsters (orc-world-lom w) t))
  (above V-SPACER
         (beside H-SPACER
                 i-player
                 H-SPACER H-SPACER H-SPACER
                 (above i-monster
                        V-SPACER V-SPACER V-SPACER
                        additional-text)
                 H-SPACER)
         V-SPACER))

(define (render-player p)
  (define s (player-strength p))
  (define a (player-agility p))
  (define h (player-health p))
  (above/align
   "left"
   (status-bar s MAX-STRENGTH STRENGTH-COLOR STRENGTH)
   V-SPACER
   (status-bar a MAX-AGILITY AGILITY-COLOR AGILITY)
   V-SPACER
   (status-bar h MAX-HEALTH HEALTH-COLOR HEALTH)
   V-SPACER V-SPACER V-SPACER
   PLAYER-IMAGE))

(define (status-bar v-current v-max color label)
  (define w (* (/ v-current v-max) HEALTH-BAR-WIDTH))
  (define f (rectangle w HEALTH-BAR-HEIGHT "solid" color))
  (define b (rectangle HEALTH-BAR-WIDTH HEALTH-BAR-HEIGHT "outline" "black"))
  (define bar (overlay/align "left" "top" f b))
  (beside bar H-SPACER))

(define (render-monsters lom with-target)
  (define target
    (if (number? with-target)
        (list-ref lom with-target)
        'palavra-sem-sentido))

    (define (render-one-monster m)
      (define image
        (if (eq? m target)
            (overlay TARGET (monster-image m))
            (monster-image m)))
      (define health (monster-health m))
      (define health-bar
        (if (= health 0)
            (overlay (message "DEAD") (status-bar 0 1 'white ""))
            (status-bar health MONSTER-HEALTH0 MONSTER-COLOR (number->string health))))
      (beside (above health-bar image) H-SPACER H-SPACER))

    (arrange (map render-one-monster lom)))

(define (arrange lom)
  (cond
    [(empty? lom) empty-image]
    [else (define r (apply beside/align "middle" (take lom PER-ROW)))
          (above  r V-SPACER (arrange (drop lom PER-ROW)))]))

(define (win? w)
  (all-dead? (orc-world-lom w)))

(define (lose? w)
  (player-dead? (orc-world-player w)))

(define (player-dead? p)
  (or (= (player-health p) 0)
      (= (player-agility p) 0)
      (= (player-strength p) 0)))

(define (all-dead? lom)
  (not (ormap monster-alive? lom)))

(define (monster-alive? m)
  (> (monster-health m) 0))

(define (end-turn w)
  (set-orc-world-attack#! w 0))

(define (heal w)
  (decrease-attack# w)
  (player-health+ (orc-world-player w) HEALING))

(define (stab w)
  (decrease-attack# w)
  (define target
    (list-ref (orc-world-lom w) (orc-world-target w)))
  (define damage
    (random-quotient (player-strength (orc-world-player w))
                     STAB-DAMAGE))
  (damage-monster target damage))

(define (flail w)
  (decrease-attack# w)
  (define target (current-target w))
  (define alive (filter monster-alive? (orc-world-lom w)))
  (define pick#
    (min
     (random-quotient (player-strength (orc-world-player w))
                      FLAIL-DAMAGE)
     (length alive)))
  (define getem (cons target (take alive pick#)))
  (for-each (lambda (m) (damage-monster m 1)) getem))

(define (decrease-attack# w)
  (set-orc-world-attack#! w (sub1 (orc-world-attack# w))))

(define (damage-monster m delta)
  (set-monster-health! m (interval- (monster-health m) delta)))

(define (current-target w)
  (list-ref (orc-world-lom w) (orc-world-target w)))

(define (move-target w delta)
  (define new (+ (orc-world-target w) delta))
  (set-orc-world-target! w (modulo new MONSTER#)))

(define (give-monster-turn-if-attack#=0 w)
  (when (zero? (orc-world-attack# w))
    (define player (orc-world-player w))
    (all-monsters-attack-player player (orc-world-lom w))
    (set-orc-world-attack#! w (random-number-of-attacks player))))

(define (all-monsters-attack-player player lom)
  
  (define (one-monster-attacks-player m)
    (cond
      [(orc? m)
       (player-health+ player  (random- (orc-club m)))]
      [(hydra? m)
       (player-health+ player  (random- (monster-health m)))]
      [(slime? m)
       (player-health+ player -1)
       (player-agility+ player
                        (random- (slime-sliminess m)))]
      [(brigand? m)
       (case (random 3)
         [(0) (player-health+ player  HEALTH-DAMAGE)]
         [(1) (player-agility+ player AGILITY-DAMAGE)]
         [(2) (player-strength+ player STRENGTH-DAMAGE)])]))
  (define live-monsters (filter monster-alive? lom))
  (for-each one-monster-attacks-player live-monsters))

(define (interval+ min delta max)
  (define sum (+ min delta))
  (cond [(< sum 0) 0]
        [(> sum max) max]
        [else sum]))


(define (interval- hp delta)
  (define sub (- hp delta))
  (if (< sub 0) 0 sub))

(define (random- dmg)
  (- 0 (random 1 (+ 1 dmg))))

(start)