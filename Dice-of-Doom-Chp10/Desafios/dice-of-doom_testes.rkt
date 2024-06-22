#lang racket

;Os comentários foram os testes feitos para achar erros e bugs ao longo da implementação dos desafios

(require 2htdp/universe 2htdp/image net/url)

(struct dice-world (src board gt) #:transparent)
(struct territory (index player dice x y) #:transparent)
(struct game (board player moves))
(struct move (action gt) #:transparent)

(define BOARD 2)
(define X-OFFSET (image-height (rotate 30 (regular-polygon 80 6 "solid" "black"))))
(define Y-OFFSET (* (image-height (rotate 30 (regular-polygon 80 6 "solid" "black"))) 3/4))
(define GRID 6);(* BOARD BOARD))
(define INIT-PLAYER 0)
(define INIT-SPARE-DICE 10)
(define TEXT-SIZE 25)
(define TEXT-COLOR "black")
(define WIDTH (+ (* 160 BOARD) 700 X-OFFSET))
(define HEIGHT 720)
(define INFO-X-OFFSET (+ 150 X-OFFSET))
(define INFO-Y-OFFSET 150); (* BOARD 50))
(define FOCUS (rotate 30 (regular-polygon 80 6 "outline" "black")))
(define COLORS '("red" "blue" "yellow"))
(define PLAYER# 2)
(define DICE# 4)
(define OFFSET0 150)
(define PLAIN (empty-scene WIDTH HEIGHT))
;(define ISCENE (place-image (text INST TEXT-SIZE TEXT-COLOR) (* .5 WIDTH) (* .9 HEIGHT) PLAIN))
(define roll "")
;(define INST (string-append "Setinhas para mover | Enter para marcar \nD para desmarcar     | P para passar \n" roll))


(define dice1 (bitmap "graphics/dice1.png"))
(define dice2 (bitmap "graphics/dice2.png"))
(define dice3 (bitmap "graphics/dice3.png"))
(define dice4 (bitmap "graphics/dice4.png"))
(define IMG-LIST (list dice1 dice2 dice3 dice4))

(define (roll-the-dice)
  (big-bang (create-world-of-dice-and-doom)
            (on-key interact-with-board)
            (on-draw draw-dice-world)
            (stop-when no-more-moves-in-world? draw-end-of-dice-world)))

(define (create-world-of-dice-and-doom)
  (define board (territory-build))
  (define gamet (game-tree board INIT-PLAYER INIT-SPARE-DICE))
  (define new-world (dice-world #f board gamet))
  (if (no-more-moves-in-world? new-world)
      (create-world-of-dice-and-doom)
      new-world))

(define (no-more-moves-in-world? w)
  (define tree (dice-world-gt w))
  (define board (dice-world-board w))
  (define player (game-player tree))
  (or (no-more-moves? tree)
      (for/and ((t board)) (= (territory-player t) player))))

(define (no-more-moves? tree)
  ;(print (game-moves tree))
  ;(newline)
  ;(empty? (game-moves tree)))
  ;(for/and ((t (game-moves tree)))
    ;(empty? (move-action t))))
  (empty? (game-moves tree)))

(define (draw-end-of-dice-world w)
  (define board (dice-world-board w))
  (define message (text (won board) TEXT-SIZE TEXT-COLOR))
  (define background PLAIN)
  (overlay message background))

(define (draw-dice-world w)
  (define INST (string-append "Setinhas para mover | Enter para marcar \nD para desmarcar     | P para passar \n" roll))
  (define ISCENE (place-image (text INST TEXT-SIZE TEXT-COLOR) (* .75 WIDTH) (* .8 HEIGHT) PLAIN))
  (add-player-info
   (game-player (dice-world-gt w)) w
   (add-board-to-scene w ISCENE)))

(define (interact-with-board w k)
  (cond [(key=? "left" k)
         (refocus-board w left)]
        [(key=? "right" k)
         (refocus-board w right)]
        [(key=? "p" k)
         (pass w)]
        [(key=? "\r" k)
         (mark w)]
        [(key=? "d" k)
         (unmark w)]
        [else w]))

(define (chance ds dt)
  (number->string (round (* 100 (/ ds (+ ds dt))))))

(define (add-player-info player w s)
  (define board (dice-world-board w))
  (define source (dice-world-src w))
  (define dicesource (if source (territory-dice (find-terry board source)) #f))
  (define dicetarget (territory-dice (first board)))
  (define chc (if source (chance dicesource dicetarget) "?"))
  (define str (string-append (whose-turn player) "\nChance de sucesso: " chc "%"))
  (define txt (text str TEXT-SIZE TEXT-COLOR))
  (place-image txt (- WIDTH INFO-X-OFFSET) INFO-Y-OFFSET s))

(define (whose-turn player)
  (define p (number->string player))
  (string-append "Player " p))

(define (add-board-to-scene w s)
  (define board (dice-world-board w))
  (define player (game-player (dice-world-gt w)))
  (define focus? (dice-world-src w))
  (define trtry1 (first board))
  (define p-focus (territory-player trtry1))
  (define t-image (draw-territory trtry1))
  (define image (draw-focus focus? p-focus player t-image))
  (define base-s (add-territory trtry1 image s))
  (for/fold ([sb base-s]) ([t (rest board)])
    (add-territory t (draw-territory t) sb)))

(define (draw-focus marked? p-in-focus p t-image)
  (if (or (and (not marked?) (= p-in-focus p))
          (and marked? (not (= p-in-focus p))))
      (overlay FOCUS t-image)
      t-image))

(define (add-territory t image scene)
  (place-image image (territory-x t) (territory-y t) scene))

(define (draw-territory t)
  (define color (color-chooser (territory-player t)))
  (overlay (draw-dice (territory-dice t)) (rotate 30 (regular-polygon 80 6 "solid" color))))

(define (color-chooser n)
  (list-ref COLORS n))

(define (draw-dice n)
  (define first-dice (get-dice-img 0))
  (define height-dice (image-height first-dice))
  (for/fold ([s first-dice]) ([i (- n 1)])
    (define dice-image (get-dice-img (+ i 1)))
    (define y-offset (* height-dice (+ .5 (* i .25))))
    (overlay/offset s 0 y-offset dice-image)))

(define (get-dice-img i)
  (list-ref IMG-LIST (modulo i (length IMG-LIST))))

(define (refocus-board w direction)
  (define source (dice-world-src w))
  (define board (dice-world-board w))
  (define tree (dice-world-gt w))
  (define player (game-player tree))
  (define (owner? tid)
    (if source (not (= tid player)) (= tid player)))
  (define new-board (rotate-until owner? board direction))
  (dice-world source new-board tree))

(define (rotate-until owned-by board rotate)
  (define next-list (rotate board))
  (if (owned-by (territory-player (first next-list)))
      next-list
      (rotate-until owned-by next-list rotate)))

(define (left l)
  (append (rest l) (list (first l))))

(define (right l)
  (reverse (left (reverse l))))

(define (pass w)
  (define m (find-moves (game-moves (dice-world-gt w)) '()))
  (cond [(not m) w]
        [else (dice-world #f (game-board m) m)]))

(define (find-moves moves action)
  (define m
    (findf (lambda (m) (equal? (move-action m) action)) moves))
  (and m (move-gt m)))

(define (mark w)
  (define tree (dice-world-gt w))
  (define board (dice-world-board w))
  (define source (dice-world-src w))
  (define focus (territory-index (first board)))
  (if source
      (attacking w source focus)
      (dice-world focus board tree)))

(define (rolledDices lst)
  (define a (for/list ([i '(1 2 3 4 5 6 7 8 9 10)] [l lst])
    (string-append " |Dado " (number->string i) ": " (number->string l))))
  (foldr string-append "" a))
  
(define (get-random-numbers n)
  (for/list ([i n]) (random 1 (add1 6))))

(define (damageterritory board source)
  (for/list ([i board])
    (if (= (territory-index i) source)
        (territory (territory-index i) (territory-player i) 1 (territory-x i) (territory-y i))
        i)))  

(define (sum lst) (for/fold ([sum 0]) ([i lst]) (+ sum i)))

(define (find-terry board s)
  (findf (lambda (i) (= (territory-index i) s)) board))

(define (attacking w source target)
  (define board (dice-world-board w))
  (define gt (dice-world-gt w))
  (define player (game-player gt))
  (define oponent (switch player))
  (define dicesource (territory-dice (find-terry board source)))
  (define dicetarget (territory-dice (first board)))
  (define rollssource (get-random-numbers dicesource))
  (define rollstarget (get-random-numbers dicetarget))
  (define sourcenumber (sum rollssource))
  (define targetnumber (sum rollstarget))
  (define winner (if (> sourcenumber targetnumber) (whose-turn player) (whose-turn oponent)))
  (set! roll (string-append "--Números obtidos nos dados anteriores--\n-> " (whose-turn player) " " (rolledDices rollssource) " \n-> "(whose-turn oponent) " " (rolledDices rollstarget) "\n-->Ganhador: " winner "\n (Se o movimento é permitido, o território foi tomado)"))
  (define feasible (game-moves (dice-world-gt w)))
  (define attack (list source target))
  (define next (find-moves feasible attack))
  (define nt (damageterritory board source))
  (define fail (game-tree nt oponent INIT-SPARE-DICE)) 
  (if (>= sourcenumber targetnumber)
      (if next (dice-world #f (game-board next) next) w)
      
      (if fail (dice-world #f nt fail) (stop-with w))))

(define (unmark w)
  (dice-world #f (dice-world-board w) (dice-world-gt w)))

(define (territory-build)
  (for/list ([n (in-range GRID)])
    (territory n (modulo n PLAYER#) (dice) (get-x n) (get-y n))))

(define (dice)
  (add1 (random DICE#)))

(define (get-x n)
  (+ OFFSET0
     (if (odd? (get-row n)) 0 (/ X-OFFSET 2))
     (* X-OFFSET (modulo n BOARD))))

(define (get-y n)
  (+ OFFSET0 (* Y-OFFSET (get-row n))))

(define (get-row pos)
  (quotient pos BOARD))

(define (game-tree board player dice)
  (define (attacks board)
    (for*/list ([src board]
                [dst (neighbours (territory-index src))]
                #:when (attackable? board player src dst))
      (define from (territory-index src))
      (define dice (territory-dice src))
      (define newb (execute board player from dst dice))
      (define more (cons (passes newb) (attacks newb)))
      (move (list from dst) (game newb player more))))

  (define (passes board)
    (define-values (new-dice newb) (distribute board player dice))
    (move '() (game-tree newb (switch player) new-dice)))

  (game board player (attacks board)))

(define (switch player)
  (modulo (add1 player) PLAYER#))

(define (distribute board player spare-dice)
  (for/fold ([dice spare-dice] [new-board '()]) ([t board])
    (if (and (= (territory-player t) player)
             (< (territory-dice t) DICE#)
             (not (zero? dice)))
    (values (- dice 1) (cons (add-dice-to t) new-board))
    (values dice (cons t new-board)))))

(define (add-dice-to t)
  (territory-set-dice t (add1 (territory-dice t))))

(define (territory-set-dice t dice)
  (territory (territory-index t) (territory-player t) dice (territory-x t) (territory-y t)))

(define (add b x)
  (if b empty (list x)))

(define (neighbours pos)
  (define top? (< pos BOARD))
  (define bottom? (= (get-row pos) (sub1 BOARD)))
  (define even-row? (zero? (modulo (get-row pos) 2)))
  (define right? (zero? (modulo (add1 pos) BOARD)))
  (define left? (zero? (modulo pos BOARD)))
  (if even-row?
      (even-row pos top? bottom? right? left?)
      (odd-row pos top? bottom? right? left?)))

(define (even-row pos top? bottom? right? left?)
  (append (add (or top? right?) (add1 (- pos BOARD)))
          (add (or bottom? right?) (add1 (+ pos BOARD)))
          (add top? (- pos BOARD))
          (add bottom? (+ pos BOARD))
          (add right? (add1 pos))
          (add left? (sub1 pos))))

(define (odd-row pos top? bottom? right? left?)
  (append (add (or top? left?) (sub1 (- pos BOARD)))
          (add (or bottom? left?) (sub1 (+ pos BOARD)))
          (add top? (add1 (- pos BOARD)))
          (add bottom? (add1 (+ pos BOARD)))
          (add right? (add1 pos))
          (add left? (sub1 pos))))

(define (attackable? board player src dst)
  (define dst-t
    (findf (lambda (t) (= (territory-index t) dst)) board))
  (and dst-t
       (= (territory-player src) player)
       (not (= (territory-player dst-t) player))
       (> (territory-dice src) (territory-dice dst-t))))

(define (execute board player src dst dice)
  (for/list ([t board])
    (define idx (territory-index t))
    (cond [(= idx src) (territory-set-dice t 1)]
          [(= idx dst)
           (define s (territory-set-dice t (- dice 1)))
           (territory-set-player s player)]
          [else t])))

(define (territory-set-player s player)
  (territory (territory-index s) player (territory-dice s) (territory-x s) (territory-y s)))

(define (won board)
  (define-values (best-score w) (winners board))
  ;(print w)
  (if (cons? (rest w)) "It's a tie." (string-append (whose-turn (first w)) " wins!!")))

(define (winners board)
  (for/fold ([best 0] [winners '()]) ([p PLAYER#])
    (define p-score (sum-territory board p))
    (cond [(> p-score best) (values p-score (list p))]
          [(< p-score best) (values best winners)]
          [(= p-score best) (values best (cons p winners))])))

(define (sum-territory board player)
  (for/fold ([result 0]) ([t board])
    (if (= (territory-player t) player) (+ result 1) result)))

(roll-the-dice)