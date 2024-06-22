#lang racket

(require 2htdp/universe 2htdp/image)
(require 2htdp/image
         (only-in racket/gui/base play-sound))


(define DIMX 30)
(define DIMY 20)
(define TICK-RATE 0.3)

(define SIZE 18)
(define SEG-SIZE 32)

(define MAX-GOO 5)
(define EXPIRATION-TIME 30)

(struct pit (snake goos snake2) #:transparent)
(struct snake (dir segs eaten) #:transparent)
(struct posn (x y) #:transparent)
(struct goo (loc expire) #:transparent)

(define WIDTH-PX (* SEG-SIZE DIMX))
(define HEIGHT-PX (* SEG-SIZE DIMY))

(define ENDGAME-TEXT-SIZE 80)

(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))
(define GOOD-SCENE (place-image (bitmap "graphics/cenario1.jpg") (/ WIDTH-PX 2) (/ HEIGHT-PX 2) MT-SCENE))
(define GOO-IMG (bitmap "graphics/apple_resized.png"))
(define SEG-IMG (bitmap "graphics/BodyPng.png"))
(define HEAD-IMG (bitmap "graphics/HeadPng.png"))
;(define SNAKE-DEAD (place-image (bitmap "graphics/game-over3.jpg") (/ WIDTH-PX 2) (/ HEIGHT-PX 2) MT-SCENE))

(define HEAD-LEFT-IMG HEAD-IMG)
(define HEAD-DOWN-IMG (rotate 90 HEAD-LEFT-IMG))
(define HEAD-RIGHT-IMG (flip-horizontal HEAD-LEFT-IMG))
(define HEAD-UP-IMG (flip-vertical HEAD-DOWN-IMG))

(define (can-eat snake goos)
  (cond [(empty? goos) '()]
        [(close? (first (snake-segs snake)) (first goos)) (list (first goos))]
        [else (can-eat snake (rest goos))]
        ))

(define (close? snake-seg goos)
  (posn=? snake-seg (goo-loc goos)))

(define (posn=? a b)
  (and (= (posn-x a) (posn-x b))
       (= (posn-y a) (posn-y b))
                                ))

(define (next-pit w)
  (define snake (pit-snake w))
  (define snak2 (pit-snake2 w))
  (define gos (pit-goos w))
  (define snakGoo (can-eat snake gos))
  (define snak2Goo (can-eat snak2 gos))
  (define goo-to-eat (cons snakGoo snak2Goo))
  (cond [(and (cons? snakGoo) (cons? snak2Goo))
        (pit (grow snake) (age-goo (eat gos goo-to-eat)) (grow snak2))]
        [(and (cons? snakGoo) (empty? snak2Goo))
        (pit (grow snake) (age-goo (eat gos goo-to-eat)) (slither snak2))]
        [(and (empty? snakGoo) (cons? snak2Goo))
        (pit (slither snake) (age-goo (eat gos goo-to-eat)) (grow snak2))]
        [(and (empty? snakGoo) (empty? snak2Goo))
        (pit (slither snake) (age-goo gos) (slither snak2))]))

(define (eat goos goo-to-eat)
  (cons (fresh-goo) (remove* goo-to-eat goos)))

(define (grow snak)
  (snake (snake-dir snak)
         (cons (next-head snak) (snake-segs snak))
         (add1 (snake-eaten snak))))

(define (slither snak)
  (snake (snake-dir snak)
         (cons (next-head snak) (all-but-last (snake-segs snak)))
         (snake-eaten snak)))

(define (all-but-last segs)
  (cond [(empty? (rest segs)) '()]
        [else (cons (first segs) (all-but-last (rest segs)))]))

(define (next-head snak)
  (define head (snake-head snak))
  (define dir (snake-dir snak))
  (cond [(or (string=? dir "up") (string=? dir "w")) (posn-move head 0 -1)]
        [(or (string=? dir "down") (string=? dir "s")) (posn-move head 0 1)]
        [(or (string=? dir "left") (string=? dir "a"))(posn-move head -1 0)]
        [(or (string=? dir "right") (string=? dir "d")) (posn-move head 1 0)]))

(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))

(define (age-goo goos)
  (rot (renew goos)))

(define (rot goos)
  (map decay goos))

(define (decay go)
  (goo (goo-loc go) (sub1 (goo-expire go))))

(define (renew goos)
  (map (lambda (x) (if (rotten? x) (fresh-goo) x)) goos))

(define (rotten? goo)
  (if (zero? (goo-expire goo)) #t #f))

(define (fresh-goo)
  (goo (posn (add1 (random (sub1 DIMX)))
             (add1 (random (sub1 DIMY))))
       EXPIRATION-TIME))

(define (direct-snake w key)
  (cond [(dir? key) (world-change-dir w key)]
        [else w]))

(define (dir? x)
  (or (key=? x "up")
      (key=? x "down")
      (key=? x "left")
      (key=? x "right")
      (key=? x "w")
      (key=? x "s")
      (key=? x "a")
      (key=? x "d")))

(define (world-change-dir w d)
  (define snake (pit-snake w))
  (define snak2 (pit-snake2 w))
  (cond [(and (or (opposite-dir? (snake-dir snak2) d) (opposite-dir? (snake-dir snake) d))
              (or (cons? (rest (snake-segs snak2))) (cons? (rest (snake-segs snake)))))
         (stop-with w)]
        [else
         (if (or (string=? d "up") (string=? d "down") (string=? d "left") (string=? d "right"))
         (pit (snake-change-dir snake d)
              (pit-goos w)
               snak2)
         (pit snake 
              (pit-goos w)
              (snake-change-dir snak2 d)))]))

(define (opposite-dir? d1 d2)
  (cond [(string=? d1 "up") (string=? d2 "down")]
        [(string=? d1 "down") (string=? d2 "up")]
        [(string=? d1 "left") (string=? d2 "right")]
        [(string=? d1 "right") (string=? d2 "left")]
        [(string=? d1 "w") (string=? d2 "s")]
        [(string=? d1 "s") (string=? d2 "w")]
        [(string=? d1 "a") (string=? d2 "d")]
        [(string=? d1 "d") (string=? d2 "a")]))

(define (snake-head snake)
  (first (snake-segs snake)))

(define (snake-body snake)
  (rest (snake-segs snake)))

(define (snake-tail snake)
  (last (snake-segs snake)))

(define (snake+scene snake scene)
  (define snake-placar
    (img+scene (posn 1 1) (text (number->string (snake-eaten snake)) 28 "red") scene))
  (define snake-body-scene
    (img-list+scene (snake-body snake) SEG-IMG snake-placar))
  (define dir (snake-dir snake))
  (img+scene (snake-head snake)
             (cond [(or (string=? "up" dir) (string=? "w" dir)) HEAD-UP-IMG]
                   [(or (string=? "down" dir) (string=? "s" dir)) HEAD-DOWN-IMG]
                   [(or (string=? "left" dir) (string=? "a" dir)) HEAD-LEFT-IMG]
                   [(or (string=? "right" dir) (string=? "d" dir)) HEAD-RIGHT-IMG])
             snake-body-scene))

(define (render-pit w)
  (snake+scene (pit-snake w)
               ;(snake+scene (pit-snake2 w)
                            (goo-list+scene (pit-goos w) GOOD-SCENE)))


 (define (img-list+scene posn img scene)
   (cond [(empty? posn) scene]
         [else (img+scene
                (first posn)
                img
                (img-list+scene (rest posn) img scene))]))

(define (img+scene posn img scene)
  (place-image img
               (* (posn-x posn) SEG-SIZE)
               (* (posn-y posn) SEG-SIZE)
               scene))


(define (goo-list+scene goos scene)
  (define (get-posn-from-goo goos)
    (cond [(empty? goos) empty]
          [else (cons (goo-loc (first goos))
                      (get-posn-from-goo (rest goos)))]))
  (img-list+scene (get-posn-from-goo goos) GOO-IMG scene))
      
(define (dead? w)
  (define snake (pit-snake w))
  (define snak2 (pit-snake2 w))
  (or (self-colliding? snake) (wall-colliding? snake) (self-colliding? snak2) (wall-colliding? snak2) (coliding? snake snak2)))

(define (render-end w)
  (play-sound "graphics/game-over.wav" #t)
  (place-image/align (bitmap "graphics/game-over3.jpg") (- (/ WIDTH-PX 2) 25) (+ (/ HEIGHT-PX 2) 150) "center" "center" MT-SCENE))

(define (self-colliding? snak)
  (cons? (member (snake-head snak) (snake-body snak))))

(define (wall-colliding? snak)
  (define x (posn-x (snake-head snak)))
  (define y (posn-y (snake-head snak)))
  (or (= 0 x) (= 0 y) (= x DIMX) (= y DIMY)))


(define (snake-change-dir snak d)
  (snake d (snake-segs snak) (snake-eaten snak)))

(define (cria-lista-goos n)
  (if (zero? n) empty
      (cons (fresh-goo) (cria-lista-goos (- n 1)))))

(define (touchSnak snk1 snk2)
  (cons? (member (snake-head snk1) (snake-segs snk2))))

(define (coliding? snake1 snake2)
  (or (touchSnak snake1 snake2) (touchSnak snake2 snake1)))
        
    
(define (start-snake)
  (big-bang (pit (snake "right" (list (posn 3 3)) 0)
                 (cria-lista-goos (+ 3 (random 8)))
                 (snake "d" (list (posn 3 10)) 0))
            (on-tick next-pit TICK-RATE)
            (on-key direct-snake)
            (to-draw render-pit)
            (stop-when dead? render-end)))

(start-snake)

