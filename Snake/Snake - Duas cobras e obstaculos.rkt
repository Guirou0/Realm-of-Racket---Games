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
(define MAX-OBSTACLES 5)
(define MIN-OBSTACLES 3)
(define EXPIRATION-TIME 30)
(define LEN-OBSTACLE 4)

(struct pit (snake goos snake2 obstacles) #:transparent)
(struct snake (dir segs eaten) #:transparent)
(struct posn (x y) #:transparent)
(struct goo (loc expire) #:transparent)
(struct obstacle (segs))

(define WIDTH-PX (* SEG-SIZE DIMX))
(define HEIGHT-PX (* SEG-SIZE DIMY))

(define ENDGAME-TEXT-SIZE 80)

(define OBSTACLE-IMG (bitmap "graphics/obstacle.png"))
(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))
(define MT-SCENE2 (empty-scene SEG-SIZE SEG-SIZE))
(define GOOD-SCENE (place-image/align (scale 1.1 (bitmap "graphics/cenario1.jpg")) (/ WIDTH-PX 2) (/ HEIGHT-PX 2) "center" "center" MT-SCENE))
(define GOO-IMG (bitmap "graphics/apple_resized.png"))
(define SEG-IMG (bitmap "graphics/BodyPng.png"))
(define SEG-IMG2 (bitmap "graphics/Body2Png.png"))
(define HEAD-IMG (bitmap "graphics/HeadPng.png"))
(define HEAD-IMG2 (bitmap "graphics/Head2Png.png"))
;(define SNAKE-DEAD (place-image (bitmap "graphics/game-over3.jpg") (/ WIDTH-PX 2) (/ HEIGHT-PX 2) MT-SCENE))

(define HEAD-LEFT-IMG HEAD-IMG)
(define HEAD-DOWN-IMG (rotate 90 HEAD-LEFT-IMG))
(define HEAD-RIGHT-IMG (flip-horizontal HEAD-LEFT-IMG))
(define HEAD-UP-IMG (flip-vertical HEAD-DOWN-IMG))

(define HEAD-LEFT-IMG2 HEAD-IMG2)
(define HEAD-DOWN-IMG2 (rotate 90 HEAD-LEFT-IMG2))
(define HEAD-RIGHT-IMG2 (flip-horizontal HEAD-LEFT-IMG2))
(define HEAD-UP-IMG2 (flip-vertical HEAD-DOWN-IMG2))

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
  (define obst (pit-obstacles w))
  (cond [(and (cons? snakGoo) (cons? snak2Goo))
        (pit (grow snake) (age-goo (eat gos goo-to-eat obst) obst) (grow snak2) obst)]
        [(and (cons? snakGoo) (empty? snak2Goo))
        (pit (grow snake) (age-goo (eat gos goo-to-eat obst) obst) (slither snak2) obst)]
        [(and (empty? snakGoo) (cons? snak2Goo))
        (pit (slither snake) (age-goo (eat gos goo-to-eat obst) obst) (grow snak2) obst)]
        [(and (empty? snakGoo) (empty? snak2Goo))
        (pit (slither snake) (age-goo gos obst) (slither snak2) obst)]))

(define (eat goos goo-to-eat obst)
  (cons (fresh-goo obst) (remove* goo-to-eat goos)))

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

(define (age-goo goos obst)
  (rot (renew goos obst)))

(define (rot goos)
  (map decay goos))

(define (decay go)
  (goo (goo-loc go) (sub1 (goo-expire go))))

(define (renew goos obst)
  (cond [(empty? goos) empty]
        [else (map (lambda (x) (if (rotten? x) (fresh-goo obst) x)) goos)]))

(define (rotten? goo)
  (if (zero? (goo-expire goo)) #t #f))

(define (list-obstacles n s1 s2)
  (if (= n 0)
      '()
      (cons (fresh-obstacle s1 s2) (list-obstacles (sub1 n) s1 s2))))

(define (obstacles-number)
  (max MIN-OBSTACLES (add1 (random MAX-OBSTACLES))))

(define (build-obstacle len dir pos)
  (if (= len 0) '()
      (cons pos (build-obstacle (sub1 len) dir (next-pos pos dir)))))

(define (next-pos pos dir)
  (cond [(string=? dir "vert") (posn-move pos 1 0)]
        [(string=? dir "horiz") (posn-move pos 0 1)]))

(define (fresh-obstacle s1 s2)
  (return-obstacle LEN-OBSTACLE (return-dir (random 2)) s1 s2))

(define (return-dir n)
  (cond [(= n 0) "horiz"]
        [(= n 1) "vert"]))

(define (return-obstacle len dir s1 s2)
  (define posOb (pos-obstacle s1 s2 dir))
  (build-obstacle len dir posOb))

(define (pos-obstacle s1 s2 dir)
  (define pos (posn (add1 (random (sub1 DIMX)))
                    (add1 (random (sub1 DIMY)))))
  (define obst (build-obstacle LEN-OBSTACLE dir pos))
  ;(print pos)
  (if (or (cons? (member (snake-head s1) obst)) (cons? (member (snake-head s2) obst)))
      (pos-obstacle s1 s2 dir)
      pos))

(define (fresh-goo obst)
  (define pos (posn (add1 (random (sub1 DIMX))) (add1 (random (sub1 DIMY)))))
  (if (pos-obsts-colliding? pos obst) 
      (fresh-goo obst)
      (goo pos EXPIRATION-TIME)))

(define (pos-obsts-colliding? pos obst)
  (cond [(empty? obst) #f]
        [(pos-obst-colliding? pos (first obst)) #t]
        [else (pos-obsts-colliding? pos (rest obst))]))

        
(define (pos-obst-colliding? pos obst)
     (cond [(empty? obst) #f]
           [(posn=? pos (first obst)) #t]
           [else (pos-obst-colliding? pos (rest obst))]))
  
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
               snak2
               (pit-obstacles w))
         (pit snake 
              (pit-goos w)
              (snake-change-dir snak2 d)
              (pit-obstacles w)))]))

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

(define (snake+scene snake pos color scene)
  (define snake-placar
    (img+scene pos (overlay (text (number->string (snake-eaten snake)) 28 color) MT-SCENE2) scene))
  (define snake-body-scene
    (if (string=? color "blue")
    (img-list+scene (snake-body snake) SEG-IMG snake-placar)
    (img-list+scene (snake-body snake) SEG-IMG2 snake-placar)))
  (define dir (snake-dir snake))
  (if (string=? color "blue")
  (img+scene (snake-head snake)
             (cond [(or (string=? "up" dir) (string=? "w" dir)) HEAD-UP-IMG]
                   [(or (string=? "down" dir) (string=? "s" dir)) HEAD-DOWN-IMG]
                   [(or (string=? "left" dir) (string=? "a" dir)) HEAD-LEFT-IMG]
                   [(or (string=? "right" dir) (string=? "d" dir)) HEAD-RIGHT-IMG])
             snake-body-scene)
  (img+scene (snake-head snake)
             (cond [(or (string=? "up" dir) (string=? "w" dir)) HEAD-UP-IMG2]
                   [(or (string=? "down" dir) (string=? "s" dir)) HEAD-DOWN-IMG2]
                   [(or (string=? "left" dir) (string=? "a" dir)) HEAD-LEFT-IMG2]
                   [(or (string=? "right" dir) (string=? "d" dir)) HEAD-RIGHT-IMG2])
             snake-body-scene)))

(define (render-pit w)
  (snake+scene (pit-snake w) (posn 1 1) "blue"
               (snake+scene (pit-snake2 w) (posn 29 1) "green"
                            (goo-list+scene (pit-goos w)
                                            (obstacle-list+scene (pit-obstacles w) GOOD-SCENE)))))


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

(define (obstacle-list+scene obs scene)
  (cond [(empty? obs) scene]
        [else (img-list+scene
               (first obs)
               OBSTACLE-IMG
               (obstacle-list+scene (rest obs) scene))]))

(define (dead? w)
  (define snake (pit-snake w))
  (define snak2 (pit-snake2 w))
  (define obs (pit-obstacles w))
  (or (self-colliding? snake) (wall-colliding? snake) (self-colliding? snak2) (wall-colliding? snak2) (coliding? snake snak2) (obstacles-colliding? snake obs) (obstacles-colliding? snak2 obs)))

(define (render-end w)
  (play-sound "graphics/game-over.wav" #t)
  (cond [(> (snake-eaten (pit-snake w)) (snake-eaten (pit-snake2 w))) 
              (place-image/align (bitmap "graphics/game-over-blue2-resize.jpg") (/ WIDTH-PX 2) (/ HEIGHT-PX 2) "center" "center" MT-SCENE)]
        [(< (snake-eaten (pit-snake w)) (snake-eaten (pit-snake2 w))) 
              (place-image/align (bitmap "graphics/game-over-green-resize.jpg") (/ WIDTH-PX 2) (/ HEIGHT-PX 2) "center" "center" MT-SCENE)]
        [(= (snake-eaten (pit-snake w)) (snake-eaten (pit-snake2 w))) 
              (place-image/align (bitmap "graphics/game-over3-resize.jpg") (/ WIDTH-PX 2)  (/ HEIGHT-PX 2) "center" "center" MT-SCENE)]))

(define (render-end2 w)
  (play-sound "graphics/game-over.wav" #t)
  (cond ;[(and (touchSnak (pit-snake w) (pit-snake2 w)) (touchSnak (pit-snake2 w) (pit-snake w))) 
              ;(place-image/align (bitmap "graphics/game-over3-resize.jpg") (/ WIDTH-PX 2)  (/ HEIGHT-PX 2) "center" "center" MT-SCENE)]
        [(or (and (touchSnak (pit-snake2 w) (pit-snake w)) (not (touchSnak (pit-snake w) (pit-snake2 w)))) (wall-colliding? (pit-snake2 w)) (self-colliding? (pit-snake2 w)) (obstacles-colliding? (pit-snake2 w) (pit-obstacles w)))
              (place-image/align (bitmap "graphics/game-over-blue2-resize.jpg") (/ WIDTH-PX 2) (/ HEIGHT-PX 2) "center" "center" MT-SCENE)]
        [(or (and (touchSnak (pit-snake w) (pit-snake2 w)) (not (touchSnak (pit-snake2 w) (pit-snake w)))) (wall-colliding? (pit-snake w)) (self-colliding? (pit-snake w)) (obstacles-colliding? (pit-snake w) (pit-obstacles w)))
              (place-image/align (bitmap "graphics/game-over-green-resize.jpg") (/ WIDTH-PX 2) (/ HEIGHT-PX 2) "center" "center" MT-SCENE)]
        [else 
              (place-image/align (bitmap "graphics/game-over3-resize.jpg") (/ WIDTH-PX 2)  (/ HEIGHT-PX 2) "center" "center" MT-SCENE)]))

(define (self-colliding? snak)
  (cons? (member (snake-head snak) (snake-body snak))))

(define (wall-colliding? snak)
  (define x (posn-x (snake-head snak)))
  (define y (posn-y (snake-head snak)))
  (or (= 0 x) (= 0 y) (= x DIMX) (= y DIMY)))

(define (obstacles-colliding? snake obs)
  (cond [(empty? obs) #f]
        [(obstacle-colliding? snake (first obs)) #t]
        [else (obstacles-colliding? snake (rest obs))]))

(define (obstacle-colliding? snake ob)
  (member-posn? (snake-head snake) ob))

(define (member-posn? sh ob)
  (cond [(empty? ob) #f]
        [(posn=? sh (first ob)) #t]
        [else (member-posn? sh (rest ob))]))

(define (pos-colliding? p ob)
  (cons? (member p ob)))

(define (snake-change-dir snak d)
  (snake d (snake-segs snak) (snake-eaten snak)))

(define (cria-lista-goos n obst)
  (if (zero? n) empty
      (cons (fresh-goo obst) (cria-lista-goos (- n 1) obst))))

(define (touchSnak snk1 snk2)
   (cons? (member (snake-head snk1) (snake-segs snk2))))

(define (coliding? snake1 snake2)
  (or (touchSnak snake1 snake2) (touchSnak snake2 snake1)))

(define (crossed-snks? snk1 snk2)
  (or (crossed-snk? snk1 snk2) (crossed-snk? snk1 snk2)))

(define (last-head s)
  (define dir (snake-dir s))
  (define head (snake-head s))
  (define headX (posn-x head))
  (define headY (posn-y head))
  (cond [(or (string=? dir "up") (string=? dir "w"))
         (posn headX (+ headY 1))]
        [(or (string=? dir "down") (string=? dir "s"))
         (posn headX (- headY 1))]
        [(or (string=? dir "left") (string=? dir "a"))
         (posn (+ headX 1) headY)]
        [(or (string=? dir "right") (string=? dir "d"))
         (posn (- headX 1) headY)]))
    
(define (crossed-snk? s1 s2)
  (define head2 (snake-head s2))
  (define lhead1 (last-head s1))
  (posn=? lhead1 head2))


(define (start-snake)
  (define snk1 (snake "right" (list (posn 1 1)) 0))
  (define snk2 (snake "w" (list (posn 15 19)) 0))
  (define obst (list-obstacles (obstacles-number) snk1 snk2))
  (big-bang (pit snk1
                 (cria-lista-goos (+ 3 (random 8)) obst)
                 snk2
                  obst)
            (name "Cobrinha")
            (on-tick next-pit TICK-RATE)
            (on-key direct-snake)
            (to-draw render-pit)
            (stop-when dead? render-end2)))

(start-snake)

