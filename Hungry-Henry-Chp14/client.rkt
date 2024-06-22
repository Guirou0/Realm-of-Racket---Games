#lang racket

(require 2htdp/universe 2htdp/image "shared.rkt")

(provide lets-eat)

(struct app (id img countdown))
(struct entree (id players food))


(define LOADING-BAR-WIDTH (/ WIDTH 4))
(define LOADING-BAR-HEIGHT (/ HEIGHT 10))
(define LOADING-OPEN-TEXT "Esperando pelo Server \n")
(define TEXT-SIZE 30)
(define TEXT-COLOR "black")
(define BASE (empty-scene WIDTH HEIGHT))
(define MY-COLOR "red")
(define PLAYER-COLOR "blue")
(define WAYPOINT-COLOR "green")

(define PLAYER-IMG (bitmap "graphics/hungry-henry.gif"))
(define FOOD-IMG (bitmap "graphics/cupcake.gif"))

(define WAYPOINT-NODE (circle 5 "solid" WAYPOINT-COLOR))

(define UPDATE-LENGTH 3)
(define END-LENGTH 2)
(define SCORE-LIST-LENGTH 2)
  
(define SEPERATOR ": ")
(define ZERO% 0)

(define INITIAL (app #f (text LOADING-OPEN-TEXT TEXT-SIZE TEXT-COLOR) ZERO%))

(define (lets-eat label server)
  (big-bang INITIAL
    (to-draw render-the-meal)
    (on-mouse set-waypoint)
    (on-receive handle-server-messages)
    (register server)
    (name label)))

(define (render-the-meal meal)
  (cond [(app? meal)    (render-appetizer meal)]
        [(entree? meal) (render-entree meal)]))

(define (handle-server-messages meal msg)
  (cond [(app? meal)    (handle-appetizer-message meal msg)]
        [(entree? meal) (handle-entree-message meal msg)]))

(define (set-waypoint meal x y me)
  (if (and (entree? meal) (mouse=? me "button-down"))
      (make-package meal (list GOTO x y))
      meal))

(define (render-appetizer app)
  (add-progress-bar (render-id+image app) (app-countdown app)))

(define (render-id+image app)
  (define id (app-id app))
  (define base-image (app-img app))
  (overlay
   (cond
     [(boolean? id) base-image]
     [else (define s (string-append "" id))
           (above base-image (text s TEXT-SIZE TEXT-COLOR))])
   BASE))

(define (add-progress-bar image countdown)
  (define w (* countdown LOADING-BAR-WIDTH))
  (define f (rectangle w LOADING-BAR-HEIGHT "solid" "yellow"))
  (define b (rectangle LOADING-BAR-WIDTH LOADING-BAR-HEIGHT "outline" "black"))
  (define bar (above (overlay/align "left" "top" f b) (text "Loading.." TEXT-SIZE TEXT-COLOR)))
  (place-image bar (* 0.5 WIDTH) (* 0.8 HEIGHT) image))

(define (handle-appetizer-message s msg)
  (cond [(id? msg)    (app msg (app-img s) (app-countdown s))]
        [(time? msg)  (app (app-id s) (app-img s) msg)]
        [(state? msg) (switch-to-entree s msg)]
        [else s]))

(define (time? msg)
  (and (real? msg) (<= 0 msg 1)))

(define (switch-to-entree s m)
  (apply entree (app-id s) (rest m)))

(define (render-entree entree)
  (define id (entree-id entree))
  (define pl (entree-players entree))
  (define fd (entree-food entree))
  (add-path id pl (add-players id pl (add-food fd BASE))))

;Teste de trapaça no jogo
(define (add-path-hack id players base-scene)
  (for/fold ([bs base-scene]) ([player players])
  (if (boolean? player)
      base-scene
      (add-waypoint* player bs))))

(define (add-food food base)
  (for/fold ([scn base]) ([fd food])
    (place-image FOOD-IMG (body-x fd) (body-y fd) scn)))

(define (add-players id lof base-scene)
  (for/fold ([scn base-scene]) ([feaster lof])
    (place-image (render-avatar id feaster)
                 (feaster-x feaster) (feaster-y feaster)
                 scn)))

(define (render-avatar id player)
  (define size (body-size (player-body player)))
  (define color
    (if (id=? id (player-id player)) MY-COLOR PLAYER-COLOR))
  (above
   (render-text (player-id player))
   (overlay (render-player-score player)
            PLAYER-IMG
            (circle size 'outline color))))

(define (render-player-score player)
  (text (number->string (get-score (body-size (player-body player)))) 14 TEXT-COLOR))

(define (add-path id players base-scene)
  (define player
    (findf (lambda (x) (id=? id (player-id x))) players))
  (if (boolean? player)
      base-scene
      (add-waypoint* player base-scene)))

(define (add-waypoint* player base-scene)
  (define loc (body-loc (player-body player)))
  (define ways (player-waypoints player))
  (define-values (resultingscene _)
    (for/fold ([scn base-scene] [from loc]) ([to ways])
      (values (add-waypoint from to scn) to)))
    resultingscene)

(define (add-waypoint from to scn)
  (define x-from (real-part from))
  (define y-from (imag-part from))
  (define x-to (real-part to))
  (define y-to (imag-part to))
  (define with-line (add-line scn x-to y-to x-from y-from WAYPOINT-COLOR))
  (place-image WAYPOINT-NODE x-to y-to with-line))

(define (body-x body)
  (real-part (body-loc body)))

(define (body-y body)
  (imag-part (body-loc body)))

(define (feaster-x feaster)
  (body-x (player-body feaster)))

(define (feaster-y feaster)
  (body-y (player-body feaster)))

(define (handle-entree-message s msg)
  (cond [(state? msg) (update-entree s msg)]
        [(score? msg) (restart s msg)]
        [else s]))

(define (update-entree s msg)
  (apply entree (entree-id s) (rest msg)))

(define (restart s end-msg)
  (define score-image (render-scores end-msg))
  (app (entree-id s) (above (text LOADING-OPEN-TEXT TEXT-SIZE TEXT-COLOR) score-image) ZERO%))

(define (render-scores msg)
  (define scores (sort (second msg) < #:key second))
  (for/fold ([img empty-image]) ([name-score scores])
    (define txt (get-text name-score))
    (above (render-text txt) img)))

(define (render-text txt)
  (text txt TEXT-SIZE TEXT-COLOR))

(define (get-text name-score)
  (define-values (name score) (apply values name-score))
  (string-append name SEPERATOR (number->string score)))

(define (state? msg)
  (and (list? msg)
       (= UPDATE-LENGTH (length msg))
       (symbol? (first msg))
       (list? (second msg))
       (list? (third msg))
       (symbol=? SERIALIZE (first msg))
       (andmap player? (second msg))
       (andmap body? (third msg))))

(define (score? msg)
  (and (list? msg)
       (= END-LENGTH (length msg))
       (symbol? (first msg))
       (list? (second msg))
       (symbol=? SCORE (first msg))
       (score-list? (second msg))))

(define (score-list? l)
  (for/and ([s l])
    (and (list? s)
         (= SCORE-LIST-LENGTH (length s))
         (id? (first s))
         (number? (second s)))))