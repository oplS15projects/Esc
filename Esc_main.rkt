#lang racket

;; Programmers: Christopher Ly, Jerra Khorn
;; Ecs.rkt
;; Created by JK on April 21, 2014 12:17 PM
;; Updated by JK on April 21, 2014 8:06 PM
;; Updated by CL on April 21, 2014 10:09 PM
;; Updated by JK on April 24, 2014 2:17 PM

(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/planetcute)
(require lang/posn)
(require racket/include)
(require math/base)

;; include Chris' Collision Code.
(include (file "Collisions.rkt"))
(include (file "Player_Controls.rkt"))
(include (file "Object_Movement.rkt"))
(include (file "Add_Water.rkt"))
(include (file "Reorder-List.rkt"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Manipulating lists functions

(define (map proc seq)
  (if (null? seq)
      '()
      (cons (proc (car seq)) (map proc (cdr seq)))))

(define (filter proc seq)
  (cond ((null? seq) '())
        ((proc (car seq))
         (cons (car seq) (filter proc (cdr seq))))
        (else (filter proc (cdr seq)))))

(define (list-ref seq num)
  (if (= num 0)
      (car seq)
      (list-ref (cdr seq) (- num 1))))

(define random-item
  (let ((rand 0))
    (λ (seq)
      (begin (set! rand (random (length seq)))
             (list-ref seq rand)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; inital speed of user controlled object.
(define speed 10)

;; list of possible colors for the objects.
(define colors '(red orange yellow green blue black grey))

;; list of possible directions for the objects.
(define directions '(up down left right))

;; /************************ Initialize Game Objects **************************/

;; User controls a circle
(define player 
  ;(circle '10 'solid 'red)
  (bitmap "tiles/boy.png"))

(define (enemy color)
  (star '10 'solid color))

;; bitmap of different enemies
(define crab     (bitmap "tiles/crab-icon.png"))
(define lobster  (bitmap "tiles/lobster-icon.png"))
(define octopus  (bitmap "tiles/octopus-icon.png"))
(define shark    (bitmap "tiles/shark-icon.png"))
(define turtle   (bitmap "tiles/turtle-icon.png"))
(define pirate1  (bitmap "tiles/pirate-icon-1.png"))
(define pirate2  (bitmap "tiles/pirate-icon-2.png"))

;; list of enemies
(define enemy-lst (list crab lobster octopus shark turtle pirate1 pirate2))


;; /************************ Game Objects (Non-Enemies) **********************/

(define deku-tree (bitmap "tiles/deku-tree2.png"))
(define sand (bitmap "tiles/Sandv2.png"))
(define water (bitmap "tiles/Waterv2.png"))
(define game-over (bitmap "tiles/Game_Over.png"))

;; /************************ End of Game Object Initialization ****************/

;; size of cavnvas is WIDTH * HEIGHT.
(define HEIGHT 650)
(define WIDTH 650)
(define canvas (empty-scene WIDTH HEIGHT))

;; center x&y coordinates of the canvas.
(define center-y (/ HEIGHT 2))
(define center-x (/ WIDTH 2))


  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Object contains image, position, speed, and direction
;; - image is an image
;; - position is a posn
;; - speed is a number
;; - direction is a direction
;; - ID is an integer 0, 1, or 2 that distinguishes the game object as either a player, enemy, or normal game object.

(define-struct w2 (image coord speed direction ID))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Initial World state is a list of five objects
;; First object in the list is a ball that is controlled by the user.
;; Next four objects are stars placed in the four corners of the scene.

(define initial-world
  (list (make-w2 player (make-posn 25 25) speed 'none 2)
        (make-w2 deku-tree (make-posn center-x center-y) speed 'none 0)
        ;;(make-w2 water (water-make-posn 50 0) speed 'none 0)
        ;;(make-w2 water (water-make-posn 0 25) speed 'none 0)
        (make-w2 sand (make-posn 350 345) speed 'none 2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tick : params: object
;;        return: object
;; tick creates a new object with its posn changed
;; according to its current posn, speed, and direction.

(define (tick world) 
  (make-w2 (w2-image world)
           (change-pos (w2-coord world) (w2-speed world) (w2-direction world))
           (w2-speed world)
           (w2-direction world)
           (w2-ID world)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; draw-scene: params: world-list
;; Places the objects in world-list onto a canvas.

(define (draw-scene world-list)
  (place-images (map w2-image world-list)
                (map w2-coord world-list)
                canvas))


;;;;;;;;;;;;;;;;;;;;;;;;; MILESTONE 2 : Adding New Objects ;;;;;;;;;;;;;;;;;;;;;;;;;

;; add-new-enemy : params: world-list
;;                 return: world-list
;; The inserts a new enemy object in the world-list
;; in the position after the player object.
;; The new enemy object will have a random color,
;; a random posn in repect to the height of the canvas,
;; a random speed between 5 - 20, and a random direction.

;; height and width of deku-tree
(define tree-width  (image-width deku-tree))
(define tree-height (image-height deku-tree))

;; x & y coordinates of deku-tree
(define x-mid-1 (floor (- center-x (/ tree-width 2))))
(define x-mid-2 (floor (+ center-x (/ tree-width 2))))
(define y-mid-1 (floor (- center-y (/ tree-height 2))))
(define y-mid-2 (floor (+ center-y (/ tree-height 2))))

(define (add-new-enemy world-list)
  (cons (car world-list)
        (cons (make-w2 ;(enemy (random-item colors))
                       (random-item enemy-lst)
                       (make-posn (if (even? (random 10))
                                      (random-integer 0 x-mid-1)
                                      (random-integer x-mid-2 WIDTH))
                                  (if (even? (random 10))
                                      (random-integer 0 y-mid-1)
                                      (random-integer y-mid-2 HEIGHT)))
                       (random-integer 5 20) 
                       (random-item directions)
                       1)
              (cdr world-list))))


;;;;;;;;;;;;;;;;;;;;;;;; MILESTONE 2 : Keeping track of time ;;;;;;;;;;;;;;;;;;;;;;;;

;; timer is a way to keep track of the time. The on-tick handler ticks 28 
;; times per second. On every tick, the count increments by one and object-movement
;; is called. Once it reaches 28, count is set back to 0, seconds is 
;; incremented by 1, a new enemy is added.

;; check if a is divisible by b
(define (divisible? a b)
  (= (remainder a b) 0))

;; displays time
(define (display-time seconds)
  (begin (display "Time : ")
         (display seconds)
         (newline)))

;; display when enemies will change direction
(define (display-rand rand)
  (begin (display "Direction Shift at : ")
         (display rand)
         (display " ")
         (newline)))

;; old timer procedure
(define timer
  (let ((clock-tick 0)
        (seconds 0))
    (lambda (world-list)
      (if (= clock-tick 28)
          (begin (set! clock-tick 0)
                 (set! seconds (+ seconds 1))
                 (display-time seconds)
                 (if (and (not (= seconds 0)) (or (divisible? seconds 2) (divisible? seconds 5)))
                     (if (divisible? seconds 15)
                         (reorder-lst (add-water (object-movement (add-new-enemy world-list))))
                         (object-movement (add-new-enemy world-list)))
                     (object-movement world-list)))
          (begin (set! clock-tick (+ clock-tick 1))
                         (object-movement world-list))))))

;; changes direction of enemies to a random direction
(define (enemy-direction-ch world-list)
  (define (helper world)
    (if (= (w2-ID world) 1)
        (make-w2 (w2-image world)
                 (w2-coord world)
                 (w2-speed world)
                 (random-item directions)
                 (w2-ID world))
        world))
  (map helper world-list))
 
;; version 2
;; implemented change in enemy direction at some random time
(define timer2
  (let ((clock-tick 0)
        (seconds 0)
        (rand (random-integer 5 20)))
    (lambda (world-list)
      (if (= clock-tick 28)
          (begin (set! clock-tick 0)
                 (set! seconds (+ seconds 1))
                 (display-time seconds)
                 (display-rand rand)
                 (if (= seconds rand)
                     (begin (set! rand (+ seconds (random-integer 1 10)))
                            (enemy-direction-ch world-list))
                     (if (and (not (= seconds 0)) 
                              (or (divisible? seconds 2) 
                                  (divisible? seconds 5)))
                         (if (divisible? seconds 15)
                             (reorder-lst (add-water (object-movement (add-new-enemy world-list))))
                             (object-movement (add-new-enemy world-list)))
                         (object-movement world-list))))
          (begin (set! clock-tick (+ clock-tick 1))
                 (object-movement world-list))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Display game over image.

(define last-image
  (λ (some-argument)
    (begin (sleep 1)
           (place-image game-over center-x center-y canvas))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Basically the game starter

(big-bang initial-world
          (on-tick timer2)
          (to-draw draw-scene)
          (on-key control)
          (on-release release)
          (stop-when collision-detection last-image)
          (name "[Esc]"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; new proc to further develop the movement of the user
;; controlled ball. once the user releases the key,
;; the ball will reduced its speed until it comes to
;; a stop. STILL IN DEVELOPMENT.

(define (new-tick world)
  (cond ((<= (w2-speed world) 0)
         (make-w2 (w2-image world) (w2-coord world) 5 'none))
        ((or (eq? (w2-direction world) 'left-slow)
             (eq? (w2-direction world) 'right-slow)
             (eq? (w2-direction world) 'up-slow)
             (eq? (w2-direction world) 'down-slow))
         (make-w2 (w2-image world) (w2-coord world) (- (w2-speed world) 1) (w2-direction world) (w2-ID world)))
        (else (tick world))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;