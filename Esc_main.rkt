#lang racket

;; Programmers: Christopher Ly, Jerra Khorn
;; Ecs.rkt
;; Created by JK on April 21, 2014 12:17 PM
;; Updated by JK on April 21, 2014 8:06 PM

(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)
(require racket/include)
(require math/base)

;; include Chris' Collision Code.
(include (file "Collisions.rkt"))
(include (file "Player_Controls.rkt"))
(include (file "Object_Movement.rkt"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Manipulating lists functions

(define (map proc seq)
  (if (null? seq)
      '()
      (cons (proc (car seq)) (map proc (cdr seq)))))

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
(define player (circle 10 'solid 'red))

;; enemy :  params: symbol
;;          return: image
;; enemy take a color which is a symbol
;; and returns a trianle of that color.
(define (enemy color)
  (star 20 'solid color))

;; /************************ Game Objects (Non-Enemies) **********************/

(define deku-tree (bitmap "tiles/deku-tree.png"))

;; /************************ End of Game Object Initialization ****************/

;; size of cavnvas is WIDTH * HEIGHT.
(define HEIGHT 700)
(define WIDTH 700)
(define canvas (empty-scene HEIGHT WIDTH))

;; center x&y coordinates of the canvas.
(define center-y (/ HEIGHT 2))
(define center-x (/ WIDTH 2))

(define (make-rand-posn width height)
  (make-posn (random-integer 12 (- width 12)) 
             (random-integer 12 (- height 12))))


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
  (list (make-w2 player (make-posn center-x center-y) speed 'none 2)
        (make-w2 (enemy 'orange) (make-rand-posn WIDTH HEIGHT) 6 'right 1)
        (make-w2 (enemy 'yellow) (make-rand-posn WIDTH HEIGHT) 6 'down 1)
        (make-w2 (enemy 'green)  (make-rand-posn WIDTH HEIGHT) 6 'up 1)
        (make-w2 (enemy 'blue)   (make-rand-posn WIDTH HEIGHT) 6 'left 1)
        (make-w2 deku-tree (make-posn center-x center-y) speed 'none 0)))


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

(define (add-new-enemy world-list)
  (cons (car world-list)
        (cons (make-w2 (enemy (random-item colors)) 
                       (make-rand-posn WIDTH HEIGHT) 
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

(define timer
  (let ((clock-tick 0)
        (seconds 0))
    (lambda (world-list)
      (cond ((= clock-tick 28)
             (begin (set! clock-tick 0)
                    (set! seconds (+ seconds 1))
                    (display-time seconds)
                    (object-movement world-list)))
            ((and (not (= seconds 0)) (divisible? seconds 5))
             (begin (set! seconds (+ seconds 1))
                    (display-time seconds)
                    (object-movement (add-new-enemy world-list))))
            (else (begin (set! clock-tick (+ clock-tick 1))
                         (object-movement world-list)))))))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(big-bang initial-world
          (on-tick timer)
          (to-draw draw-scene)
          (on-key control)
          (on-release release)
          (stop-when collision-detection)
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
