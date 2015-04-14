#lang racket

;;;;;; Version 3 ;;;;;;

(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)
(require racket/include)

;; include Chris' Collision Code.
(include (file "Esc_Collisions.rkt"))

;; User controls a circle
(define player (circle 10 'solid 'red))

;; enemy :  params: symbol
;;          return: image
;; enemy take a color which is a symbol
;; and returns a trianle of that color.
(define (enemy color)
  (star 25 'solid color))

(define HEIGHT 300)
(define WIDTH 300)
(define center-y (/ HEIGHT 2))
(define center-x (/ WIDTH 2))
(define canvas (empty-scene HEIGHT WIDTH))
(define speed 5)



;; Manipulating lists functions

(define (map proc seq)
  (if (null? seq)
      '()
      (cons (proc (car seq)) (map proc (cdr seq)))))



;; Object contains image, position, speed, and direction
;; - image is an image
;; - position is a posn
;; - speed is a number
;; - direction is a direction

(define-struct w2 (image coord speed direction))



;; Initial World state is a list of five objects
;; First object in the list is a ball that is controlled by the user.
;; Next four objects are stars placed in the four corners of the scene.

(define initial-world2
  (list (make-w2 player (make-posn center-x center-y) speed 'none)
        (make-w2 (enemy 'blue)   (make-posn 20 20) 10 'right)
        (make-w2 (enemy 'green)  (make-posn (- WIDTH 20) 20) 10 'down)
        (make-w2 (enemy 'yellow) (make-posn 20 (- HEIGHT 20)) 10 'up)
        (make-w2 (enemy 'orange) (make-posn (- WIDTH 20) (- HEIGHT 20)) 10 'left)))



;; change-pos : params: Posn Speed Direction
;;              return: Posn 
;; Given a posn, speed and direction return the new position (as a Posn) 
;; moved due to speed and direction 

(define (change-pos coord speed direction)
  (cond 
    ((eq? direction 'right) (make-posn (+ (posn-x coord) speed) (posn-y coord)))
    ((eq? direction 'left)  (make-posn (- (posn-x coord) speed) (posn-y coord)))
    ((eq? direction 'up)    (make-posn (posn-x coord) (- (posn-y coord) speed)))
    ((eq? direction 'down)  (make-posn (posn-x coord) (+ (posn-y coord) speed)))
    ((eq? direction 'none)  (make-posn (posn-x coord) (posn-y coord)))))



;; Next four defines: border checkers for each side of the scene.

(define (right-border-check world)
  (<= WIDTH (posn-x (w2-coord world))))

(define (left-border-check world)
  (>= 0 (posn-x (w2-coord world))))

(define (top-border-check world)
  (>= 0 (posn-y (w2-coord world))))

(define (bottom-border-check world)
  (<= HEIGHT (posn-y (w2-coord world))))


;; tick : params: object
;;        return: object
;; tick creates a new object with its posn changed
;; according to its current posn, speed, and direction.

(define (tick world) 
  (make-w2 (w2-image world)
          (change-pos (w2-coord world) (w2-speed world) (w2-direction world))
          (w2-speed world)
          (w2-direction world)))


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
         (make-w2 (w2-image world) (w2-coord world) (- (w2-speed world) 1) (w2-direction world)))
        (else (tick world))))



;; move-player: params: object
;;              return: object
;; Does border checks with the object.
;; If the object is at the border of the scene,
;; the object's posn is changed to the opposite
;; side of the scene. Direction and speed remain
;; are unchanged. The only object thar should be
;; passed to this procedure as an arugment is the
;; user controlled object.

(define (move-player world)
  (cond
    ((right-border-check world)
     (make-w2 player
             (make-posn 1 (posn-y (w2-coord world)))
             (w2-speed world)
             (w2-direction world)))
    ((left-border-check world)
     (make-w2 player
             (make-posn (- WIDTH 1) (posn-y (w2-coord world)))
             (w2-speed world)
             (w2-direction world)))
    ((top-border-check world)
     (make-w2 player
             (make-posn (posn-x (w2-coord world)) (- HEIGHT 1))
             (w2-speed world)
             (w2-direction world)))
    ((bottom-border-check world)
     (make-w2 player
             (make-posn (posn-x (w2-coord world)) 1)
             (w2-speed world)
             (w2-direction world)))
    (else (tick world))))



;; enemy-border-check : param: object
;;                      return: object
;; If the enemy objects reaches the border,
;; the direction is changed to the opposite
;; of its current direction.

(define (enemy-border-check world)
  (cond
    ((right-border-check world)
     (make-w2 (w2-image world)
              (make-posn (- WIDTH 1) (posn-y (w2-coord world)))
              (w2-speed world)
              'left))
    ((left-border-check world)
     (make-w2 (w2-image world)
              (make-posn 1 (posn-y (w2-coord world)))
              (w2-speed world)
              'right))
    ((top-border-check world)
     (make-w2 (w2-image world)
              (make-posn (posn-x (w2-coord world)) 1)
              (w2-speed world)
              'down))
    ((bottom-border-check world)
     (make-w2 (w2-image world)
              (make-posn (posn-x (w2-coord world)) (- HEIGHT 1))
              (w2-speed world)
              'up))
    (else (tick world))))


;; move-enemy : params: world-list
;;              return: list
;; Applies procedure enemy-border-check to
;; every enemy object in world-list.
;; Returns a new world-list containing
;; the new enemyo bjects.

(define (move-enemy world-list)
  (map enemy-border-check world-list))



;;;;;;;;;;;;;;; Keeping track of time ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tick- is a way to keep track of the time. The on-tick handler ticks 28 
;; times per second. On every tick, the count increments by one and tick4 
;; is called. Once it reaches 28, count is set back to 0, seconds is 
;; incremented by 1, and tick4 is called.

(define tick-
  (let ((clock-tick 0)
        (seconds 0))
    (lambda (world-list)
      (if (= clock-tick 28)
          (begin (set! clock-tick 0)
                 (set! seconds (+ seconds 1))
                 (display "Time : ")
                 (display seconds)
                 (newline)
                 (tick4 world-list))
          (begin (set! clock-tick (+ clock-tick 1))
                 (tick4 world-list))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; release: params: World KeyEvent
;;          return: World
;; If the key that is being released is left, right, up, or down,
;; then a new world is returned with the same properties at the
;; first argument except with its direction changed to none. Else,
;; the first argument is returned.

(define (release world-list a-key)
  (if (cond ((key=? a-key "left"))
          (key=? a-key "right")
          (key=? a-key "up")
          (key=? a-key "down"))
      (cons (make-w2 player (w2-coord (car world-list)) (w2-speed (car world-list)) 'none)
             (cdr world-list))
      world-list))



;; (place-images images posns scene) → image?
;;  images : (listof image?)
;;  posns : (listof posn?)
;;  scene : image?
;;  Places each of images into scene like place-image would,
;;  using the coordinates in posns as the x and y arguments to place-image.

(define (draw-scene2 world-list)
  (place-images (map w2-image world-list)
                (map w2-coord world-list)
                canvas))


(define (change-direction world-list direction)
  (cons (make-w2 (w2-image (car world-list)) 
                 (w2-coord (car world-list))
                 (w2-speed (car world-list))
                 direction)
        (cdr world-list)))



;; control :    params : World, KeyEvent
;;             return : World
;; Key events are represented with strings.
;; "left" is the left arrow.
;; "right" is the right arrow.
;; "up" is the up arrow.
;; "down" is the down arrow.
;; "c" is the c key.
;; If the key=? returns true when comparing the key event
;; with a specfic string, a new world is return with
;; some of its properties modified depending on the mathching
;; string.

(define (control world-list a-key)
  (cond
    ((key=? a-key "left")  (change-direction world-list 'left))
    ((key=? a-key "right") (change-direction world-list 'right))
    ((key=? a-key "up")    (change-direction world-list 'up))
    ((key=? a-key "down")  (change-direction world-list 'down))
    ;; ((key=? a-key "c")     (make-w2 (make-posn center-x center-y) speed 'none))
    (else world-list)))



(big-bang initial-world2
          (on-tick tick-)
          (on-draw draw-scene2)
          (on-key control)
          (on-release release)
          (name "Lone Wolf"))

