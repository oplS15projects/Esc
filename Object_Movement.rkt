;; Programmer: Jerra Khorn
;; Object_Movement.rkt

;; Updated by JK on April 24, 2014 2:17 PM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Next four defines: border checkers for each side of the scene.

(define (right-border-check world)
  (>= (posn-x (w2-coord world)) (- (x-max border) 10)))

(define (left-border-check world)
  (>= (x-min border) (posn-x (w2-coord world))))

(define (top-border-check world)
  (>= (y-min border) (posn-y (w2-coord world))))

(define (bottom-border-check world)
  (<= (- (y-max border) 10) (posn-y (w2-coord world))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; move-player: params: object
;;              return: object
;; Does border checks with the object.
;; If the object is at the border of the scene,
;; the object's posn is changed to the opposite
;; side of the scene. Direction and speed remain
;; are unchanged. The only object thar should be
;; passed to this procedure as an arugment is the
;; user controlled object.

(define (move-player world world-list)
  (cond
    ((right-border-check world)
     (make-w2 player
              (make-posn (- (x-max border) 25) (posn-y (w2-coord world)))
              (w2-speed world)
              (w2-direction world)
              (w2-ID world)))
    ((left-border-check world)
     (make-w2 player
             (make-posn (+ (x-min border) 25) (posn-y (w2-coord world)))
             (w2-speed world)
             (w2-direction world)
             (w2-ID world)))
    ((top-border-check world)
     (make-w2 player
             (make-posn (posn-x (w2-coord world)) (+ (y-min border) 25))
             (w2-speed world)
             (w2-direction world)
             (w2-ID world)))
    ((bottom-border-check world)
     (make-w2 player
              (make-posn (posn-x (w2-coord world)) (- (y-max border) 25))
              (w2-speed world)
              (w2-direction world)
              (w2-ID world)))
    (else  
     ;(tick world)
     ;(collision-non-enemy (collision-tree world world-list) world-list)
     ;(collision-non-enemy world world-list)
     (collision-tree world world-list)
     )))



(define (move-player2 world)
  (tick world))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enemy-border-check : param: object
;;                      return: object
;; If the enemy objects reaches the border,
;; the direction is changed to the opposite
;; of its current direction.

(define (enemy-border-check world)
  (cond
    ((right-border-check world)
     (make-w2 (w2-image world)
              (make-posn (- (x-max border) 11) (posn-y (w2-coord world)))
              (w2-speed world)
              'left
              (w2-ID world)))
    ((left-border-check world)
     (make-w2 (w2-image world)
              (make-posn (+ (x-min border) 11) (posn-y (w2-coord world)))
              (w2-speed world)
              'right
              (w2-ID world)))
    ((and (= (w2-ID world) 1) (top-border-check world))
     (make-w2 (w2-image world)
              (make-posn (posn-x (w2-coord world)) (+ (y-min border) 11))
              (w2-speed world)
              'down
              (w2-ID world)))
    ((bottom-border-check world)
     (make-w2 (w2-image world)
              (make-posn (posn-x (w2-coord world)) (- (y-max border) 11))
              (w2-speed world)
              'up
              (w2-ID world)))
    (else (tick world))))


;; only move enemies
(define (enemy-border-check2 world)
  (if (= (w2-ID world) 1)
      (enemy-border-check world)
      world))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; move-enemy : params: world-list
;;              return: list
;; Applies procedure enemy-border-check to
;; every enemy object in world-list.
;; Returns a new world-list containing
;; the new enemyo bjects.

(define (move-enemy world-list)
  (map enemy-border-check2 world-list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; object-movement : params: world-list
;;                   return: world-list
;; object-movement calls player movement to modify the user controled
;; object (first in world-list). It also calls move-enemy to modify the
;; rest of the list which contains the enemy objects. object-movement
;; cons together whats return from the two procedure it calls to
;; create a new world-list with the objects modofied.

(define (object-movement world-list)
  (cons (move-player (car world-list) (cdr world-list)) (move-enemy (cdr world-list))))
  ;;(collision-detection world-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(define (new-enemy-movement object)
;;  ())