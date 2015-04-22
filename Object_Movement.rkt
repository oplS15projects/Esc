;; Programmer: Jerra Khorn
;; Object_Movement.rkt


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (change-direction world-list direction)
  (cons (make-w2 (w2-image (car world-list)) 
                 (w2-coord (car world-list))
                 (w2-speed (car world-list))
                 direction
                 (w2-ID (car world-list)))
        (cdr world-list)))


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
  (<= WIDTH (posn-x (w2-coord world))))

(define (left-border-check world)
  (>= 0 (posn-x (w2-coord world))))

(define (top-border-check world)
  (>= 0 (posn-y (w2-coord world))))

(define (bottom-border-check world)
  (<= HEIGHT (posn-y (w2-coord world))))


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

(define (move-player world)
  (cond
    ((right-border-check world)
     (make-w2 player
             (make-posn 1 (posn-y (w2-coord world)))
             (w2-speed world)
             (w2-direction world)
             (w2-ID world)))
    ((left-border-check world)
     (make-w2 player
             (make-posn (- WIDTH 1) (posn-y (w2-coord world)))
             (w2-speed world)
             (w2-direction world)
             (w2-ID world)))
    ((top-border-check world)
     (make-w2 player
             (make-posn (posn-x (w2-coord world)) (- HEIGHT 1))
             (w2-speed world)
             (w2-direction world)
             (w2-ID world)))
    ((bottom-border-check world)
     (make-w2 player
             (make-posn (posn-x (w2-coord world)) 1)
             (w2-speed world)
             (w2-direction world)
             (w2-ID world)))
    (else (tick world))))


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
              (make-posn (- WIDTH 1) (posn-y (w2-coord world)))
              (w2-speed world)
              'left
              (w2-ID world)))
    ((left-border-check world)
     (make-w2 (w2-image world)
              (make-posn 1 (posn-y (w2-coord world)))
              (w2-speed world)
              'right
              (w2-ID world)))
    ((top-border-check world)
     (make-w2 (w2-image world)
              (make-posn (posn-x (w2-coord world)) 1)
              (w2-speed world)
              'down
              (w2-ID world)))
    ((bottom-border-check world)
     (make-w2 (w2-image world)
              (make-posn (posn-x (w2-coord world)) (- HEIGHT 1))
              (w2-speed world)
              'up
              (w2-ID world)))
    (else (tick world))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; move-enemy : params: world-list
;;              return: list
;; Applies procedure enemy-border-check to
;; every enemy object in world-list.
;; Returns a new world-list containing
;; the new enemyo bjects.

(define (move-enemy world-list)
  (map enemy-border-check world-list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; object-movement : params: world-list
;;                   return: world-list
;; object-movement calls player movement to modify the user controled
;; object (first in world-list). It also calls move-enemy to modify the
;; rest of the list which contains the enemy objects. object-movement
;; cons together whats return from the two procedure it calls to
;; create a new world-list with the objects modofied.

(define (object-movement world-list)
  (cons (move-player (car world-list)) (move-enemy (cdr world-list))))
  ;;(collision-detection world-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(define (new-enemy-movement object)
;;  ())
