;; Author: Chris Ly

;; /*************** Collision Detection Code ***************************/
(define (width object)
  (image-width object))

(define (height object)
  (image-height object))

(define (collision-helper enemy-list player-list)
  (and (< (- (posn-x (w2-coord enemy-list)) (/ (width (enemy 'black)) 2)) (+ (- (posn-x (w2-coord player-list)) (/ (width player) 2)) (width player)))
       (> (+ (- (posn-x (w2-coord enemy-list)) (/ (width (enemy 'black)) 2)) (width (enemy 'black))) (- (posn-x (w2-coord player-list)) (/ (width player) 2)))
       (< (- (posn-y (w2-coord enemy-list)) (/ (height (enemy 'black)) 2)) (+ (- (posn-y (w2-coord player-list)) (/ (height player) 2)) (height player)))
       (> (+ (- (posn-y (w2-coord enemy-list)) (/ (height (enemy 'black)) 2)) (height (enemy 'black))) (- (posn-y (w2-coord player-list)) (/ (height player) 2)))))

(define (collision-detection world-list)
  (if (or (collision-helper (cadr world-list) (car world-list))
           (collision-helper (caddr world-list) (car world-list))
           (collision-helper (cadddr world-list) (car world-list))
           (collision-helper (car (cdr (cdr (cdr (cdr world-list))))) (car world-list)))
      (error 'collision)
      (cons (move-player (car world-list)) (move-enemy (cdr world-list)))))
      

(define (tick4 world-list)
  ;;(cons (move-player (car world-list)) (move-enemy (cdr world-list))))
  (collision-detection world-list))

;; /****************** End of Collision Detection **************************/
