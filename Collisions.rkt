;; Programmer: Christopher Ly

;; /*************** Collision Detection Code ***************************/

;; Improved Collision Detection which will find a collision for an arbitrary number of enemy objects.

(define (width object)
  (image-width object))

(define (height object)
  (image-height object))

;;(define (show arg)
  ;;arg)

(define (collision-helper enemy-list player-list)
  (cond ((null? enemy-list) #f)
        ((and (= (w2-ID (car enemy-list)) 1) (and (< (- (posn-x (w2-coord (car enemy-list))) (/ (width (enemy 'black)) 2)) (+ (- (posn-x (w2-coord player-list)) (/ (width player) 2)) (width player)))
                                                  (> (+ (- (posn-x (w2-coord (car enemy-list))) (/ (width (enemy 'black)) 2)) (width (enemy 'black))) (- (posn-x (w2-coord player-list)) (/ (width player) 2)))
                                                  (< (- (posn-y (w2-coord (car enemy-list))) (/ (height (enemy 'black)) 2)) (+ (- (posn-y (w2-coord player-list)) (/ (height player) 2)) (height player)))
                                                  (> (+ (- (posn-y (w2-coord (car enemy-list))) (/ (height (enemy 'black)) 2)) (height (enemy 'black))) (- (posn-y (w2-coord player-list)) (/ (height player) 2)))))
         #t)
        (else (collision-helper (cdr enemy-list) player-list))))

(define (collision-detection world-list)
  (if (collision-helper (cdr world-list) (car world-list))
      (begin (display "Game Over")
             #t)
      #f))

(define (collision-non-enemy world world-list)
  (cond ((null? world-list) (tick world))
        ((and (= (w2-ID (car world-list)) 0) 
              (and (< (- (posn-x (w2-coord (car world-list))) (/ (width (w2-image (car world-list))) 2)) (+ (- (posn-x (w2-coord world)) (/ (width player) 2)) (width player)))
                   (> (+ (- (posn-x (w2-coord (car world-list))) (/ (width (w2-image (car world-list))) 2)) (width (w2-image (car world-list)))) (- (posn-x (w2-coord world)) (/ (width player) 2)))
                   (< (- (posn-y (w2-coord (car world-list))) (/ (height (w2-image (car world-list))) 2)) (+ (- (posn-y (w2-coord world)) (/ (height player) 2)) (height player)))
                   (> (+ (- (posn-y (w2-coord (car world-list))) (/ (height (w2-image (car world-list))) 2)) (height (w2-image (car world-list)))) (- (posn-y (w2-coord world)) (/ (height player) 2)))))
         (cond ((eq? (w2-direction world) 'right) 
                (make-w2 player
                         (make-posn (- (- (posn-x (w2-coord (car world-list))) (/ (width (w2-image (car world-list))) 2)) 25) 
                                    (posn-y (w2-coord world)))
                         (w2-speed world)
                         (w2-direction world)
                         (w2-ID world)))
               ((eq? (w2-direction world) 'left)
                (make-w2 player
                         (make-posn (+ (+ (posn-x (w2-coord (car world-list))) (/ (width (w2-image (car world-list))) 2)) 25) 
                                    (posn-y (w2-coord world)))
                         (w2-speed world)
                         (w2-direction world)
                         (w2-ID world)))
               ((eq? (w2-direction world) 'down)
                (make-w2 player
                         (make-posn (posn-x (w2-coord world)) 
                                    (- (- (posn-y (w2-coord (car world-list))) (/ (height (w2-image (car world-list))) 2)) 25))
                         (w2-speed world)
                         (w2-direction world)
                         (w2-ID world)))
               ((eq? (w2-direction world) 'up)
                (make-w2 player
                         (make-posn (posn-x (w2-coord world)) 
                                    (+ (+ (posn-y (w2-coord (car world-list))) (/ (height (w2-image (car world-list))) 2)) 25))
                         (w2-speed world)
                         (w2-direction world)
                         (w2-ID world)))
               (else (tick world))))
         (else (collision-non-enemy world (cdr world-list)))))
      

;; /****************** End of Collision Detection **************************/