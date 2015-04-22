;; Programmer: Christopher Ly

;; /*************** Collision Detection Code ***************************/

;; Improved Collision Detection which will find a collision for an arbitrary number of enemy objects.

(define (width object)
  (image-width object))

(define (height object)
  (image-height object))

(define (show arg)
  arg)

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
      

;; /****************** End of Collision Detection **************************/
