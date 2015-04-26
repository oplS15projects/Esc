;; Sand make-posn procedure: 
;; args: x and y coordinates of top left corner
(define (water-make-posn x-coord y-coord)
  (make-posn (- (+ x-coord (width water)) (/ (width water) 2)) (- (+ y-coord (height water)) (/ (height water) 2))))

(define (center-list pos vert hori)
  (define (get-top start hori)
    (if (= 0 hori)
        '()
        (cons start (get-top (make-posn (+ (posn-x start) 50) (posn-y start))
                             (- hori 1)))))
  (define (get-right lst vert)
    (define (get-right-helper start vert)
      (if (= 0 vert)
          '()
          (cons start (get-right-helper (make-posn (posn-x start) (+ (posn-y start) 25))
                                        (- vert 1)))))
    (append lst (get-right-helper (list-ref lst (- (length lst) 1)) vert)))
  (define (get-bottom lst hori)
    (define (get-bottom-helper start hori)
      (if (= 0 hori)
          '()
          (cons start (get-bottom-helper (make-posn (- (posn-x start) 50) (posn-y start))
                                         (- hori 1)))))
    (append lst (get-bottom-helper (list-ref lst (- (length lst) 1)) hori)))
  (define (get-left lst vert)
    (define (get-left-helper start vert)
      (if (= 0 vert)
          '()
          (cons start (get-left-helper (make-posn (posn-x start) (- (posn-y start) 25))
                                       (- vert 1)))))
    (append lst (get-left-helper (list-ref lst (- (length lst) 1)) vert)))
  (get-left (get-bottom (get-right (get-top pos hori) vert) hori) vert))
  

(define (find-posn start vert hori)
  (center-list (water-make-posn (car start) (cdr start)) vert hori))

(define (add-water2 posn-list world-list)
  (if (null? posn-list)
      world-list
      (add-water2 (cdr posn-list) (cons (car world-list)
                                        (cons (make-w2 water
                                                       (car posn-list)
                                                       0
                                                       'none
                                                       0)
                                              (cdr world-list))))))
  
(define add-water
  (let ((start (cons 0 0))
        (next-start (cons 0 0))
        (vert 26)
        (hori 13))
    (lambda (world-list)
      (begin (set! start next-start)
             (set! next-start (cons (+ (car start) 50) (+ (cdr start) 25)))
             (set! vert (- vert 2))
             (set! hori (- hori 2))
             (add-water2 (find-posn start (+ vert 2) (+ hori 2)) world-list)))))