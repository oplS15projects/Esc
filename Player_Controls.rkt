;; Programmer: Jerra Khorn
;; Player_Controls.rkt

;; Updated by JK on April 24, 2014 2:17 PM


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; change-direction: params: world-list, direction
;;                   return: world-list
;; Creates new object with all its fields the same as
;; the first object in the list, except for direction.
;; The direction of the new object is the argument
;; direction. Then return a world-list with the new object
;; as the first list object and the rest of the old
;; world-list.

(define (change-direction world-list direction)
  (cons (make-w2 (w2-image (car world-list)) 
                 (w2-coord (car world-list))
                 (w2-speed (car world-list))
                 direction
                 (w2-ID (car world-list)))
        (cdr world-list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; change-speed: params: world-list, speed
;;               return: world-list
;; Creates new object with all its fields the same as
;; the first object in the list, except for speed.
;; The speed of the new object is the argument
;; speed. Then return a world-list with the new object
;; as the first list object and the rest of the old
;; world-list.

(define (change-speed world-list new-speed)
  (cons (make-w2 (w2-image     (car world-list)) 
                 (w2-coord     (car world-list))
                 new-speed
                 (w2-direction (car world-list))
                 (w2-ID        (car world-list)))
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
    ((key=? a-key "left")  (change-speed (change-direction world-list 'left) 10))
    ((key=? a-key "right") (change-speed (change-direction world-list 'right) 10))
    ((key=? a-key "up")    (change-speed (change-direction world-list 'up) 10))
    ((key=? a-key "down")  (change-speed (change-direction world-list 'down) 10))
    ;; ((key=? a-key "c")     (make-w2 (make-posn center-x center-y) speed 'none))
    ((key=? a-key " ")     (cons (make-w2 player
                                          (make-posn (if (even? (random 10))
                                                         (random-integer 0 x-mid-1)
                                                         (random-integer x-mid-2 WIDTH))
                                                     (if (even? (random 10))
                                                         (random-integer 0 y-mid-1)
                                                         (random-integer y-mid-2 HEIGHT)))
                                          10
                                          'none
                                          2)
                                 (cdr world-list)))
    (else world-list)))

(define (control-2 world-list a-key)
  (if (eq? (w2-image (car world-list)) game-over2)
      (if (key=? a-key "\r")
          (begin (set! is-game-over? #f)
                initial-world)
          world-list)
      (cond
        ((key=? a-key "left")  (change-speed (change-direction world-list 'left) 10))
        ((key=? a-key "right") (change-speed (change-direction world-list 'right) 10))
        ((key=? a-key "up")    (change-speed (change-direction world-list 'up) 10))
        ((key=? a-key "down")  (change-speed (change-direction world-list 'down) 10))
        ;; ((key=? a-key "c")     (make-w2 (make-posn center-x center-y) speed 'none))
        ((key=? a-key " ")     (cons (make-w2 player
                                              (make-posn (if (even? (random 10))
                                                             (random-integer 0 x-mid-1)
                                                             (random-integer x-mid-2 WIDTH))
                                                         (if (even? (random 10))
                                                             (random-integer 0 y-mid-1)
                                                             (random-integer y-mid-2 HEIGHT)))
                                              10
                                              'none
                                              2)
                                     (cdr world-list)))
        (else world-list))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; release: params: World KeyEvent
;;          return: World
;; If the key that is being released is left, right, up, or down,
;; then a new world is returned with the same properties at the
;; first argument except with its direction changed to none. Else,
;; the first argument is returned.
      
(define (release world-list a-key)
  (if (or (key=? a-key "left")
          (key=? a-key "right")
          (key=? a-key "up")
          (key=? a-key "down"))
      (cons (make-w2 (w2-image (car world-list))
                     (w2-coord (car world-list))
                     0
                     (w2-direction (car world-list))
                     (w2-ID (car world-list)))
            (cdr world-list))
      world-list))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;