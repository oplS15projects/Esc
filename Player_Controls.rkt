;; Programmer: Jerra Khorn
;; Player_Controls.rkt


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
      (cons (make-w2 player (w2-coord (car world-list)) (w2-speed (car world-list)) 'none (w2-ID (car world-list)))
             (cdr world-list))
      world-list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
