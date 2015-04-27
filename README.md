#Esc

###Authors

Christopher Ly

Jerra Khorn

###Overview

You are stranded on an island and must not come in contact with the island's cute/lethal inhabitants. By the way, it is also high tide season and you have no idea how to swim. Can you make it out alive?

###Screenshot

![Alt text](https://github.com/oplS15projects/Esc/blob/master/Sceenshot_4-25-15.png)

###Concepts Demonstrated

- Higher Order Procedures (HOP): Almost every procedure in the repo uses other smaller procedures to perform different tasks. Also makes the code more organized and readable.

- Abstraction: Follows along the same lines as HOP. The concept applied here is to immplement procedures for different kinds of modified arguements as parameters. For example, when creating a game object we gave it an ID field which is an integer that distinguishes what type of game object it is and based on the ID certain procedures perform different task.

- Collision Detection: When facing with the issue of collision detection we used the Separating Axis Theorem as our method of determining if collisions occurred. 

###External Technology and Libraries
[collision]: https://developer.mozilla.org/en-US/docs/Games/Techniques/2D_collision_detection
[universe]: http://docs.racket-lang.org/teachpack/2htdpuniverse.html
[image]: http://docs.racket-lang.org/teachpack/2htdpimage.html

- **[Collision Detection][collision]**: Used the method of the Separating Axis Theorem 
- **[Universe.rkt][universe]**: This library provided the way to use the `big-bang` function, which enables us to run multiple functions on the canvas at the appropriate time.
- **[Images.rkt][image]**: This library allowed us to place graphics onto the canvas and manipulate them.

###Favorite Lines of Code

####Jerra's Favorite Lines of Code:

This release procedure is called everytime you let go of a key. If that key is one of the arrow keys,
the player stops moving. The reason why it was my favorite line of code is that when
we were writing of collision detection code, we couldn't figure out why it was not working the way it
should. I had the idea of changing the speed of player to 0 in the release procedure,
which greatly improved the collision detection. It was just a funny moment figuring out that our problem 
was solve by just one number: 0.

```
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
```

####Christopher's Favorite Lines of Code:

We wanted to implement border of waves every 15 seconds onto the map. These waves act as a border to make the map smaller. This simple concept was surprisingly difficult to implement. We spent a lot of time trying to come up with a method to add the correct number of water tiles to the map. This became my favorite lines of code because of the degree of difficulty it provided and the fact that we figured it out. We did it by creating a lists of positions to add the watter tiles. First we made a list of positions for the the top border, the right, bottom and left; and finally appended all four lists together to create one.  

```
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
```
------

  
###Additional Remarks

Our motto: When in doubt, set speed to 0.

###How to Download and Run
- Open and run Esc_main.rkt. file.
- The player will spawn in the top left corner. Use the arrow keys to control the player.
- Avoid collisions moving enemies.
- Water boundaries will spawn along the border every 15 seconds to make the map smaller.
- Enemies will change direction at random times.

[demo]:https://github.com/oplS15projects/Esc/releases/tag/v3
- **[Demo-Release][demo]**
