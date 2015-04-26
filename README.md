#Esc

###Authors

Christopher Ly

Jerra Khorn

###Overview

You are the a strandred boy on on an island. Avoid the cute, lovely, killer sea creatures (and yes, sharks can walk on land). Oh, and watch out for those darn pirates too. 

###Screenshot

![Alt text](https://github.com/oplS15projects/Esc/blob/master/Sceenshot_4-25-15.png)

###Concepts Demonstrated

- HOP: Almost every procedure in the repo uses other smaller procedures for different tasks.

As for existing technologies we've utilized, the only non-orginal code was from the libraries and images we used.

###Favorite Lines of Code

####Jerra's Favorite line of Code:

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

####Christopher's Favorite line of Code:

------

  
###Additional Remarks

Our motto: When in doubt, set speed to 0.

###How to Download and Run
(link release)

Open and run Esc_main.rkt. The player will spawn in the top left corner. Use the arrows to control the player.
