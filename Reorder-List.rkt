(define (get-player world-list)
  (list (car world-list)))

(define (get-enemy world-list)
  (filter (λ (x)
            (= (w2-ID x) 1))
          world-list))

(define (get-water world-list)
  (filter (λ (x)
            (eq? (w2-image x) water))
          world-list))

(define (get-tree world-list)
  (filter (λ (x)
            (eq? (w2-image x) deku-tree))
          world-list))

(define (get-sand world-list)
  (list (list-ref world-list (- (length world-list) 1))))

(define (reorder-lst world-list)
  (append (get-player world-list) 
          (get-enemy world-list)
          (get-water world-list) 
          (get-tree world-list) 
          (get-sand world-list)))