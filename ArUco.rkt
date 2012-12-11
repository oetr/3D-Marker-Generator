(module ArUco racket
  (require "matrices.rkt"
           racket/draw)
  
  (provide (all-defined-out))
  
  (define (generate-marker id)
    (define ids (vector #x10 #x17 #x09 #x0e))
    (for/vector ([y (in-range 0 5)])
      (define index (bitwise-and (arithmetic-shift id (- (* 2 (- 4 y))))
                                 3))
      (define val (vector-ref ids index))
      (for/vector ([x (in-range 0 5)])
        (if (> (bitwise-and (arithmetic-shift val (- (- 4 x))) 1) 0)
            1
            0))))


  (define (matrix-rotate a-matrix (direction 'ccw))
    ;; do only ccw rotation for now!
    (define rows (vector-length a-matrix))
    (define cols (vector-length (vector-ref a-matrix 0)))
    (define result (make-matrix rows cols))
    (define (find-new-row row col) col)
    (define (find-new-col row col) (+ (* row -1) (- cols 1)))
    (for* ([col (in-range 0 cols)]
           [row (in-range 0 rows)])
      (matrix-set! result
                   (find-new-row row col)
                   (find-new-col row col)
                   (matrix-ref a-matrix row col)))
    result)

  (define (draw-marker a-matrix width height)
    (define target (make-bitmap width height))
    (define dc (new bitmap-dc% [bitmap target]))
    (send dc set-pen "white" 0 'transparent)
    (send dc set-brush "white" 'solid)
    (send dc draw-rectangle 0 0 width height)
    (define rows (vector-length a-matrix))
    (define cols (vector-length (vector-ref a-matrix 0)))
    (define unit-x (/ width (+ cols 2.0)))
    (define unit-y (/ height (+ rows 2.0)))
    (for* ([row (in-range 0 rows)]
           [col (in-range 0 cols)])
      (if (zero? (matrix-ref a-matrix row col))
          (send dc set-brush "white" 'solid)
          (send dc set-brush "black" 'solid))
      (send dc draw-rectangle
            (+ unit-x (* col unit-x))
            (+ unit-y (* row unit-y)) unit-x unit-y))
    target)


  (define (make-board markers width height (separator 0.2))
    (define size (inexact->exact (ceiling (sqrt (length markers)))))
    (define W (inexact->exact (round (+ (* size width)
                                        (* (+ size 1) (* separator width))))))
    (define H (inexact->exact (round (+ (* size height)
                                        (* (+ size 1) (* separator height))))))
    (define bm (make-bitmap W H))
    (define dc (send bm make-dc))
    ;; make the background black
    ;;(send dc set-smoothing 'smoothed)
    (send dc set-brush "black" 'solid)
    (send dc draw-rectangle 0 0 W H)
    ;; draw as many markers as they fit
    (for ([marker (in-list markers)]
          [n (in-range 0 (length markers))])
      (define row (quotient n size))
      (define col (remainder n size))
      (define y-offset (+ (* height separator) (* row (+ height (* height separator)))))
      (define x-offset (+ (* width separator) (* col (+ width (* width separator)))))
      (send dc draw-bitmap (draw-marker marker width height) x-offset y-offset))
    bm)
)
