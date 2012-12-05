(require sgl sgl/gl-vectors)
(require sgl/gl-vectors)
(require sgl/gl)

;; Init function
;; (define (my-gl-init)
;;   (glLoadIdentity)
;;   (glOrtho -1.0 1.0 -1.0 1.0 -1.0 1.0))

;; (define (my-gl-draw)
;;   (glClear GL_COLOR_BUFFER_BIT)
;;   (glBegin GL_POLYGON)
;;   (glVertex2f -0.5 -0.5)
;;   (glVertex2f -0.5 0.5)
;;   (glVertex2f 0.5 0.5)
;;   (glVertex2f 0.5 -0.5)
;;   (glEnd)
;;   (glFlush))

(define W 600)
(define H 600)
(define frame (new frame%
                   [label "Example"]
                   [width W]
                   [height H]))
;; variables
(define init? #f)

(define gl-config (new gl-config%))
(define bm (make-gl-bitmap W H gl-config))
(define dc (send bm make-dc))
(define gl-context (send dc get-gl-context))


(define canvas (new canvas%
                    [parent frame]
                    [min-width W]
                    [min-height H]
                    [style '(gl)]
                    [paint-callback
                     (lambda (c dc)
                       (unless init?
                         (send gl-context call-as-current my-gl-init)
                         (set! init? #t))                       
                       (send gl-context call-as-current my-gl-draw)
                       (send gl-context swap-buffers)
                       ;;(send c swap-gl-buffers)
                       (send dc draw-bitmap bm 0 0))]))

(send frame show #t)
(sleep/yield 3)
