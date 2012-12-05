;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Markers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


(define (visualize-markers markers width height (separator 0.2))
  (define size (inexact->exact (ceiling (sqrt (length markers)))))
  (define W (inexact->exact (round (+ (* size width)
                                      (* (+ size 1) (* separator width))))))
  (define H (inexact->exact (round (+ (* size height)
                                      (* (+ size 1) (* separator height))))))
  (define frame (new frame%
                     [label "Example"]
                     [width W]
                     [height H]))
  (define canvas (new canvas% [parent frame]
                      [min-width W]
                      [min-height H]))
  (define c-dc (send canvas get-dc))
  (define bm (send canvas make-bitmap W H))
  (define dc (send bm make-dc))
  (send frame show #t)
  (sleep/yield 0.1)
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
  (send c-dc draw-bitmap bm 0 0)
  (send bm save-file "test.png" 'png)
  bm)

(define (draw-markers markers width height (separator 0.2))
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




(define markers (map generate-marker (range 0 80)))
;;(define bm (visualize-markers (map generate-marker (range 0 100)) 100 100 0.2))
(define bm (draw-markers (map generate-marker (range 0 100)) 100 100 0.2))
;; what about opengl and synthetic images?



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require sgl sgl/gl-vectors)
(require sgl/gl-vectors)
(require sgl/gl)

(define gl-draw void)

(define gl-init
  (lambda ()
    (glShadeModel GL_SMOOTH)
    (glClearColor 0.0 0.0 0.0 0.5)
    (glClearDepth 1)
    (glEnable GL_DEPTH_TEST)
    (glDepthFunc GL_LEQUAL)
    (glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST)))

(define (set-gl-draw-fn fn)
  (set! gl-draw fn))

(define (set-gl-init-fn fn)
  (set! gl-init fn))

;; A function that recorrects for a new aspect ratio when the window is resized
(define (gl-resize width height)
  (glViewport 0 0 width height)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (gluPerspective 45 (/ width height) 0.1 100)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity))

(define (recursive-handle-key list code)
  (cond
   ((empty? list) void)
   ((equal? (caar list) code) ((car (cdr (car list)))))
   (else (recursive-handle-key (rest list) code))))

(define *key-mappings* '())

(define (add-key-mapping key fn)
  (set! *key-mappings* (cons (list key fn) *key-mappings*)))

(define (clear-key-mappings)
  (set! *key-mappings* '()))

(define (gl-handlekey key)
  (recursive-handle-key *key-mappings* (send key get-key-code)))


(define glcanvas%
  (class canvas%
    (inherit refresh with-gl-context swap-gl-buffers)
    (define init? #f)
    (define/override (on-paint)
      (with-gl-context
       (lambda ()
         (unless init?
           (gl-init)
           (set! init? #t))
         (gl-draw)
         (swap-gl-buffers)))
      (queue-callback (lambda () (refresh)) #f))
    (define/override (on-size w h)
      (with-gl-context
       (lambda ()
         (gl-resize w h)))
      (refresh))
    (define/override (on-char key)
      (gl-handlekey key)
      (refresh))
    (super-new (style '(gl no-autoclear)))))

(define *textures* '())

(define init-textures
  (lambda (count)
    (set! *textures* (glGenTextures count))))

(define (bitmap->gl-vector bmp)
  (let* ([dc (instantiate bitmap-dc% (bmp))]
         [pixels (* (send bmp get-width) (send bmp get-height))]
         [vec (make-gl-ubyte-vector (* pixels 3))]
         [data (make-bytes (* pixels 4))]
         [i 0])
    (send dc get-argb-pixels 0 0 (send bmp get-width) (send bmp get-height) data)
    (letrec
        ([loop
          (lambda ()
            (when (< i pixels)
              (begin
                (gl-vector-set! vec (* i 3)
                                (bytes-ref data (+ (* i 4) 1)))
                (gl-vector-set! vec (+ (* i 3) 1)
                                (bytes-ref data (+ (* i 4) 2)))
                (gl-vector-set! vec (+ (* i 3) 2)
                                (bytes-ref data (+ (* i 4) 3)))
                (set! i (+ i 1))
                (loop))))])
      (loop))
    (send dc set-bitmap #f)
    (list (send bmp get-width) (send bmp get-height) vec)))

(define (image->gl-vector file)
  (bitmap->gl-vector (make-object bitmap% file 'unknown #f)))

(define (gl-load-texture image-vector width height min-filter mag-filter ix)
  (glBindTexture GL_TEXTURE_2D (gl-vector-ref *textures* ix))
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER min-filter)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER mag-filter)
  (let* ([new-width width]
         [new-height height]
         [new-img-vec (make-gl-ubyte-vector (* new-width new-height 3))])
    (gluScaleImage GL_RGB
                   width height GL_UNSIGNED_BYTE image-vector
                   new-width new-height GL_UNSIGNED_BYTE new-img-vec)
    (if (or (= min-filter GL_LINEAR_MIPMAP_NEAREST)
            (= mag-filter GL_LINEAR_MIPMAP_NEAREST))
        (gluBuild2DMipmaps GL_TEXTURE_2D 3 new-width new-height GL_RGB
                           GL_UNSIGNED_BYTE new-img-vec)
        (glTexImage2D GL_TEXTURE_2D 0 3 new-width new-height 0 GL_RGB
                      GL_UNSIGNED_BYTE new-img-vec))))

(define get-texture
  (lambda (ix)
    (gl-vector-ref *textures* ix)))

(define *texture* (bitmap->gl-vector bm))

;; Init function
(define (my-gl-init)
  (let ((res *texture*))
    ;; Same texture, three smoothing styles...
    (init-textures 3)
    (unless (gl-load-texture (list-ref res 2) (list-ref res 0) (list-ref res 1)
                             GL_NEAREST GL_NEAREST 0)
      (error "Couldn't load texture"))
    (unless (gl-load-texture (list-ref res 2) (list-ref res 0) (list-ref res 1)
                             GL_LINEAR GL_LINEAR 1)
      (error "Couldn't load texture"))
    (unless (gl-load-texture (list-ref res 2) (list-ref res 0) (list-ref res 1)
                             GL_LINEAR GL_LINEAR_MIPMAP_NEAREST 2)
      (error "Couldn't load texture"))
    ;; Set-up alpha blending 50% transparency
    (glColor4d 1 1 1 0)
    (glBlendFunc GL_SRC_ALPHA GL_ONE)
    (glEnable GL_BLEND)
    ;; Standard Init
    (glEnable GL_TEXTURE_2D)
    (glShadeModel GL_SMOOTH)
    (glClearColor 0.0 0.0 0.0 0.5)
    (glClearDepth 1)
    (glEnable GL_DEPTH_TEST)
    (glDepthFunc GL_LEQUAL)
    (glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST)))

;; Our user interaction variables
(define *xrot* 0)
(define *yrot* 0)
(define *zrot* 0)
(define *z* -5)
(define *blend* #f)
(define *tex* 0)

(define (reset-vars!)
  (set! *xrot* 0)
  (set! *yrot* 0)
  (set! *zrot* 0)
  (set! *z* -1.2)
  (set! *blend* #f)
  (set! *tex* 0))


;; Our main function that does the drawing
(define (my-gl-draw)
  ;; erase the background
  (glClear (+ GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  ;; turn blending on/off
  (if *blend* (glEnable GL_BLEND) (glDisable GL_BLEND))
  ;; draw cube.
  
  (glLoadIdentity)
  (glTranslated 0 0 *z*)
  (glRotated *xrot* 1 0 0)
  (glRotated *yrot* 0 1 0)
  (glRotated *zrot* 0 0 1)
  (glBindTexture GL_TEXTURE_2D (get-texture *tex*))
  (glBegin GL_QUADS)
  ;; front
  (glTexCoord2i 0 1)
  (glVertex3f -0.5 -0.5 0.0)
  (glTexCoord2i 1 1)
  (glVertex3f 0.5 -0.5 0.0)
  (glTexCoord2i 1 0)
  (glVertex3f 0.5 0.5 0.0)
  (glTexCoord2i 0 0)
  (glVertex3f -0.5 0.5 0.0)
  (glEnd)
  (glFlush))

;; Our user interaction variables
(define *xrot* 0)
(define *yrot* 0)
(define *zrot* 0)
(define *z* -5)
(define *blend* #f)
(define *tex* 0)

(define (reset-vars!)
  (set! *xrot* 0)
  (set! *yrot* 0)
  (set! *zrot* 0)
  (set! *z* -3.0)
  (set! *blend* #f)
  (set! *tex* 0))

(define (gl-run W H)
  (let* ((frame (new frame% (label "OpenGL Window")
                     (width W)
                     (height H)))
         (glcanvas (new glcanvas% (parent frame)
                        (min-width W)
                        (min-height H))))
    (unless (send (send (send glcanvas get-dc) get-gl-context) ok?)
      (display "Error: OpenGL context failed to initialize")
      (newline)
      (exit))
    (send frame show #t)))

(clear-key-mappings)
;; Move forward
(add-key-mapping #\i (lambda () (set! *z* (+ *z* 0.3))))
;; Move backward
(add-key-mapping #\k (lambda () (set! *z* (- *z* 0.3))))
;; Turn blending on/off
(add-key-mapping #\b (lambda () (set! *blend* (not *blend*))))
;; Cycle textures
(add-key-mapping #\t (lambda () (set! *tex* (modulo (+ *tex* 1) 3))))
(add-key-mapping 'up (lambda () (set! *xrot* (+ *xrot* 2))))
(add-key-mapping 'down (lambda () (set! *xrot* (- *xrot* 2))))
(add-key-mapping 'left (lambda () (set! *yrot* (+ *yrot* 2))))
(add-key-mapping 'right (lambda () (set! *yrot* (- *yrot* 2))))
(add-key-mapping #\w (lambda () (set! *zrot* (+ *zrot* 2))))
(add-key-mapping #\q (lambda () (set! *zrot* (- *zrot* 2))))
(add-key-mapping #\r (lambda () (reset-vars!)))
(add-key-mapping #\s (lambda () (reset-vars!)))

(define (my-gl-init)
  (glShadeModel GL_SMOOTH)
  (glClearColor 0.0 0.0 0.0 0.5)
  (glClearDepth 1)
  (glEnable GL_DEPTH_TEST)
  (glDepthFunc GL_LEQUAL)
  (glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST))

(define x-rot 0.0)
(define y-rot 0.0)
(define z-rot -10.0)

(define (my-gl-draw)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glLoadIdentity)
  (gluLookAt 0.0 0.0 -10.0 0.0 0.0 0.0 0.0 0.0 0.0)
  (glTranslated 0 0 *z*)
  (glRotated *xrot* 1 0 0)
  (glRotated *yrot* 0 1 0)
  (glRotated *zrot* 0 0 1)

  (glLoadIdentity)
  (glTranslated 0 0 *z*)
  (glRotated *xrot* 1 0 0)
  (glRotated *yrot* 0 1 0)
  (glRotated *zrot* 0 0 1)
  
  (glBegin GL_POLYGON);
  (glColor3f  1.0 0.0 0.0 );
  (glVertex3f   0.5 -0.5 -0.5 );      ;; P1 is red
  (glColor3f  0.0 1.0 0.0 );
  (glVertex3f   0.5  0.5 -0.5 ); ;; P2 is green
  (glColor3f  0.0 0.0 1.0 );
  (glVertex3f  -0.5  0.5 -0.5 );      ;; P3 is blue
  (glColor3f  1.0 0.0 1.0 );
  (glVertex3f  -0.5 -0.5 -0.5 );      ;; P4 is purple
  (glEnd );

  ;; White side - BACK
  (glBegin GL_POLYGON);
  (glColor3f    1.0  1.0 1.0 );
  (glVertex3f   0.5 -0.5 0.5 );
  (glVertex3f   0.5  0.5 0.5 );
  (glVertex3f  -0.5  0.5 0.5 );
  (glVertex3f  -0.5 -0.5 0.5 );
  (glEnd );
  
  ;; Purple side - RIGHT
  (glBegin GL_POLYGON);
  (glColor3f   1.0  0.0  1.0 );
  (glVertex3f  0.5 -0.5 -0.5 );
  (glVertex3f  0.5  0.5 -0.5 );
  (glVertex3f  0.5  0.5  0.5 );
  (glVertex3f  0.5 -0.5  0.5 );
  (glEnd );
  
  ;; Green side - LEFT
  (glBegin GL_POLYGON);
  (glColor3f    0.0  1.0  0.0 );
  (glVertex3f  -0.5 -0.5  0.5 );
  (glVertex3f  -0.5  0.5  0.5 );
  (glVertex3f  -0.5  0.5 -0.5 );
  (glVertex3f  -0.5 -0.5 -0.5 );
  (glEnd );
  
  ;; Blue side - TOP
  (glBegin GL_POLYGON);
  (glColor3f    0.0  0.0  1.0 );
  (glVertex3f   0.5  0.5  0.5 );
  (glVertex3f   0.5  0.5 -0.5 );
  (glVertex3f  -0.5  0.5 -0.5 );
  (glVertex3f  -0.5  0.5  0.5 );
  (glEnd );
  
  ;; Red side - BOTTOM
  (glBegin GL_POLYGON);
  (glColor3f    1.0  0.0  0.0 );
  (glVertex3f   0.5 -0.5 -0.5 );
  (glVertex3f   0.5 -0.5  0.5 );
  (glVertex3f  -0.5 -0.5  0.5 );
  (glVertex3f  -0.5 -0.5 -0.5 );
  (glEnd );

  (glFlush))

(set-gl-init-fn my-gl-init)
(set-gl-draw-fn my-gl-draw)

(gl-run 640 480)
