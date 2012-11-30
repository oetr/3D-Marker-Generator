(require sgl
         sgl/gl-vectors)


(define controls? #t)

(define gears-canvas%
  (class* canvas% ()

    (inherit refresh with-gl-context swap-gl-buffers get-parent
             get-top-level-window)

    (define rotation 0.0)

    (define view-rotx 20.0)
    (define view-roty 30.0)
    (define view-rotz 0.0)

    (define gear1 #f)
    (define gear2 #f)
    (define gear3 #f)

    (define step? #f)

    (define/public (run)
      (set! step? #t)
      (refresh))

    (define/public (move-left)
      (set! view-roty (+ view-roty 5.0))
      (refresh))

    (define/public (move-right)
      (set! view-roty (- view-roty 5.0))
      (refresh))

    (define/public (move-up)
      (set! view-rotx (+ view-rotx 5.0))
      (refresh))

    (define/public (move-down)
      (set! view-rotx (- view-rotx 5.0))
      (refresh))

    (define (build-gear inner-radius ; radius of hole at center
                        outer-radius ; radius at center of teeth
                        width ; width of gear
                        teeth ; number of teeth
                        tooth-depth) ; depth of tooth
      (let* ((r0 inner-radius)
             (r1 (- outer-radius (/ tooth-depth 2.0)))
             (r2 (+ outer-radius (/ tooth-depth 2.0)))
             (da (/ (* 2.0 pi) teeth 4.0))
             (da2 (* da 2))
             (da3 (* da 3))
             (half-width (* width 0.5))
             (neg-half-width (- half-width)))

        ;; TODO: Generalize away some more redundant program text.

        (gl-shade-model 'flat)

        (gl-normal 0.0 0.0 1.0)

        ;; Draw front face.
        (gl-begin 'quad-strip)
        (do ((i 0 (+ 1 i))) ((> i teeth))
          (let* ((angle (/ (* i 2.0 pi) teeth))
                 (cos-angle (cos angle))
                 (sin-angle (sin angle)))
            (gl-vertex (* r0 cos-angle) (* r0 sin-angle) half-width)
            (gl-vertex (* r1 cos-angle) (* r1 sin-angle) half-width)
            (when (< i teeth)
              (gl-vertex (* r0 cos-angle)
                         (* r0 sin-angle)
                         (* half-width))
              (gl-vertex (* r1 (cos (+ angle da3)))
                         (* r1 (sin (+ angle da3)))
                         half-width))))
        (gl-end)

        ;; Draw front sides of teeth.
        (gl-begin 'quads)
        (do ((i 0 (+ 1 i))) ((= i teeth))
          (let ((angle (/ (* i 2.0 pi) teeth)))
            (gl-vertex (* r1 (cos angle))
                       (* r1 (sin angle))
                       half-width)
            (gl-vertex (* r2 (cos (+ angle da)))
                       (* r2 (sin (+ angle da)))
                       half-width)
            (gl-vertex (* r2 (cos (+ angle da2)))
                       (* r2 (sin (+ angle da2)))
                       half-width)
            (gl-vertex (* r1 (cos (+ angle da3)))
                       (* r1 (sin (+ angle da3)))
                       half-width)))
        (gl-end)

        (gl-normal 0.0 0.0 -1.0)

        ;; Draw back face.
        (gl-begin 'quad-strip)
        (do ((i 0 (+ 1 i))) ((> i teeth))
          (let* ((angle (/ (* i 2.0 pi) teeth))
                 (cos-angle (cos angle))
                 (sin-angle (sin angle)))
            (gl-vertex (* r1 cos-angle) (* r1 sin-angle) neg-half-width)
            (gl-vertex (* r0 cos-angle) (* r0 sin-angle) neg-half-width)
            (when (< i teeth)
              (gl-vertex (* r1 (cos (+ angle da3)))
                         (* r1 (sin (+ angle da3)))
                         neg-half-width)
              (gl-vertex (* r0 cos-angle)
                         (* r0 sin-angle)
                         neg-half-width))))
        (gl-end)

        ;; Draw back sides of teeth.
        (gl-begin 'quads)
        (do ((i 0 (+ 1 i))) ((= i teeth))
          (let ((angle (/ (* i 2.0 pi) teeth)))
            (gl-vertex (* r1 (cos (+ angle da3)))
                       (* r1 (sin (+ angle da3)))
                       neg-half-width)
            (gl-vertex (* r2 (cos (+ angle da2)))
                       (* r2 (sin (+ angle da2)))
                       neg-half-width)
            (gl-vertex (* r2 (cos (+ angle da)))
                       (* r2 (sin (+ angle da)))
                       neg-half-width)
            (gl-vertex (* r1 (cos angle))
                       (* r1 (sin angle))
                       neg-half-width)))
        (gl-end)

        ;; Draw outward faces of teeth.
        (gl-begin 'quad-strip)
        (do ((i 0 (+ 1 i))) ((= i teeth))
          (let* ((angle (/ (* i 2.0 pi) teeth))
                 (cos-angle (cos angle))
                 (sin-angle (sin angle)))

            (gl-vertex (* r1 cos-angle) (* r1 sin-angle) half-width)
            (gl-vertex (* r1 cos-angle) (* r1 sin-angle) neg-half-width)

            (let* ((u (- (* r2 (cos (+ angle da))) (* r1 cos-angle)))
                   (v (- (* r2 (sin (+ angle da))) (* r1 sin-angle)))
                   (len (sqrt (+ (* u u) (* v v)))))
              (gl-normal (/ v len) (- (/ u len)) 0.0))

            (gl-vertex (* r2 (cos (+ angle da)))
                       (* r2 (sin (+ angle da)))
                       half-width)
            (gl-vertex (* r2 (cos (+ angle da)))
                       (* r2 (sin (+ angle da)))
                       neg-half-width)
            (gl-normal cos-angle sin-angle 0.0)
            (gl-vertex (* r2 (cos (+ angle da2)))
                       (* r2 (sin (+ angle da2)))
                       half-width)
            (gl-vertex (* r2 (cos (+ angle da2)))
                       (* r2 (sin (+ angle da2)))
                       neg-half-width)

            (let ((u (- (* r1 (cos (+ angle da3)))
                        (* r2 (cos (+ angle da2)))))
                  (v (- (* r1 (sin (+ angle da3)))
                        (* r2 (sin (+ angle da2))))))
              (gl-normal v (- u) 0.0))

            (gl-vertex (* r1 (cos (+ angle da3)))
                       (* r1 (sin (+ angle da3)))
                       half-width)
            (gl-vertex (* r1 (cos (+ angle da3)))
                       (* r1 (sin (+ angle da3)))
                       neg-half-width)
            (gl-normal cos-angle sin-angle 0.0)))

        (gl-vertex (* r1 (cos 0)) (* r1 (sin 0)) half-width)
        (gl-vertex (* r1 (cos 0)) (* r1 (sin 0)) neg-half-width)
        (gl-end)

        (gl-shade-model 'smooth)

        ;; Draw inside radius cylinder.
        (gl-begin 'quad-strip)
        (do ((i 0 (+ 1 i))) ((> i teeth))
          (let* ((angle (/ (* i 2.0 pi) teeth))
                 (cos-angle (cos angle))
                 (sin-angle (sin angle)))
            (gl-normal (- cos-angle) (- sin-angle) 0.0)
            (gl-vertex (* r0 cos-angle) (* r0 sin-angle) neg-half-width)
            (gl-vertex (* r0 cos-angle) (* r0 sin-angle) half-width)))
        (gl-end)))

    (define/private (report-no-gl)
      (message-box "Gears"
                   (string-append
                    "There was an error initializing OpenGL. "
                    "Maybe OpenGL is not supported on the current platform.")
                   (get-top-level-window)
                   '(ok stop))
      (exit 1))

    (define/override (on-size width height)
      (with-gl-context
       #:fail (lambda () (report-no-gl))
       (lambda ()

         (unless gear1
           (printf " RENDERER: ~A\n" (gl-get-string 'renderer))
           (printf " VERSION: ~A\n" (gl-get-string 'version))
           (printf " VENDOR: ~A\n" (gl-get-string 'vendor))
           (printf " EXTENSIONS: ~A\n" (gl-get-string 'extensions)))

         (gl-viewport 0 0 width height)
         (gl-matrix-mode 'projection)
         (gl-load-identity)
         (let ((h (/ height width)))
           (gl-frustum -1.0 1.0 (- h) h 5.0 60.0))
         (gl-matrix-mode 'modelview)
         (gl-load-identity)
         (gl-translate 0.0 0.0 -40.0)

         (gl-light-v 'light0 'position (vector->gl-float-vector
                                        (vector 5.0 5.0 10.0 0.0)))
         (gl-enable 'cull-face)
         (gl-enable 'lighting)
         (gl-enable 'light0)
         (gl-enable 'depth-test)

         (unless gear1

           (set! gear1 (gl-gen-lists 1))
           (gl-new-list gear1 'compile)
           (gl-material-v 'front
                          'ambient-and-diffuse
                          (vector->gl-float-vector (vector 0.8 0.1 0.0 1.0)))
           (build-gear 1.0 4.0 1.0 20 0.7)
           (gl-end-list)

           (set! gear2 (gl-gen-lists 1))
           (gl-new-list gear2 'compile)
           (gl-material-v 'front
                          'ambient-and-diffuse
                          (vector->gl-float-vector (vector 0.0 0.8 0.2 1.0)))
           (build-gear 0.5 2.0 2.0 10 0.7)
           (gl-end-list)

           (set! gear3 (gl-gen-lists 1))
           (gl-new-list gear3 'compile)
           (gl-material-v 'front
                          'ambient-and-diffuse
                          (vector->gl-float-vector (vector 0.2 0.2 1.0 1.0)))
           (build-gear 1.3 2.0 0.5 10 0.7)
           (gl-end-list)

           (gl-enable 'normalize))))
      (refresh))

    (define sec (current-seconds))
    (define frames 0)
    
    (define/override (on-paint)
      (when gear1
        (when (>= (- (current-seconds) sec) 5)
          (send (get-parent) set-status-text (format "~a fps" (/ (exact->inexact frames) 5)))
          (set! sec (current-seconds))
          (set! frames 0))
        (set! frames (add1 frames))
        
        (when step?
          ;; TODO: Don't increment this infinitely.
          (set! rotation (+ 2.0 rotation)))
        (with-gl-context
         #:fail (lambda () (report-no-gl))
         (lambda ()

           (gl-clear-color 0.0 0.0 0.0 0.0)
           (gl-clear 'color-buffer-bit 'depth-buffer-bit)

           (gl-push-matrix)
           (gl-rotate view-rotx 1.0 0.0 0.0)
           (gl-rotate view-roty 0.0 1.0 0.0)
           (gl-rotate view-rotz 0.0 0.0 1.0)

           (gl-push-matrix)
           (gl-translate -3.0 -2.0 0.0)
           (gl-rotate rotation 0.0 0.0 1.0)
           (gl-call-list gear1)
           (gl-pop-matrix)

           (gl-push-matrix)
           (gl-translate 3.1 -2.0 0.0)
           (gl-rotate (- (* -2.0 rotation) 9.0) 0.0 0.0 1.0)
           (gl-call-list gear2)
           (gl-pop-matrix)

           (gl-push-matrix)
           (gl-translate -3.1 4.2 0.0)
           (gl-rotate (- (* -2.0 rotation) 25.0) 0.0 0.0 1.0)
           (gl-call-list gear3)
           (gl-pop-matrix)

           (gl-pop-matrix)

           (swap-gl-buffers)
           (gl-flush)))
        (when step?
          (set! step? #f)
          (queue-callback (lambda x (send this run)) #f))))

    (super-instantiate () (style '(gl no-autoclear)))))

(define (f)
  (let* ((f (make-object frame% "gears.rkt" #f))
         (c (instantiate gears-canvas% (f) (min-width 300) (min-height 300))))
    (send f create-status-line)
    (when controls?
      (let ((h (instantiate horizontal-panel% (f)
                 (alignment '(center center)) (stretchable-height #f))))
        (instantiate button%
            ("Start" h (lambda (b e) (send b enable #f) (send c run)))
          (stretchable-width #t) (stretchable-height #t))
        (let ((h (instantiate horizontal-panel% (h)
                   (alignment '(center center)))))
          (instantiate button% ("Left" h (lambda x (send c move-left)))
            (stretchable-width #t))
          (let ((v (instantiate vertical-panel% (h)
                     (alignment '(center center)) (stretchable-width #f))))
            (instantiate button% ("Up" v (lambda x (send c move-up)))
              (stretchable-width #t))
            (instantiate button% ("Down" v (lambda x (send c move-down)))
              (stretchable-width #t)))
          (instantiate button% ("Right" h (lambda x (send c move-right)))
            (stretchable-width #t)))))
    (send f show #t)))
(f)

(require sgl/gl-vectors)
(require sgl/gl)

(define my-canvas%
  (class* canvas% () ; The base class is canvas%
    (inherit refresh with-gl-context swap-gl-buffers get-parent
             get-top-level-window)    

    (define/private (report-no-gl)
      (message-box "Gears"
                   (string-append
                    "There was an error initializing OpenGL. "
                    "Maybe OpenGL is not supported on the current platform.")
                   (get-top-level-window)
                   '(ok stop))
      (exit 1))
    
    (define/public (run)
      (refresh))
    (define frames 0)
    (define sec 0)
    
    ;; Define overriding method to handle openGL
    (define/override (on-paint)
      (with-gl-context
       #:fail (lambda () (report-no-gl))
       (lambda ()
         (draw-my-stuff)
         ;; show fps in the status text
         (when (>= (- (current-seconds) sec) 1)
           (send (get-parent) set-status-text
                 (format "~a fps" (exact->inexact frames)))
           (set! sec (current-seconds))
           (set! frames 0))
         (set! frames (add1 frames))
         (swap-gl-buffers)
         (glFlush)))
      (queue-callback (lambda x (sleep 0.1) (send this run)) #f))
    ;; Call the superclass init, passing on all init args    
    (super-instantiate () (style '(gl no-autoclear)))))

(require sgl sgl/gl-vectors)
(require sgl/gl-vectors)
(require sgl/gl)

(define (init)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glLoadIdentity)
  (glTranslatef -1.5 0.0 -6.0))


(define (draw)
  (glBegin GL_TRIANGLES)            ;;Drawing Using Triangles
  (glVertex3f 0.0 1.0 0.0)          ;;Top
  (glVertex3f -1.0 -1.0 0.0) ;; Bottom Left
  (glVertex3f 1.0 -1.0 0.0)   ;; Bottom Right
  (glEnd)
  (glFlush)
(glFinish)
(sleep 0.1))


(glEnable GL_POINT_SMOOTH)
(glViewport 0 0 640 480)
(glMatrixMode GL_PROJECTION)
(glLoadIdentity)
(glOrtho 0.0 640.0 0.0 480.0 0.0 1.0)
(glClear GL_COLOR_BUFFER_BIT)
(gluLookAt 0 0 5 0 0 0 0 1 0)
(glRotatef spin-x spin-y spin-z 1.0)
(glColor3f 0.2 6.0 3.0)
(glPointSize 20)
(glBegin GL_POINTS)  
(glVertex3f 340 220 0)
(glVertex3f 300 220 0)
(glVertex3f 300 260 0)
(glVertex3f 340 260 0)
(glEnd)
(glRectf 10 10 630 470)
(glFlush)
(glFinish)
(sleep 0.01)

(define (change-spin)
  (set! spin-x 0)
  (set! spin-y 0)
  (set! spin-z 0)
  (sleep 0.01))

(define (change-state)
  (define (loop)
    (change-spin)
    (loop))
  (define t (thread loop))
  (lambda ()
    (kill-thread t)))

(define x -100.0)
(define spin-x 0)
(define spin-y 0)
(define spin-z 0)

(define (gl-resize width height)
  (glViewport 0 0 width height)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (gluPerspective 45 (/ width height) 0.1 100)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity))

(define (draw-my-stuff canvas)
  (let ([kill-this empty])
  (define (loop)
    (send canvas with-gl-context draw)
    ;;(send canvas swap-gl-buffers)
    (unless (send (send canvas get-parent) is-shown?)
      (printf "thread killed~n")
      (kill-this)
      (kill-thread (current-thread)))
    (loop))
  (send canvas with-gl-context init)
  ;; start state changer
  (define kill-state-change (change-state))
  (define t (thread loop))
  (set! kill-this (lambda ()
                    (kill-state-change)
                    (kill-thread t)))
  kill-this))

(define (f)
  (let* ([frame (new frame% [label "Example"])]
         [canvas (new canvas%
                      [parent frame]
                      [style '(gl no-autoclear)]
                      [min-width 640]
                      [min-height 480])])
    (send frame create-status-line)
    (send frame show #t)
    (draw-my-stuff canvas)))

(define/override (on-size w h)
        (with-gl-context
         (lambda ()
           (gl-resize w h)))
        (refresh))

(define stop-drawing (f))



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

 (define (gl-run)
    (let* ((frame (new frame% (label "OpenGL Window")
                              (width 640)
                              (height 480)))
           (glcanvas (new glcanvas% (parent frame))))
      (unless (send (send (send glcanvas get-dc) get-gl-context) ok?)
        (display "Error: OpenGL context failed to initialize")
        (newline)
        (exit))
      (send frame show #t)))

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
  (set! *z* -5)
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
  (glTexCoord2i 0 0)
  (glVertex3i -1 -1 0)
  (glTexCoord2i 1 0)
  (glVertex3i 1 -1 0)
  (glTexCoord2i 1 1)
  (glVertex3i 1 1 0)
  (glTexCoord2i 0 1)
  (glVertex3i -1 1 0)
  (glEnd)
  (glFlush))


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


(set-gl-init-fn my-gl-init)
(set-gl-draw-fn my-gl-draw)

(gl-run)




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


(define markers (map generate-marker (range 0 80)))
(define bm (visualize-markers (map generate-marker (range 0 100)) 100 100 0.2))


;; what about opengl and synthetic images?
