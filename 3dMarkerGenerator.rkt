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

(require sgl/gl-vectors)
(require sgl/gl)

(define (init)
  ;;(glEnable GL_POINT_SMOOTH)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (glOrtho 0.0 1.0 0.0 1.0 -1.0 1.0)
  (glClearColor 0.0 0.0 0.0 0.0)
  (glClear GL_COLOR_BUFFER_BIT)
  )

(define (draw)
  ;;(glEnable GL_POINT_SMOOTH)
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
  ;;(glRectf 10 10 630 470)
  (glFlush)
  (glFinish)
  (sleep 0.001)
  )

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


(define (draw-my-stuff canvas)
  (define count-fps
    (let ([time (current-seconds)]
          [frames 0]
          [fps 0])
      (lambda ()
        (when (>= (- (current-seconds) time) 1)
          (set! fps frames)
          (set! time (current-seconds))
          (set! frames 0))
        (set! frames (add1 frames))
        (send (send canvas get-parent) set-status-text
              (format "~a fps, ~a°"
                      fps
                      (inexact->exact (modulo (floor spin) 360)))))))
  (define (loop)
    (send canvas with-gl-context draw)
    (send canvas swap-gl-buffers)
    (count-fps)
    (unless (send (send canvas get-parent) is-shown?)
      (printf "thread killed~n")
      (kill-thread (current-thread)))
    (loop))
  (send canvas with-gl-context init)
  ;; start state changer
  (define kill-state-change (change-state))
  (define t (thread loop))
  (lambda ()
    (kill-state-change)
    (kill-thread t)))

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

(define stop-drawing (f))



(define (generate-marker id)
  (define ids (vector #x10 #x17 #x09 #x0e))
  (for/list ([y (in-range 0 5)])
    (define index (bitwise-and (arithmetic-shift id (- (* 2 (- 4 y))))
                               3))
    (define val (vector-ref ids index))
    (for/list ([x (in-range 0 5)])
      (if (> (bitwise-and (arithmetic-shift val (- (- 4 x))) 1) 0)
          1
          0))))
