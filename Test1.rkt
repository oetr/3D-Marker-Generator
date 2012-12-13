(require ffi/unsafe/objc
         ffi/unsafe
         ffi/unsafe/nsstring
         ffi/unsafe/define
         ffi/cvector
         sgl/gl
         sgl/gl-vectors
         "types.rkt"
         "utilities.rkt"
         "constants.rkt"
         "geometry.rkt"
         "ArUco.rkt")

(import-class NSWindow NSString NSOpenGLView NSOpenGLPixelFormat NSOpenGLContext)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants to make the window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define W 640)
(define H 480)

;; constants for marker generation
(define size 10.0)    ;; board size
(define markers-n 10) ;; number of markers
(define gap 0.2)      ;; gap in percent of the marker size
(define marker-size (/ size (+ markers-n (* (- markers-n 1) gap))))
(define tile-size (/ marker-size 7.0)) ;; size of the bit in the marker

;; field of view of the camera
(define fov 43.6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting attributes of window, pixel format,
;; and the opengl context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define window
  (tell (tell NSWindow alloc)
        initWithContentRect: #:type _NSRect (make-NSRect
                                             (make-NSPoint 640 276) ;; window position
                                             (make-NSSize  W H))
        styleMask: #:type _int (bitwise-ior
                                NSTitledWindowMask
                                NSMiniaturizableWindowMask
                                NSClosableWindowMask)
        backing: #:type _int NSBackingStoreBuffered
        defer: #:type _BOOL YES))

(tell window setTitle: #:type _NSString "Fiducial Marker Generator")

(define attributes
  (array #:type _NSOpenGLPixelFormatAttribute
         NSOpenGLPFAAllRenderers
         NSOpenGLPFAPixelBuffer
         NSOpenGLPFAAccelerated
         NSOpenGLPFAMultisample
         NSOpenGLPFASampleBuffers 1
         NSOpenGLPFASamples 16
         NSOpenGLPFADepthSize 64
         NSOpenGLPFAColorSize 24
         NSOpenGLPFASupersample
         0))


(define pixFmt (tell (tell NSOpenGLPixelFormat alloc)
                     initWithAttributes: #:type _pointer (array-ptr attributes)))

(define context (tell (tell NSOpenGLContext alloc) initWithFormat: pixFmt
                      shareContext: #f))

;; Puts the window on top of all other windows (useful when debugging)
(tellv window setLevel: #:type _int NSScreenSaverWindowLevel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init and draw procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define init? #f)

(define (my-gl-init)
  (glClearColor 0.0 0.0 0.0 0.0)
  (glClearDepth 1))

;; camera coordinates
(define x 0.0)
(define y 0.0)
(define z 0.0)

;; camera rotation
(define xrot 0)
(define yrot 0)
(define zrot 0)

;; coordinates of the board
(define x-obj 0.0)
(define y-obj 0.0)
(define z-obj 0.0)

;; generate markers
(define markers (list->vector (map generate-marker (range 0 (sqr markers-n)))))

(define (my-gl-draw)
  (glClearColor 0.0 0.0 0.0 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glLoadIdentity)
  (gluLookAt x y z x-obj y-obj z-obj 0 1 0)  
  (glColor3f 1.0 1.0 1.0)
  
  (glPushMatrix)
  (glTranslated (- (/ (- size tile-size) 2.0)) (/ (- size tile-size) 2.0) 0.0)
  (for ([y (in-range 0 markers-n 1)])
    (define translate-y (* y (+ marker-size (* marker-size gap))))
    (for ([x (in-range 0 markers-n 1)])
      (define translate-x (* x (+ marker-size (* marker-size gap))))
      (glPushMatrix)
      (glTranslated translate-x (- translate-y) 0.0)
      ;; get the marker
      (define marker (vector-ref markers (+ (* y markers-n) x)))
      (for ([row (in-range 0 7)])
        (for ([column (in-range 0 7)])
          (glPushMatrix)
          (glTranslated (* column tile-size)
                        (* (- row) tile-size) 0.0)
          (if (or (zero? row) (zero? column)
                  (= row 6) (= column 6))
              (glColor3f 1 1 1)
              (if (zero? (matrix-ref marker (- row 1) (- column 1)))
                  (glColor3f 1 1 1)
                  (glColor3f 0 0 0)))
          (glutRectangle tile-size)
          (glPopMatrix)))
      (glPopMatrix)))
  (glPopMatrix)
  (glFlush))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extend the NSOpenGLView to make my own custom view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: put my-gl-init into the overwritten OpenGL initialization procedure
;; instead of asking each time whether the OpenGL context has been initialized
(define-objc-class my-gl-view NSOpenGLView
  (context)
  (- _void (prepareOpenGL)
     (super-tell prepareOpenGL))
  (- _void (update)
     (super-tell update)
     (unless init?
       (my-gl-init)
       (set! init? #t))
     (my-gl-draw)
     (glFlush)
     (tellv context flushBuffer)))

;; Instantiate my OpenGL view
(define glv
  (tell (tell my-gl-view alloc)
        initWithFrame: #:type _NSRect (make-NSRect
                                       (make-NSPoint 0 0)
                                       (make-NSSize  W H))
        pixelFormat: pixFmt))

;; Add the OpenGL view to the window and make the window visible
(tellv window setContentView: glv)
(tellv window  makeKeyAndOrderFront: #:type _BOOL YES)
(tellv glv update)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions to manipulate the camera
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (change-viewport x y width height fov ar near far)
  (glViewport x y width height)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (gluPerspective fov ar near far)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
  (tellv glv update))

(define (set-camera-position! x-prime y-prime z-prime)
  (set! x x-prime)
  (set! y y-prime)
  (set! z z-prime)
  (tellv glv update))

(define (set-look-at-position! x-prime y-prime z-prime)
  (set! x-obj x-prime)
  (set! y-obj y-prime)
  (set! z-obj z-prime)
  (tellv glv update))

(define (randomize-camera)
  ;; set camera position
  (set! x (- (* size (random)) (/ size 2.0)))
  (set! y (- (* size (random)) (/ size 2.0)))
  (set! z (+ (* size (random)) (* size 0.1)))
  ;; set view position
  (set! x-obj (- (* size (random)) (* size 0.5)))
  (set! y-obj (- (* size (random)) (* size 0.5)))
  (set! z-obj 0.0)
  (tellv glv update))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test data generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(randomize-camera)
(change-viewport 0 0 W H fov (/ W H 1.0) 0.00001 1000.0)
(set-look-at-position! 0 0 0)
(set-camera-position! -0.5 0.5 0.5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Capture bitmap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bm-cv (make-cvector _ubyte (* W H 4)))
(glReadPixels 0 0 W H GL_RGBA GL_UNSIGNED_BYTE bm-cv)
(define bm (list->bytes (cvector->list bm-cv)))
(define res (opengl->bitmap (rgba->argb bm W H) W H))
(define r (make-bitmap W H))
(send r set-argb-pixels 0 0 W H res)
(send r save-file "test.png" 'png)
