(require ffi/unsafe/objc
         ffi/unsafe
         ffi/unsafe/nsstring
         ffi/unsafe/define
         ffi/cvector
         sgl/gl
         sgl/gl-vectors
         "utilities.rkt"
         "constants.rkt"
         "geometry.rkt"
         "ArUco.rkt")

(define _NSInteger _long)
(define _NSUInteger _ulong)
(define _NSOpenGLPixelFormatAttribute _uint)

(define _OSStatus _sint32)

(define 64-bit? (= (ctype-sizeof _long) 8))

(define _CGFloat (make-ctype (if 64-bit? _double _float)
                             (lambda (v) (if (and (number? v)
                                             (exact? v))
                                        (exact->inexact v)
                                        v)) #f))

(define-cstruct _NSPoint ([x _CGFloat]
                          [y _CGFloat]))


(define-cstruct _CGImageRef
  ([width _int]
   [height _int]
   [bitsPerComponent _int]
   [bitsPerPixel _int]
   [bytesPerRow _int]
   [colorspace _pointer]
   [bitmapInfo _pointer]
   [provide _pointer]
   [decode _pointer]
   [shouldInterpolate _bool]
   [intent _pointer]))

(define-cstruct _NSSize ([width _CGFloat]
                         [height _CGFloat]))

(define-cstruct _NSRect ([origin _NSPoint][size _NSSize]))

(define-cstruct _NSRange ([location _NSUInteger]
                          [length _NSUInteger]))

(import-class NSObject)

(define NSNotFound (if 64-bit?
                       #x7fffffffffffffff
                       #x7fffffff))

(import-class NSArray NSView NSPanel NSGraphicsContext NSScroller NSComboBox NSWindow 
              NSImageView NSTextFieldCell 
              NSOpenGLView NSOpenGLPixelFormat NSString NSOpenGLView NSOpenGLPixelFormat NSOpenGLContext NSBitmapImageRep CGImageRef)

(import-class NSAttributedString)
(import-protocol NSComboBoxDelegate)

(define first-string (tell (tell NSString alloc)
                           initWithUTF8String: #:type _string "test my shit"))

(tell #:type _int first-string length)

(define W 640)
(define H 480)

;; making my own window
(define c
  (tell (tell NSWindow alloc)
        initWithContentRect: #:type _NSRect (make-NSRect
                                             (make-NSPoint 0 0)
                                             (make-NSSize  W H))
        styleMask: #:type _int (bitwise-ior
                                NSMiniaturizableWindowMask
                                NSResizableWindowMask
                                NSClosableWindowMask
                                ;;NSBorderlessWindowMask
                                ;;NSTexturedBackgroundWindowMask
                                ;;NSFullScreenWindowMask
                                )
        backing: #:type _int NSBackingStoreBuffered
        defer: #:type _BOOL YES))

(tell c setTitle: #:type _NSString "Fiducial Marker Generator")


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

(tellv c setLevel: #:type _int NSScreenSaverWindowLevel)

(struct posn (x y z) #:mutable)

(define t 0.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Textures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(define markers (map generate-marker (range 0 80)))
;;(define bm (make-board (map generate-marker (range 0 100)) 100 100 0.2))

;; (define *textures* '())

;; (define (init-textures count)
;;     (set! *textures* (glGenTextures count)))

;; (define (get-texture ix)
;;   (gl-vector-ref *textures* ix))

;; (define *texture* (bitmap->gl-vector bm))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init and draw procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define init? #f)

(define *tex* 2)

(define (my-gl-init)
  (glEnable GL_MULTISAMPLE)
  (glEnable GL_LINE_SMOOTH)
  ;;(glShadeModel GL_SMOOTH)
  
  ;;(glEnable GL_TEXTURE_2D)
  ;;(glEnable GL_BLEND)
  (glClearColor 0.0 0.0 0.0 0.0)
  (glEnable GL_POLYGON_SMOOTH)
  (glHint GL_POLYGON_SMOOTH GL_NICEST)
  (glShadeModel GL_FLAT)
  (glClearDepth 1)
  )

(define xrot 0)
(define yrot 0)
(define zrot 0)

(define x 0.0)
(define y 0.0)
(define z 0.0)

(define x-obj 0.0)
(define y-obj 0.0)
(define z-obj 0.0)

(define markers (list->vector (map generate-marker (range 0 100))))

(define (my-gl-draw)
  (glClearColor 0.0 0.0 0.0 1.0)
  (glClear GL_COLOR_BUFFER_BIT) ;;Clear the colour buffer (more buffers later on)  
  (glLoadIdentity) ;; Load the Identity Matrix to reset our drawing locations
  (gluLookAt x y z x-obj y-obj z-obj 0 1 0)  
  (glColor3f 1.0 1.0 1.0)

  (define size 1.0)
  (define markers-n 10)
  (define offset 0.2) ;; offset in percent
  (define marker-size (/ size (+ markers-n (* (- markers-n 1) offset))))
  (define tile-size (/ marker-size 7.0))
  
  (glPushMatrix)
  ;; (glTranslated (- (/ (- size marker-size) 2.0))
  ;;               (/ (- size marker-size) 2.0)
  ;;               0.0)
  (glTranslated (- (/ (- size tile-size) 2.0))
                (/ (- size tile-size) 2.0) 0.0)
  (for ([y (in-range 0 markers-n 1)])
    (define translate-y (* y (+ marker-size (* marker-size offset))))
    (for ([x (in-range 0 markers-n 1)])
      (define translate-x (* x (+ marker-size (* marker-size offset))))
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
          (glutRectangle tile-size) ;; Render the primitive
          (glPopMatrix)))
      (glPopMatrix)
      ))
  (glPopMatrix)
  ;; (glColor3f 1.0 1.0 1.0)
  ;; (glTranslated 0.0 0.0 0.0)
  ;; (glutRectangle 1.0) ;; Render the primitive
  (glFlush))


(define-objc-class my-gl-view NSOpenGLView
  (context)
  (- _void (prepareOpenGL)
     (super-tell prepareOpenGL))
   (- _void (reshape)
      (super-tell reshape)
      (define bounds (tell #:type _NSRect self bounds))
      (define origin (NSRect-origin bounds))
      (define size (NSRect-size bounds))
      (define x (NSPoint-x origin))
      (define y (NSPoint-y origin))
      (define w (NSSize-width size))
      (define h (NSSize-height size))
      (glViewport (inexact->exact x)
                  (inexact->exact y)
                  (inexact->exact w)
                  (inexact->exact h))
      (glMatrixMode GL_PROJECTION)	;; Select The Projection Matrix
      (glLoadIdentity)		        ;; Reset The Projection Matrix
      (gluPerspective 40.0 (/ w h 1.0) 0.1 10000.0)
      (glMatrixMode GL_MODELVIEW)	;; Select The Modelview Matrix
      (glLoadIdentity)		        ;; Reset The Modelview Matrix
      (glFlush)
      )
  (- _void (update)
     (super-tell update)
     (unless init?
       (my-gl-init)
       (set! init? #t))
     (my-gl-draw)
     (glFlush)
     ;;(glFinish)
     (tellv context flushBuffer)
     ))


(define glv
  (tell (tell my-gl-view alloc)
        initWithFrame: #:type _NSRect (make-NSRect
                                       (make-NSPoint 0 0)
                                       (make-NSSize  W H))
        pixelFormat: pixFmt))

(tellv c setContentView: glv)
(tellv c makeKeyAndOrderFront: #:type _BOOL YES)
(tellv glv update)

(define (change-z diff)
  (set! z (+ z diff))
  (tellv glv update))

(define (set-z! new-z)
  (set! z new-z)
  (tellv glv update))

(change-z -0.0)

(define (change-y diff)
  (set! y (+ y diff))
  (tellv glv update))

(define (change-viewport x y width height fov ar near far)
  (glViewport x y width height)
  (glMatrixMode GL_PROJECTION)	;; Select The Projection Matrix
  (glLoadIdentity)		;; Reset The Projection Matrix
  (gluPerspective fov ar near far)
  (glMatrixMode GL_MODELVIEW)	;; Select The Modelview Matrix
  (glLoadIdentity)		;; Reset The Modelview Matrix
  (tellv glv update))

(change-viewport 0 0 W H 43.6 (/ W H 1.0) 0.00001 1000.0)

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
  (set! x (- (* 2.0 (random)) 0.5))
  (set! y (- (* 2.0 (random)) 0.5))
  (set! z (+ (* 0.5 (random)) 0.5))
  ;; set view position
  (set! x-obj (- (* 1.0 (random)) .5))
  (set! y-obj (- (* 1.0 (random)) .5))
  (set! z-obj 0.0)
  (tellv glv update))

(define (increment-camera-position! x-diff y-diff z-diff)
  (set! x (+ x x-diff))
  (set! y (+ y y-diff))
  (set! z (+ z z-diff))
  (tellv glv update))

(define (compute-fov size distance)
  (/ (* 2.0 180.0 (atan (/ size 2.0) distance)) pi))

;;(randomize-camera)
(set! *tex* 0)
(set-camera-position! x y z)
(set-look-at-position! 0 0 0)
;;(set-camera-position! 0 0 2)
(set-camera-position! 0 0 1.25)

;; (define (t)
;;   (set-camera-position! (random 3) (random 3) (random 3))
;;   (sleep .5)
;;   (t))

;; (define k (thread t))


(define bm-cv (make-cvector _ubyte (* W H 4)))

(glReadPixels 0 0 W H GL_RGBA GL_UNSIGNED_BYTE bm-cv)

(define bm (list->bytes (cvector->list bm-cv)))
(define res (opengl->bitmap (rgba->argb bm W H) W H))

(define r (make-bitmap W H))
(send r set-argb-pixels 0 0 W H res)
(send r save-file "test.png" 'png)
