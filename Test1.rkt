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

(define (<< a b) (arithmetic-shift a b))
(define NSBorderlessWindowMask 0)
(define NSUtilityWindowMask (1 . << . 4))
(define NSTexturedBackgroundWindowMask (1 . << . 8))
(define NSResizableWindowMask 8)
(define NSClosableWindowMask 2)
(define NSBackingStoreBuffered 2)
(define NSMiniaturizableWindowMask 4)
(define NSFullScreenWindowMask      (<< 1 14))
(define NSOpenGLPFAMultisample       59)

(define NSScreenSaverWindowLevel     1000)

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

(define W 600)
(define H 600)

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
         NSOpenGLPFASampleBuffers 2
         NSOpenGLPFASamples 16
         NSOpenGLPFADepthSize 32
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
(define markers (map generate-marker (range 0 80)))
(define bm (make-board (map generate-marker (range 0 100)) 100 100 0.2))

(define *textures* '())

(define (init-textures count)
    (set! *textures* (glGenTextures count)))

(define (get-texture ix)
  (gl-vector-ref *textures* ix))

(define *texture* (bitmap->gl-vector bm))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init and draw procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define init? #f)

(define *tex* 2)

(define (my-gl-init)
  (let ((res *texture*))
    ;; load textures
    (init-textures 4)
    (unless (gl-load-texture *textures*
                             (list-ref res 2) (list-ref res 0) (list-ref res 1)
                             GL_NEAREST GL_NEAREST 0)
      (error "Couldn't load texture"))
    (unless (gl-load-texture *textures*
                             (list-ref res 2) (list-ref res 0) (list-ref res 1)
                             GL_LINEAR GL_LINEAR 1)
      (error "Couldn't load texture"))
    (unless (gl-load-texture *textures*
                             (list-ref res 2) (list-ref res 0) (list-ref res 1)
                             GL_LINEAR GL_LINEAR_MIPMAP_NEAREST 2)
      (error "Couldn't load texture"))
    (unless (gl-load-texture *textures*
                             (list-ref res 2) (list-ref res 0) (list-ref res 1)
                             GL_LINEAR GL_LINEAR_MIPMAP_LINEAR 3)
      (error "Couldn't load texture"))
    (glShadeModel GL_SMOOTH)
    (glEnable GL_MULTISAMPLE)
    (glEnable GL_TEXTURE_2D)
    (glClearColor 0.0 0.0 0.0 0.0)
    (glShadeModel GL_FLAT)
    (glClearDepth 1)))

(define xrot 0)
(define yrot 0)
(define zrot 0)

(define x 0.0)
(define y 0.0)
(define z 0.0)

(define (my-gl-draw)
  (glClearColor 0.0 0.0 0.0 1.0)
  (glClear GL_COLOR_BUFFER_BIT) ;;Clear the colour buffer (more buffers later on)  
  (glLoadIdentity) ;; Load the Identity Matrix to reset our drawing locations
  (gluLookAt x y z 0 0 0 0 1 0)
  
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
  ;;(glFlush)
  ;;(glutWireCube 1.0) ;; Render the primitive
  ;;(glutSolidCube 1.0) ;; Render the primitive
  )


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

(change-viewport 0 0 W H 40.0 (/ W H 1.0) 0.1 1000.0)
(set-z! -100.0)

(define (set-camera-position! x-prime y-prime z-prime)
  (set! x x-prime)
  (set! y y-prime)
  (set! z z-prime)
  (tellv glv update))

(define (increment-camera-position! x-diff y-diff z-diff)
  (set! x (+ x x-diff))
  (set! y (+ y y-diff))
  (set! z (+ z z-diff))
  (tellv glv update))

(define (compute-fov size distance)
  (/ (* 2.0 180.0 (atan (/ size 2.0) distance)) pi))

(set-camera-position! (random 10) (random 10) (random 5))
(set-camera-position! 0 2 1.5)
(change-viewport 0 0 W H 43.6 (/ W H 1.0) 0.1 1000.0)

(set! *tex* 0)
(set-camera-position! 0 0 1.25)

;; (define (t)
;;   (set-camera-position! (random 3) (random 3) (random 3))
;;   (sleep .5)
;;   (t))

;; (define k (thread t))

;;(increment-camera-position! 0.0 0.0 (sin z))


;;(define bm (make-array #:type _ubyte (* W H 4) 0))

(define bm-cv (make-cvector _ubyte (* W H 4)))

(glReadPixels 0 0 W H GL_RGBA GL_UNSIGNED_BYTE bm-cv)

(define bm (list->bytes (cvector->list bm-cv)))

(define r (make-bitmap W H))
(send r set-argb-pixels 0 0 W H (rgba->argb bm W H))
(send r save-file "test.png" 'png)
