(require ffi/unsafe/objc
         ffi/unsafe
         ffi/unsafe/nsstring
         ffi/unsafe/define
         sgl/gl
         "constants.rkt"
         "geometry.rkt")

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
              NSOpenGLView NSOpenGLPixelFormat NSString NSOpenGLView NSOpenGLPixelFormat NSOpenGLContext)

(import-class NSAttributedString)
(import-protocol NSComboBoxDelegate)


(define first-string (tell (tell NSString alloc)
                           initWithUTF8String: #:type _string "test my shit"))

(tell #:type _int first-string length)

(define W 400)
(define H 400)

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
         ;;         NSOpenGLPFAPixelBuffer
         NSOpenGLPFAAccelerated
         NSOpenGLPFAMultisample
         NSOpenGLPFASampleBuffers 8
         NSOpenGLPFASamples 16
         NSOpenGLPFADepthSize 16
         NSOpenGLPFAColorSize 24
         NSOpenGLPFASupersample
         0))


(define pixFmt (tell (tell NSOpenGLPixelFormat alloc)
                     initWithAttributes: #:type _pointer (array-ptr attributes)))

(define context (tell (tell NSOpenGLContext alloc) initWithFormat: pixFmt
                      shareContext: #f))

(tellv c setLevel: #:type _int NSScreenSaverWindowLevel)

(define init? #f)

(define (my-gl-init)
  (glShadeModel GL_SMOOTH)
  (glClearColor 0.0 0.0 0.0 0.0)
  (glShadeModel GL_FLAT)
  (glClearDepth 1)
  )


(struct posn (x y z) #:mutable)

(define p1 (posn 0.0 1.0 0.0))
(define p2 (posn -1.0 0.0 0.0))
(define p3 (posn 1.0 0.0 0.0))
(define z 0.1)
(define t 0.0)

(define xrot 0)
(define yrot 0)
(define zrot 0)

(define (draw-triangles)
  (glLoadIdentity)
  (glTranslated 0.0 0.0 z)
  (glRotated xrot 1 0 0)
  (glRotated yrot 0 1 0)
  (glRotated zrot 0 0 1)
  
  (glBegin GL_TRIANGLES)
  (glColor3f 1.0 0.0 0.0)
  (glVertex3f (posn-x p1) (posn-y p1) (posn-z p1))
  (glColor3f 0.0 0.0 1.0)
  (glVertex3f (posn-x p2) (posn-y p2) (posn-z p2))
  (glColor3f 1.0 0.0 0.0)
  (glVertex3f (posn-x p3) (posn-y p3) (posn-z p3))
  (glEnd))

(define y 0.0)

(define (my-gl-draw)
  (glClearColor 1.0 0.0 0.0 1.0)
  (glClear GL_COLOR_BUFFER_BIT) ;;Clear the colour buffer (more buffers later on)  
  (glLoadIdentity) ;; Load the Identity Matrix to reset our drawing locations
  (gluLookAt 0 0 z 0 0 0 0 1 0)
  (glutWireCube 1.0) ;; Render the primitive
  (glutSolidCube 1.0) ;; Render the primitive  
  )

(define-objc-class my-gl-view NSOpenGLView
  (context)
  (- _void (prepareOpenGL)
     (super-tell prepareOpenGL))
   (- _void (reshape)
      ;;(super-tell reshape)
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
      (glLoadIdentity)		;; Reset The Projection Matrix
      (gluPerspective 40.0 (/ w h 1.0) 0.1 10000.0)
      (glMatrixMode GL_MODELVIEW)	;; Select The Modelview Matrix
      (glLoadIdentity)		;; Reset The Modelview Matrix
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

(define (move-p2 diff)
  (set-posn-x! p2 (+ (posn-x p2) diff))
  (tellv glv update))

(define (posn+ p1 p2)
  (posn (+ (posn-x p1) (posn-x p2))
        (+ (posn-y p1) (posn-y p2))
        (+ (posn-z p1) (posn-z p2))))

(define (posn+! p1 p2)
  (define new-posn (posn+ p1 p2))
  (set-posn-x! p1 (posn-x new-posn))
  (set-posn-y! p1 (posn-y new-posn))
  (set-posn-z! p1 (posn-z new-posn)))

(move-p2 0.1)
(move-p2 -0.1)

(define (change-z diff)
  (set! z (+ z diff))
  (tellv glv update))

(define (set-z! new-z)
  (set! z new-z)
  (tellv glv update))

(change-z -0.0)

(define (change-xrot diff)
  (set! xrot (+ xrot diff))
  (tellv glv update))

(define (change-yrot diff)
  (set! yrot (+ yrot diff))
  (tellv glv update))

(define (change-zrot diff)
  (set! zrot (+ zrot diff))
  (tellv glv update))

(define (change-t diff)
  (set! t (+ t diff))
  (tellv glv update))

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
(set-z! 10.0)
(change-z 0.0001)
