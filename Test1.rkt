(require ffi/unsafe/objc
         ffi/unsafe
         ffi/unsafe/nsstring
         ffi/unsafe/define
         sgl/gl)

(define (<< a b) (arithmetic-shift a b))
(define NSBorderlessWindowMask 0)
(define NSUtilityWindowMask (1 . << . 4))
(define NSTexturedBackgroundWindowMask (1 . << . 8))
(define NSResizableWindowMask 8)
(define NSClosableWindowMask 2)
(define NSBackingStoreBuffered 2)
(define NSMiniaturizableWindowMask 4)
(define NSFullScreenWindowMask      (<< 1 14))

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

;; making my own window
(define c
  (tell (tell NSWindow alloc)
        initWithContentRect: #:type _NSRect (make-NSRect
                                             (make-NSPoint 100 100)
                                             (make-NSSize  400 400))
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

(tell #:type _BOOL c isOpaque)
(tell #:type _BOOL c isVisible)
(tell #:type _BOOL c isKeyWindow)
(tell #:type _BOOL c canBecomeKeyWindow)



;;(tellv c toggleFullScreen: c)
(tell c setTitle: #:type _NSString "This Window is Awesome!")

;;(tellv c setLevel: #:type _int NSScreenSaverWindowLevel)
;;(tellv c close)

(define NSOpenGLPFADoubleBuffer       5)
(define NSOpenGLPFADepthSize          12)
(define attributes (malloc 'atomic (_array _uint 1)))

;;(ptr-set! attributes _uint 0 NSOpenGLPFADoubleBuffer)
(ptr-set! attributes _uint 0 0)


(define pixFmt (tell (tell NSOpenGLPixelFormat alloc)
                     initWithAttributes: #:type _pointer attributes))

(define context (tell (tell NSOpenGLContext alloc) initWithFormat: pixFmt
                      shareContext: #f))

(define init? #f)

(define (my-gl-init)
  (glShadeModel GL_SMOOTH)
  ;;(glClearColor 0.0 0.0 0.0 1)
  (glClearDepth 1)
  (glEnable GL_DEPTH_TEST)
  (glDepthFunc GL_LEQUAL)
  (glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST))


(define (draw-triangles)
  (glColor3f 1.0 0.85 0.35)
  (glBegin GL_TRIANGLES)
  (glVertex3f 0.0 0.6 0.0)
  (glVertex3f -0.2 -0.3 0.0)
  (glVertex3f 0.2 -0.3 0.0)
  (glEnd))

(define (my-gl-draw context)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glClearColor 0 0.3 0.3 0)
  (draw-triangles))


(define (draw-box)
  (glLoadIdentity)
  (glTranslated 0 0 10)
  (glRotated 0 1 0 0)
  (glRotated 0 0 1 0)
  (glRotated 0 0 0 1)
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
  (glColor3f    1.0  1.0 0.0 );
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
  (glBegin GL_POLYGON)
  (glColor3f    1.0  0.0  0.0 )
  (glVertex3f   0.5 -0.5 -0.5 )
  (glVertex3f   0.5 -0.5  0.5 )
  (glVertex3f  -0.5 -0.5  0.5 )
  (glVertex3f  -0.5 -0.5 -0.5 )
  (glEnd))

(define gl-lib (ffi-lib
                "/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL"))

(define-ffi-definer define-gl
  (ffi-lib "/opt/local/lib/libopencv_highgui"))


(define-objc-class my-gl-view NSOpenGLView
  (context)
  (- _void (update)
     (super-tell update)
     (unless init?
       (my-gl-init)
       (set! init? #t))
     (my-gl-draw context)
     (glFinish)
     (printf "flush.update!~n")
     (tellv context flushBuffer)))


(define glv
  (tell (tell my-gl-view alloc)
        initWithFrame: #:type _NSRect (make-NSRect
                                       (make-NSPoint 100 100)
                                       (make-NSSize  400 400))
        pixelFormat: pixFmt))

(tellv c setContentView: glv)
(tellv c makeKeyAndOrderFront: #:type _BOOL YES)

(tellv glv update)
