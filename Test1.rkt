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

;; making my own window
(define c
  (tell (tell NSWindow alloc)
        initWithContentRect: #:type _NSRect (make-NSRect
                                             (make-NSPoint 0 0)
                                             (make-NSSize  640 480))
        styleMask: #:type _int (bitwise-ior
                                NSMiniaturizableWindowMask
                                NSResizableWindowMask
                                NSClosableWindowMask
                                NSBorderlessWindowMask
                                NSTexturedBackgroundWindowMask
                                ;;NSFullScreenWindowMask
                                )
        backing: #:type _int NSBackingStoreBuffered
        defer: #:type _BOOL YES))

(tell c setTitle: #:type _NSString "This Window is Awesome!")

(define NSOpenGLPFADoubleBuffer       5)
(define NSOpenGLPFADepthSize          12)
(define attributes (malloc 'atomic (_array _uint 2)))

;;(ptr-set! attributes _uint 0 NSOpenGLPFADoubleBuffer)
(ptr-set! attributes _uint 0 NSOpenGLPFAMultisample)
(ptr-set! attributes _uint 1 0)


(define pixFmt (tell (tell NSOpenGLPixelFormat alloc)
                     initWithAttributes: #:type _pointer attributes))

(define context (tell (tell NSOpenGLContext alloc) initWithFormat: pixFmt
                      shareContext: #f))

(tellv c setLevel: #:type _int NSScreenSaverWindowLevel)

(define init? #f)

(define (my-gl-init)
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
    (glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST)
    )
  ;; (glShadeModel GL_SMOOTH)
  ;; (glClearColor 0.0 0.0 0.0 1)
  ;; (glClearDepth 1)
  ;; (glEnable GL_DEPTH_TEST)
  ;; (glDepthFunc GL_LEQUAL)
  ;; (glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST))

(struct posn (x y z) #:mutable)

(define p1 (posn 0.0 1.0 0.0))
(define p2 (posn -1.0 0.0 0.0))
(define p3 (posn 1.0 0.0 0.0))
(define z 0.1)

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
  (glEnable GL_MULTISAMPLE)
  (glColor3f 1.0 0.0 0.0)
  (glVertex3f (posn-x p1) (posn-y p1) (posn-z p1))
  (glColor3f 0.0 0.0 1.0)
  (glVertex3f (posn-x p2) (posn-y p2) (posn-z p2))
  (glColor3f 1.0 0.0 0.0)
  (glVertex3f (posn-x p3) (posn-y p3) (posn-z p3))
  (glEnd))

;; (define (draw-shade bounds)
;;   NSGradient* aGradient = [[[NSGradient alloc]
;;                             initWithStartingColor:[NSColor orangeColor]
;;                             endingColor:[NSColor cyanColor]] autorelease];
;;   NSPoint centerPoint = NSMakePoint(NSMidX(bounds), NSMidY(bounds));
;;   NSPoint otherPoint = NSMakePoint(centerPoint.x + 60.0, centerPoint.y + 60.0);
;;   CGFloat firstRadius = MIN( ((bounds.size.width/2.0) - 2.0),
;;                              ((bounds.size.height/2.0) -2.0) );
;;   [aGradient drawFromCenter:centerPoint radius:firstRadius
;;              toCenter:otherPoint radius:5.0
;;              options:0];
;;   )


(define (my-gl-draw context)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glClearColor 0.0 0.0 0.0 1.0)
  (draw-triangles))


(define-objc-class my-gl-view NSOpenGLView
  (context)
  ;; (- _void (drawRect: [_NSRect exposed-rect])
  ;;    (super-tell drawRect: #:type _NSRect exposed-rect)
  ;;    (define origin (NSRect-origin exposed-rect))
  ;;    (define size (NSRect-size exposed-rect))
  ;;    (printf "size: ~a, ~a, origin: ~a, ~a~n"
  ;;            (NSSize-width size)
  ;;            (NSSize-height size)
  ;;            (NSPoint-x origin)
  ;;            (NSPoint-y origin))
  ;;    (unless init?
  ;;      (my-gl-init)
  ;;      (set! init? #t))
  ;;    (my-gl-draw context)
  ;;    (glFinish)
  ;;    (tellv context flushBuffer))
  (- _void (update)
     (super-tell update)
     (define b (tell #:type _NSRect self bounds))
     (define or (NSRect-origin b))
     (define s (NSRect-size b))
     (printf "b_size: ~a, ~a, b_origin: ~a, ~a~n"
             (NSSize-width s)
             (NSSize-height s)
             (NSPoint-x or)
             (NSPoint-y or))
     (unless init?
       (my-gl-init)
       (set! init? #t))
     (my-gl-draw context)
     (glFinish)
     (tellv context flushBuffer)))


(define glv
  (tell (tell my-gl-view alloc)
        initWithFrame: #:type _NSRect (make-NSRect
                                       (make-NSPoint 0 0)
                                       (make-NSSize  640 480))
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

(change-z -0.2)

(define (change-xrot diff)
  (set! xrot (+ xrot diff))
  (tellv glv update))

(define (change-yrot diff)
  (set! yrot (+ yrot diff))
  (tellv glv update))

(define (change-zrot diff)
  (set! zrot (+ zrot diff))
  (tellv glv update))
