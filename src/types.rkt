(module types racket
  (require ffi/unsafe)
  (provide (all-defined-out))

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
  )
