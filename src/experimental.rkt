(require ffi/unsafe/objc
         ffi/unsafe
         ffi/unsafe/nsstring
         ffi/unsafe/define
         ffi/cvector
         sgl/gl
         sgl/gl-vectors)

(define gl-lib (ffi-lib "/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL"))

(define-ffi-definer define-gl gl-lib)

;; Frame buffers
(define GL_FRAMEBUFFER                    #x8D40)
(define GL_COLOR_ATTACHMENT0              #x8CE0)
(define GL_COLOR_ATTACHMENT1              #x8CE1)
(define GL_DEPTH_ATTACHMENT               #x8D00)
(define GL_FRAMEBUFFER_COMPLETE           #x8CD5)
(define-gl glGenFramebuffers (_fun _int _pointer -> _void))
(define-gl glBindFramebuffer (_fun _uint _uint -> _void))
(define-gl glFramebufferRenderbuffer (_fun _uint _uint _uint _uint -> _void))
(define-gl glCheckFramebufferStatus (_fun _uint -> _uint))
(define-gl glDeleteFramebuffers (_fun _int _pointer -> _void))

;; Render buffers
(define GL_RENDERBUFFER                   #x8D41)
(define GL_MAX_SAMPLES                    #x8D57)
(define-gl glBindRenderbuffer (_fun _uint _uint -> _void ))
(define-gl glGenRenderbuffers (_fun _int _pointer -> _void))
(define-gl glRenderbufferStorage (_fun _uint _uint _int _int -> _void ))
(define-gl glRenderbufferStorageMultisample (_fun _uint _int _uint _int _int -> _void))
(define-gl glDeleteRenderbuffers (_fun _int _pointer -> _void))

;; Overwrite standard texture procedures
(define-gl glGenTextures (_fun _int _pointer -> _void))
(define-gl glTexImage2D
  (_fun _uint _int _int _int _int _int _uint _uint _pointer -> _void))
(define-gl glFramebufferTexture2D (_fun _uint _uint _uint _uint _int -> _void))
(define-gl glGetIntegerv (_fun _uint _pointer -> _void))



(define framebuffer (malloc 'atomic _uint 10))
(ptr-set! framebuffer _uint 0)
(define renderbuffer (malloc 'atomic _uint))
(ptr-set! renderbuffer _uint 0)

(glGenFramebuffers 10 framebuffer)
(glBindFramebuffer GL_FRAMEBUFFER (ptr-ref framebuffer _uint))

(glGenRenderbuffers 1 renderbuffer)
(glRenderbufferStorage GL_RENDERBUFFER GL_RGBA 640 480)

(glFramebufferRenderbuffer GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_RENDERBUFFER (ptr-ref renderbuffer _uint))
(define status (glCheckFramebufferStatus GL_FRAMEBUFFER))





;;; Trying out to render to a texture
(define framebuffer (malloc 'atomic _uint))
(define texture (malloc 'atomic _uint))


(define W 640)
(define H 480)
;; Set up the FBO with one texture attachment
(glBindFramebuffer GL_FRAMEBUFFER (ptr-ref framebuffer _uint))
(glGenTextures 1 texture)
(glBindTexture GL_TEXTURE_2D (ptr-ref texture _uint))
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
(glTexImage2D GL_TEXTURE_2D 0 GL_RGBA8 100 100 0 GL_RGBA GL_UNSIGNED_BYTE #f)
(glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D (ptr-ref texture _uint) 0)

(define status (glCheckFramebufferStatus GL_FRAMEBUFFER))
(unless (= status GL_FRAMEBUFFER_COMPLETE)
  (printf "status not fine!~n"))
(glBindFramebuffer GL_FRAMEBUFFER 0)

(change-viewport 0 0 W H fov (/ W H 1.0) 0.00001 1000.0)
(randomize-camera)
(my-gl-draw)
;(glFlush)

;; make the window the target

;;(my-gl-draw)


(define bm-cv (make-cvector _ubyte (* W H 4)))
(glReadPixels 0 0 W H GL_RGBA GL_UNSIGNED_BYTE bm-cv)
(define bm (list->bytes (cvector->list bm-cv)))
(define res (opengl->bitmap (rgba->argb bm W H) W H))
(define r (make-bitmap W H))
(send r set-argb-pixels 0 0 W H res)
(send r save-file "test.png" 'png)


 
;;; Get some information abotu my system
(define prevTextureBinding (malloc 'atomic _int))
(glGetIntegerv GL_RED_BITS prevTextureBinding)
(ptr-ref prevTextureBinding _uint)

(define result (malloc 'atomic _bool))
(define r (glGetBooleanv GL_STEREO result))



;;; Try again!
(define framebuffer (malloc 'atomic _uint 10))
(define renderbuffer (malloc 'atomic _uint 10))

(glGenFramebuffers 10 framebuffer)
(glGenRenderbuffers 10 renderbuffer)

(glBindFramebuffer GL_FRAMEBUFFER (ptr-ref framebuffer _uint 2))
(glBindRenderbuffer GL_RENDERBUFFER (ptr-ref renderbuffer _uint 2))
(glRenderbufferStorageMultisample GL_RENDERBUFFER (- GL_MAX_SAMPLES 1) GL_RGBA8 640 480)

;;(glRenderbufferStorage GL_RENDERBUFFER GL_RGBA 640 480)

(glFramebufferRenderbuffer GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_RENDERBUFFER (ptr-ref renderbuffer _uint 2))

(define status (glCheckFramebufferStatus GL_FRAMEBUFFER))
(printf "~a~n"status)

(glDeleteRenderbuffers 10 renderbuffer)
(glDeleteFramebuffers 10 framebuffer)
