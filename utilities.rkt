(module utilities racket/gui
  (require ffi/unsafe
           ffi/unsafe/define
           sgl/gl
           sgl/gl-vectors)
  (provide (all-defined-out))
  
  (define (array #:type type . elements)
    (define size (length elements))
    (define result
      (ptr-ref (malloc type size 'atomic)
               (_array type size)))
    (for ([i (in-range 0 size)]
          [val (in-list elements)])
      (array-set! result i val))
    result)

  (define (make-array #:type type size (init #f))
    (define result
      (ptr-ref (malloc type size 'atomic)
               (_array type size)))
    (when init
      (for ([i (in-range 0 size)])
        (array-set! result i init)))
    result)


  ;; rgba -> argb format conversion
  (define (rgba->argb rgba W H)
    (define argb (make-bytes (* W H 4)))
    (for ([i (in-range 0 (bytes-length rgba) 4)])
      (bytes-set! argb i (bytes-ref rgba (+ i 3)))
      (bytes-set! argb (+ i 1) (bytes-ref rgba i))
      (bytes-set! argb (+ i 2) (bytes-ref rgba (+ i 1)))
      (bytes-set! argb (+ i 3) (bytes-ref rgba (+ i 2))))
    argb)

  (define (opengl->bitmap bm-bytes W H)
    (define result (make-bytes (* W H 4)))
    (for ([line (in-range 0 (* W H 4) (* W 4))])
      (bytes-copy! result (- (* W H 4) (* W 4) line)
                   bm-bytes line (+ line (* W 4))))
    result)


  (define (bitmap->gl-vector bmp)
    (let* ([dc (send bmp make-dc)]
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

  (define (gl-load-texture textures image-vector width height min-filter mag-filter ix)
    (glBindTexture GL_TEXTURE_2D (gl-vector-ref textures ix))
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

  )
