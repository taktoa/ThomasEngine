#lang racket
(require 
  "matrix.rkt"
  racket/draw
  racket/gui)

(provide
 (all-defined-out))

(struct tileset (filename tile-width tile-height width height))

(define ascii-canvas%
  (class canvas%
    (inherit
      get-width
      get-height
      refresh)
    
    (init-field
     parent
     [tiles (tileset "tiles.png" 9 16 16 16)]
     [canvas-width 80]
     [canvas-height 24])
    
    (field
     [offscreen-buffer #f] 
     [offscreen-buffer-dc #f]
     [cursor-x 0]
     [cursor-y 0]
     [glyphs #f]
     [tile-mat #f]
     [old-tile-mat #f])
    
    (define tile-width (tileset-tile-width tiles))
    (define tile-height (tileset-tile-height tiles))
    
    ; Repaint the canvas
    (define/private (paint self dc)
      (unless offscreen-buffer (init-buffer))
      
      (for* ([x (in-range canvas-width)]
             [y (in-range canvas-height)]
             #:when (not (eq? (matrix-ref tile-mat x y) (matrix-ref old-tile-mat x y))))
        (draw-tile x y))
      
      (send dc draw-bitmap offscreen-buffer 0 0))
    
    (define/private (init-buffer)
      (set! offscreen-buffer (make-screen-bitmap (get-width) (get-height)))
      (set! offscreen-buffer-dc (new bitmap-dc% [bitmap offscreen-buffer])))
    
    (define/private (draw-tile x y)
      (define src-c (matrix-ref tile-mat x y))
      (define src-x (remainder src-c (tileset-width tiles)))
      (define src-y (quotient src-c (tileset-height tiles)))
      (send offscreen-buffer-dc draw-bitmap-section
            glyphs
            (* x tile-width)
            (* y tile-height)
            (* src-x tile-width)
            (* src-y tile-height)
            tile-width
            tile-height
            'xor ; solid, opaque, or xor
            (make-object color% "black")
            glyphs)
      (matrix-set! old-tile-mat x y (matrix-ref tile-mat x y)))
    
    (define/public clear
      (case-lambda
        [(tile) (send this clear tile 0 0 canvas-width canvas-height)]
        [(tile x y width height)
         (unless (<= 0 x (- canvas-width 1))
           (raise-argument-error 'clear (format "x in the range [0, ~a)" canvas-width) x))
         (unless (<= 0 y (- canvas-height 1))
           (raise-argument-error 'clear (format "y in the range [0, ~a)" canvas-height) y))
         (unless (<= 0 width)
           (raise-argument-error 'clear "positive integer? for width" width))
         (unless (<= 0 height)
           (raise-argument-error 'clear "positive integer? for height" height))
         (unless (<= (+ x width) canvas-width)
           (raise-argument-error 'clear (format "x+width in range [0, ~a)" canvas-width) (+ x width)))
         (unless (<= (+ y height) canvas-height)
           (raise-argument-error 'clear (format "y+height in range [0, ~a)" canvas-height) (+ y height)))
         (clear-core tile x y width height)]))
    
    (define/private (clear-core tile x y width height)
      (for* ([xi (in-range x (+ x width))]
             [yi (in-range y (+ y height))])
        (send this write tile xi yi)))

    ; Draw a single tile
    (define/public (write tile x y)
      (unless (<= 0 x (- canvas-width 1))
        (raise-argument-error 'write (format "x in the range [0, ~a)" canvas-width) x))
      (unless (<= 0 y (- canvas-height 1))
        (raise-argument-error 'write (format "y in the range [0, ~a)" canvas-height) y))
      (write-core tile x y))
    
    (define/private (write-core tile x y)
      (matrix-set! tile-mat x y tile)
      (set! cursor-x (+ x 1))
      (set! cursor-y (+ y 1)))
    
    ; Validate that the width and height make sense
    (unless (positive? canvas-width) (raise-argument-error 'ascii-canvas% "positive integer" canvas-width))
    (unless (positive? canvas-height) (raise-argument-error 'ascii-canvas% "positive integer" canvas-height))
    
    ; Set up the tile buffers
    (set! tile-mat (make-matrix canvas-width canvas-height (λ (x y) 0)))
    (set! old-tile-mat (make-matrix canvas-width canvas-height (λ (x y) 0)))
    
    ; Load the glyphs
    (define glyph-file (read-bitmap (tileset-filename tiles) 'unknown/alpha (make-object color% "magenta")))
    (set! glyphs (make-monochrome-bitmap (send glyph-file get-width) (send glyph-file get-height)))
    (define glyphs-dc (new bitmap-dc% [bitmap glyphs]))
    (send glyphs-dc draw-bitmap glyph-file 0 0)
 
    (set! tile-width (quotient (send glyphs get-width) (tileset-width tiles)))
    (set! tile-height (quotient (send glyphs get-height) (tileset-width tiles)))
    
    ; Create the canvas
    (super-new 
      [parent parent]
      [paint-callback (λ (c dc) (paint c dc))]
      [min-width (* canvas-width tile-width)]
      [min-height (* canvas-height tile-height)])
    
    ; Do an initial clear
    (send this clear 32)
    
    (send this focus)))

