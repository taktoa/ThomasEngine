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
     [chars #f]
     [old-chars #f])
    
    (define char-width (tileset-tile-width tiles))
    (define char-height (tileset-tile-height tiles))
    
    ; Repaint the canvas
    (define/private (my-paint-callback self dc)
      (unless offscreen-buffer (init-buffer))
      
      (for* ([x (in-range canvas-width)]
             [y (in-range canvas-height)]
             #:when (not (eq? (matrix-ref chars x y) (matrix-ref old-chars x y))))
        (draw-character x y))
      
      (send dc draw-bitmap offscreen-buffer 0 0))
    
    (define/private (init-buffer)
      (set! offscreen-buffer (make-screen-bitmap (get-width) (get-height)))
      (set! offscreen-buffer-dc (new bitmap-dc% [bitmap offscreen-buffer])))
    
    (define/private (draw-character x y)
      (define src-c (char->integer (matrix-ref chars x y)))
      (define src-x (remainder src-c (tileset-width tiles)))
      (define src-y (quotient src-c (tileset-height tiles)))
      ; Draw the glyph to the buffer
      (send offscreen-buffer-dc draw-bitmap-section
            glyphs
            (* x char-width)
            (* y char-height)
            (* src-x char-width)
            (* src-y char-height)
            char-width
            char-height
            'xor ; solid, opaque, or xor
            (make-object color% "black")
            glyphs)
      (send offscreen-buffer-dc draw-ellipse (* 0.5 char-width canvas-width) (* 0.5 char-height canvas-height) 5 5)
      ; Update maps
      (matrix-set! old-chars x y (matrix-ref chars x y)))
    
    ; Clear the screen
    (define/public clear
      (case-lambda
        [() (send this clear #\space 0 0 canvas-width canvas-height)]
        [(char x y width height) (clear-validated char x y width height)]))
    
    (define/private (clear-validated char x y width height)
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
      (clear-full char x y width height))
    
    (define/private (clear-full char x y width height)
      (for* ([xi (in-range x (+ x width))]
             [yi (in-range y (+ y height))])
        (send this write char xi yi)))

    ; Write a single character
    (define/public (write char x y)
      (write-validate char x y))
    
    (define/private (write-validate char x y)
      (unless (<= 0 x (- canvas-width 1))
        (raise-argument-error 'write (format "x in the range [0, ~a)" canvas-width) x))
      (unless (<= 0 y (- canvas-height 1))
        (raise-argument-error 'write (format "y in the range [0, ~a)" canvas-height) y))
      (write-full char x y))
    
    (define/private (write-full char x y)
      (matrix-set! chars x y char)
      (set! cursor-x (+ x 1))
      (set! cursor-y (+ y 1)))
    
    ; Validate that the width and height make sense
    (unless (positive? canvas-width) (raise-argument-error 'ascii-canvas% "positive integer" canvas-width))
    (unless (positive? canvas-height) (raise-argument-error 'ascii-canvas% "positive integer" canvas-height))
    
    ; Set up the char arrays
    (set! chars (make-matrix canvas-width canvas-height (λ (x y) #\nul)))
    (set! old-chars (make-matrix canvas-width canvas-height (λ (x y) #\nul)))
    
    ; Load the glyphs
    (define glyph-file (read-bitmap (tileset-filename tiles) 'unknown/alpha (make-object color% "magenta")))
    (set! glyphs (make-monochrome-bitmap (send glyph-file get-width) (send glyph-file get-height)))
    (define glyphs-dc (new bitmap-dc% [bitmap glyphs]))
    (send glyphs-dc draw-bitmap glyph-file 0 0)
    
    (set! char-width (quotient (send glyphs get-width) 16))
    (set! char-height (quotient (send glyphs get-height) 16))
    
    ; Create the canvas
    (super-new 
      [parent parent]
      [paint-callback (λ (c dc) (my-paint-callback c dc))]
      [min-width (* canvas-width char-width)]
      [min-height (* canvas-height char-height)])
    
    ; Do an initial clear
    (send this clear)
    
    (send this focus)))

