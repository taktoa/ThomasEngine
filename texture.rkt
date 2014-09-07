#lang racket
(require racket/gui)

(provide
 (all-defined-out))

(define texture-canvas%
  (class canvas%
    (inherit
      get-width
      get-height
      refresh)
    
    (init-field
      parent
      [texture-path "bigtexture.png"]
      [width 960]
      [height 540])
    
    (field
      [texture #f]
      [texture-width 0]
      [texture-height 0]
      [offscreen-buffer #f] 
      [offscreen-buffer-dc #f]
      [position-x 0]
      [position-y 0])
    
    ; Screen-painting callback function
    (define/private (paint self dc)
      (unless offscreen-buffer (init-buffer))
      (draw-texture position-x position-y)
      (send dc draw-bitmap offscreen-buffer 0 0))
    
    ; Initialize offscreen buffer and drawing context
    (define/private (init-buffer)
      (set! offscreen-buffer (make-screen-bitmap (get-width) (get-height)))
      (set! offscreen-buffer-dc (new bitmap-dc% [bitmap offscreen-buffer])))
    
    ; Utility functions for allowable position bounds
    (define/public (min-x) 0)
    (define/public (min-y) 0)
    (define/public (max-x) (- texture-width width))
    (define/public (max-y) (- texture-height height))
    
    ; Function that brackets x within [a, b]
    (define/private (bound x a b)
      (cond [(> x b) b]
            [(< x a) a]
            [true x]))
    
    ; Draw the screen at a position, bracketed by texture size bounds
    (define/private (draw-texture x y)
      (define adj-x (bound x (min-x) (max-x)))
      (define adj-y (bound y (min-y) (max-y)))
      (send offscreen-buffer-dc draw-bitmap-section texture 0 0 adj-x adj-y (get-width) (get-height)))

    ; Set the screen position if it has changed
    (define/public (set-position x y)
      (unless (and (= position-x x) (= position-y y))
        (set! position-x x)
        (set! position-y y)))

    ; Read texture file in, set width and height variables, and import the bitmap to a drawing context
    (define texture-file (read-bitmap texture-path 'unknown))
    (set! texture-width (send texture-file get-width))
    (set! texture-height (send texture-file get-height))
    (set! texture (make-bitmap texture-width texture-height))
    (define texture-dc (new bitmap-dc% [bitmap texture]))
    (send texture-dc draw-bitmap texture-file 0 0)
    
    ; Set pain callback, minimum width, and minimum height
    (super-new 
      [parent parent]
      [paint-callback (λ (c dc) (paint c dc))]
      [min-width width]
      [min-height height])
    
    ; Focus the canvas
    (send this focus)))

