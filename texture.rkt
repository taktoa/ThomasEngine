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
      [offscreen-buffer #f] 
      [offscreen-buffer-dc #f]
      [position-x 0]
      [position-y 0])
    
    ; Repaint the canvas
    (define/private (paint self dc)
      (unless offscreen-buffer (init-buffer))
      (draw-texture position-x position-y)
      (send dc draw-bitmap offscreen-buffer 0 0))
    
    (define/private (init-buffer)
      (set! offscreen-buffer (make-screen-bitmap (get-width) (get-height)))
      (set! offscreen-buffer-dc (new bitmap-dc% [bitmap offscreen-buffer])))
    
    (define/private (draw-texture x y)
      (send offscreen-buffer-dc draw-bitmap-section texture 0 0 x y (get-width) (get-height)))
    
    (define/public (set-position x y)
      (set! position-x x)
      (set! position-y y))

    ; Load the texture
    (define texture-file (read-bitmap texture-path 'unknown))
    (set! texture (make-bitmap (send texture-file get-width) (send texture-file get-height)))
    (define texture-dc (new bitmap-dc% [bitmap texture]))
    (send texture-dc draw-bitmap texture-file 0 0)
    
    ; Create the canvas
    (super-new 
      [parent parent]
      [paint-callback (λ (c dc) (paint c dc))]
      [min-width width]
      [min-height height])
    
    (send this focus)))

