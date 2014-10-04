#lang racket
(require racket/draw
         2htdp/universe)

(define sprite%
  (class object%
    ;; Class fields
    (init-field
     [sprite #f])
    
    (field
     [outline-dc #f]
     [scale 1]
     [rotation 0]
     [sprite-w 0]
     [sprite-h 0])
    
    ;; Private functions
    (define/private (gen-outline-bitmap)
      (make-object bitmap%
        (* 2 scale sprite-w)
        (* 2 scale sprite-h)))
    
    (define/private (set-sprite-dims)
      (set! sprite-w (send sprite get-width))
      (set! sprite-h (send sprite get-height)))
    
    (define/private (dtr r) (* r (/ (* 2 pi) 360)))
    
    ;; Public functions
    ; Set the scale
    (define/public (set-scale! s) (set! scale s))
    
    ; Set the rotation
    (define/public (set-rotation! r) (set! rotation r))
    
    ; Render the sprite
    (define/public (render)
      (set-sprite-dims)
      (set! outline-dc (new bitmap-dc% [bitmap (gen-outline-bitmap)]))
      (send outline-dc set-origin
            (* scale sprite-w)
            (* scale sprite-h))
      (send outline-dc set-rotation (dtr rotation))
      (send outline-dc set-scale scale scale)
      (send outline-dc draw-bitmap ((λ () sprite)) (* -1/2 sprite-w) (* -1/2 sprite-h))
      (send outline-dc get-bitmap))
    
    ;; Class initialization
    (super-new)))

(define test-sprite
  (new sprite%
       [sprite (read-bitmap "../res/grass.png" 'unknown)]))

;(send test-sprite set-scale! 4)
;
;(define (animate-example r)
;  (send test-sprite set-rotation! r)
;  (send test-sprite render))
;
;(animate animate-example)

(send test-sprite set-rotation! 35)

(define big-texture (read-bitmap "../res/texture.png" 'unknown))

(define test-w 500)
(define test-h 500)

(define test-dc
  (new bitmap-dc% [bitmap (make-object bitmap% test-w test-h)]))

(send test-dc draw-bitmap big-texture 0 0)

(send test-dc draw-bitmap (send test-sprite render) 200 200 'xor)

(send test-dc get-bitmap)