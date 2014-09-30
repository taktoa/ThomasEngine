#lang racket
(require racket/draw
         2htdp/universe)
(define sprite (read-bitmap "../res/grass.png" 'unknown))
(define sprite-w (send sprite get-width))
(define sprite-h (send sprite get-height))

(define (dtr r) (* r (/ (* 2 pi) 360)))

(define scale 2)
(define (rotsprite) (make-object bitmap% (* scale 2 sprite-w) (* scale 2 sprite-h) #f #t))
(define rotsprite-dc (send (rotsprite) make-dc))

(define (rotsprite-rotate n)
  (send rotsprite-dc set-origin (* scale sprite-w) (* scale sprite-h))
  (send rotsprite-dc set-rotation (dtr (* 20 n)))
  (send rotsprite-dc set-scale scale scale)
  (send rotsprite-dc draw-bitmap sprite (* sprite-w -1/2) (* sprite-h -1/2))
  (define result (send rotsprite-dc get-bitmap))
  (send rotsprite-dc set-bitmap (rotsprite))
  result)

(animate rotsprite-rotate)


(define sprite%
  (class object%
    ;; Class fields
    (init-field
     [sprite #f])
    
    (field
     [scale 1]
     [rotation 0]
     [sprite-w 0]
     [sprite-h 0])
    
    ;; Private functions
    (define/private (gen-outline-bitmap)
      (make-object bitmap%
        (* 2 scale (send sprite get-width))
        (* 2 scale (send sprite get-height))))
    
    ;; Public functions
    ; Set the scale
    (define/public (set-scale! s) (set! scale s))
    
    ; Set the rotation
    (define/public (set-rotation! r) (set! rotation r))
    
    ; Render the sprite
    (define/public (render)
      (define outline-dc (new bitmap-dc% [bitmap (gen-outline-bitmap)]))
      (send outline-dc set-origin
            (* scale sprite-w)
            (* scale sprite-h))
      (send outline-dc set-rotation (dtr rotation))
      (send outline-dc set-scale scale scale)
      (send outline-dc draw-bitmap sprite (* -1/2 sprite-w) (* -1/2 sprite-h))
      (send outline-dc get-bitmap))
    
    ;; Class initialization
    (set! sprite-w (send sprite get-width))
    (set! sprite-h (send sprite get-height))
    
    (super-new)))
