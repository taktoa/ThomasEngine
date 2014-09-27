#lang racket
(require racket/draw
         2htdp/universe)
(define sprite (read-bitmap "../res/grass.png" 'unknown))
(define sprite-w (send sprite get-width))
(define sprite-h (send sprite get-height))

(define (dtr r) (* r (/ (* 2 pi) 360)))

(define (rotsprite-rotate n)
  (define scale 2)
  (define rotsprite (make-object bitmap% (* scale 2 sprite-w) (* scale 2 sprite-h) #f #t))
  (define rotsprite-dc (new bitmap-dc% [bitmap rotsprite]))
  (send rotsprite-dc set-origin (* scale sprite-w) (* scale sprite-h))
  (send rotsprite-dc set-rotation (dtr (* 20 n)))
  (send rotsprite-dc set-scale scale scale)
  (send rotsprite-dc draw-bitmap sprite (* sprite-w -1/2) (* sprite-h -1/2))
  (send rotsprite-dc get-bitmap))

(animate rotsprite-rotate)
