#lang racket
(require
  racket/draw
  "entity.rkt"
  2htdp/universe)

(define sprite-entity%
  (class entity%
    ;; Class fields
    (inherit
      update!
      queue-prop-change!
      prop-get
      prop-get-all)
    
    (init-field
     [sprite #f])
    
    (field
     [sprite-dc #f])
    
    ;; Private functions
    (define/private (dtr r) (* r (/ (* 2 pi) 360)))
    
    (define/private (render-sprite)
      (let ([s (prop-get 'scale)]
            [r (prop-get 'rotation)]
            [sw (send sprite get-width)]
            [sh (send sprite get-height)])
        (define (gen-blank-bm) (make-bitmap (* 2 s sw) (* 2 s sh) #t))
        (set! sprite-dc (new bitmap-dc% [bitmap (gen-blank-bm)]))
        (send sprite-dc set-origin (* s sw) (* s sh))
        (send sprite-dc set-rotation (dtr r))
        (send sprite-dc set-scale s s)
        (send sprite-dc draw-bitmap sprite (* -1/2 sw) (* -1/2 sh))
        (send sprite-dc set-rotation r)
        (send sprite-dc get-bitmap)))
    
    ;; Public functions
    (define/public (render)
      (update!)
      (values
       (render-sprite)
       (prop-get 'position-x)
       (prop-get 'position-y)))
    
    (define/public (set-position! x y)
      (queue-prop-change!
       (hash 'position-x x 'position-y y)))
    
    (define/public (set-rotation! r)
      (queue-prop-change!
       (hash 'rotation r)))
    
    (define/public (set-scale! s)
      (queue-prop-change!
       (hash 'scale s)))
    
    ;; Class initialization
    (super-new)))

(define (mutate-sprite-entity! se r s x y)
  (send se set-rotation! r)
  (send se set-scale! s)
  (send se set-position! x y))

(define make-sprite-entity
  (case-lambda
    [(bm) (new sprite-entity% [sprite bm])]
    [(bm r s x y)
     (define se (make-sprite-entity bm))
     (mutate-sprite-entity! se r s x y)
     se]))

(define grass-bm
  (read-bitmap "../res/grass.png" 'unknown))
(define bg-bm
  (read-bitmap "../res/texture.png" 'unknown))

(define my-entities
  (list (make-sprite-entity grass-bm 35 3 200 200)
        (make-sprite-entity grass-bm 100 1 100 200)
        (make-sprite-entity grass-bm 35 2 300 300)
        (make-sprite-entity grass-bm 45 4 200 400)
        (make-sprite-entity grass-bm 75 2 400 200)))

(define (render-big entities width height background-bm)
  (define dc (new bitmap-dc% [bitmap (make-object bitmap% width height)]))
  (send dc draw-bitmap background-bm 0 0)
  (define (draw-entity entity)
    (define-values (bm x y) (send entity render))
    (send dc draw-bitmap bm x y 'xor))
  (for-each draw-entity entities)
  (send dc get-bitmap))

(render-big my-entities 600 600 bg-bm)