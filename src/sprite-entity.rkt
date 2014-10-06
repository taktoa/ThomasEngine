; sprite-entity.rkt
; Copyright 2014 Remy E. Goldschmidt <taktoa@gmail.com>
; This file is part of ThomasEngine.
;    ThomasEngine is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    ThomasEngine is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with ThomasEngine. If not, see <http://www.gnu.org/licenses/>.

#lang racket
(require
  racket/draw
  "entity.rkt")

(provide
 (all-defined-out))

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
      (list (render-sprite)
            (prop-get 'position-x)
            (prop-get 'position-y)))
    
    (define/public (set-properties! props)
      (queue-prop-change! props))
    
    (define/public (set-property! prop-name prop-val)
      (set-properties! (hash prop-name prop-val)))
    
    (define/public (set-position! x y)
      (set-properties!
       (hash 'position-x x 'position-y y)))
    
    (define/public (set-rotation! r)
      (set-property! 'rotation r))
    
    (define/public (set-scale! s)
      (set-property! 'scale s))
    
    ;; Class initialization
    (super-new)))

(define sprite-entity-set%
  (class object%
    ;; Class fields
    (field
     [entity-hash (make-hash)])
    
    ;; Private functions
    (define/private (update-all-entities)
      (hash-for-each entity-hash (λ (k v) (send v update!))))
    
    (define/private (get-entities-within-area w h x y)
      (define (posn-within-area px py)
        (and (< (- px x) w) (< (- py y) h)))
      (define (entity-within-area ent)
        (posn-within-area
         (send ent prop-get 'position-x)
         (send ent prop-get 'position-y)))
      (define result (make-hash))
      (hash-for-each
       (get-entities)
       (λ (k v) (if (entity-within-area v) (hash-set! result k v) (void))))
      result)
    
    ;; Public functions
    (define/public (add-sprite-entity! name se)
      (hash-set! entity-hash name se))
    
    (define/public (rem-sprite-entity! name)
      (hash-remove! entity-hash name))
    
    (define/public (set-entity-properties! name props)
      (hash-update! entity-hash name
                    (λ (se)
                      (send se queue-prop-change! props)
                      (send se update!)
                      se)))
    
    (define/public (set-entity-property! name prop-name prop-val)
      (set-entity-properties! name (hash prop-name prop-val)))
    
    (define/public (set-entity-position! name x y)
      (set-entity-properties! name (hash 'position-x x 'position-y y)))
    
    (define/public (set-entity-rotation! name r)
      (set-entity-property! name 'rotation r))
    
    (define/public (set-entity-scale! name s)
      (set-entity-property! name 'scale s))
    
    (define/public (get-entity-property name prop-name)
      (send (hash-ref entity-hash name) prop-get prop-name))
    
    (define/public (get-entity-position name)
      (values
       (get-entity-property name 'position-x)
       (get-entity-property name 'position-y)))
    
    (define/public (get-entity-rotation name)
      (get-entity-property name 'rotation))
    
    (define/public (get-entity-scale name)
      (get-entity-property name 'scale))
    
    (define/public (get-entities)
      (update-all-entities)
      (hash-copy entity-hash))
    
    (define/public (render width height x y)
      (define to-draw (get-entities-within-area width height x y))
      (define sprites (hash-map to-draw (λ (k v) (send v render))))
      (define dc (new bitmap-dc% [bitmap (make-bitmap width height #t)]))
      (for-each
       (match-lambda
         [(list rr px py) (send dc draw-bitmap rr (- px x) (- py y) 'xor)])
       sprites)
      (send dc get-bitmap))
    
    ;; Class initialization
    (super-new)))

;(define (mutate-sprite-entity! se r s x y)
;  (send se set-rotation! r)
;  (send se set-scale! s)
;  (send se set-position! x y))
;
;(define make-sprite-entity
;  (case-lambda
;    [(bm) (new sprite-entity% [sprite bm])]
;    [(bm r s x y)
;     (define se (make-sprite-entity bm))
;     (mutate-sprite-entity! se r s x y)
;     se]))
;
;(define grass-bm
;  (read-bitmap "../res/grass.png" 'unknown))
;(define bg-bm
;  (read-bitmap "../res/texture.png" 'unknown))
;
;(define my-entities
;  (list (cons 'a (make-sprite-entity grass-bm 35 3 200 200))
;        (cons 'b (make-sprite-entity grass-bm 100 1 100 200))
;        (cons 'c (make-sprite-entity grass-bm 35 2 300 300))
;        (cons 'd (make-sprite-entity grass-bm 45 4 200 400))
;        (cons 'e (make-sprite-entity grass-bm 75 2 400 200))))
;
;(define (gen-entity-set ents)
;  (define entity-set (new sprite-entity-set%))
;  (for-each
;   (match-lambda
;     [(cons name se) (send entity-set add-sprite-entity! name se)]
;     [_ (error "you screwed up somehow")])
;   ents)
;  entity-set)
;
;(define my-entity-set (gen-entity-set my-entities))
;
;(send my-entity-set render 500 500 50 50)
