; entity.rkt
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

(require data/queue)

(provide
 (all-defined-out))

(define entity%
  (class object%
    ;; Class fields
    (field
     [update-queue (make-queue)]
     [properties (make-hash)])
    
    ;; Private functions
    ; Merge changes into the properties
    (define/private (prop-merge! changes)
      (hash-for-each
       changes
       (λ (k v)
         (if (eq? v 'delete)
             (hash-remove! properties k)
             (hash-set! properties k v)))))
    
    ;; Public functions
    ; Commit changes in the update queue
    (define/public (update!)
      (for ([u (queue->list update-queue)])
        (prop-merge! u))
      (set! update-queue (make-queue)))
    
    ; Queue up a property change
    (define/public (queue-prop-change! c)
      (enqueue! update-queue c))
    
    ; Get a specific property of the entity
    (define/public (prop-get k) (hash-ref properties k))    
    
    ; Get all properties of this entity
    (define/public (prop-get-all) properties)
    
    ;; Class initialization
    (super-new)))

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
    (define/private (render-sprite)
      (let ([s (prop-get 'scale)]
            [r (prop-get 'rotation)])
        (send sprite-dc clear)
        (send sprite-dc draw-bitmap sprite 0 0)
        (send sprite-dc scale s s)
        (send sprite-dc set-rotation r)))
    
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
