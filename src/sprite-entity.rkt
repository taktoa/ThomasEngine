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