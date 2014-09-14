; collisions.rkt
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
  "utility.rkt"
  racket/gui)

(provide
 (all-defined-out))

(define collision-mask%
  (class object%
    
    ; Class field
    (init-field
     bitmap)
    
    ;; Public  function
    ; Collision detection. Returns true iff something is colliding with something else.
    (define/public (colliding? x y)
      (black? x y))

    ; Read in collision diskmap at path
    (define bitmap-dc
      (new bitmap-dc% [bitmap bitmap]))
    
    ; Check if color of a pixel at position x y is black
    (define/private (black? x y)
      (define black (make-object color% "black"))
      (define color-gotten (make-object color% "white"))
      (send bitmap-dc get-pixel x y color-gotten)
      (define (color-numbers color) (list (send color red) (send color green) (send color blue)))
      (equal? (color-numbers color-gotten) (color-numbers black)))
    
    (super-new)))