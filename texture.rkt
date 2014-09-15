; texture.rkt
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

(define texture-canvas%
  (class canvas%
    ;; Class fields
    (inherit
      get-width
      get-height
      refresh)
    
    (init-field
     parent
     texture
     width
     height
     [event-callback (λ (c) #f)])
    
    (field
     [position-x 0]
     [position-y 0])
    
    ;; Local utility variables
    ; Get texture width and height
    (define texture-width  (send texture get-width))
    (define texture-height (send texture get-height))
    
    ;; Private functions
    ; Draw the texture at a position
    (define/private (draw-texture x y dc)
      (send dc draw-bitmap-section texture 0 0 position-x position-y (get-width) (get-height)))
   
    ;; Public getters and setters
    ; Utility functions for allowable position bounds
    (define/public (min-x) 0)
    (define/public (min-y) 0)
    (define/public (max-x) (- texture-width width))
    (define/public (max-y) (- texture-height height))
         
    ; Set the screen position if it has changed, bracked by position bounds
    (define/public (set-position x y)
      (define adj-x (bound x (min-x) (max-x)))
      (define adj-y (bound y (min-y) (max-y)))
      (unless (and (= position-x adj-x) (= position-y adj-y))
        (set! position-x adj-x)
        (set! position-y adj-y)))

    ;; Superclass overrides
    ; Override on-char and on-event with the event callback
    (define/override (on-char key-event) (event-callback key-event))
    (define/override (on-event key-event) (event-callback key-event))
    
    ; Override screen-painting function
    (define/override (on-paint)
      (let ([dc (send this get-dc)])
        (send dc suspend-flush)
        (draw-texture position-x position-y dc)
        (send dc resume-flush)))

    ;; Class initialization
    ; Set paint callback, minimum width, and minimum height
    (super-new 
     [parent parent]
     [min-width width]
     [min-height height])
    
    ; Focus the canvas
    (send this focus)))

