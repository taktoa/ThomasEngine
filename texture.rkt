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
     [texture #f]
     [width 960]
     [height 540]
     [char-callback #f])
    
    (field
     [position-x 0]
     [position-y 0])
    
    ;(define-values (texture-width texture-height) (send texture get-size))
    (define texture-width  (send texture get-width))
    (define texture-height (send texture get-height))
    
    ; Screen-painting callback function
    (define/private (paint self dc)
      (send dc suspend-flush)
      (draw-texture position-x position-y dc)
      (send dc resume-flush))
    
    ; Utility functions for allowable position bounds
    (define/public (min-x) 0)
    (define/public (min-y) 0)
    (define/public (max-x) (- texture-width width))
    (define/public (max-y) (- texture-height height))
    
    ; Function that brackets x within [a, b]
    (define/private (bound x a b)
      (cond [(> x b) b]
            [(< x a) a]
            [true    x]))
    
    ; Draw the screen at a position, bracketed by texture size bounds
    (define/private (draw-texture x y dc)
      (define adj-x (bound x (min-x) (max-x)))
      (define adj-y (bound y (min-y) (max-y)))
      (send dc draw-bitmap-section texture 0 0 adj-x adj-y (get-width) (get-height)))
    
    ; Set the screen position if it has changed
    (define/public (set-position x y)
      (unless (and (= position-x x) (= position-y y))
        (set! position-x x)
        (set! position-y y)))
    
    ; Unwrap the key-codes from the key-event and pass them to key-translate
    (define/private (key-translate-event e)
      (key-translate (send e get-key-code) (send e get-key-release-code)))
    
    ; Translate raw key-events to more useable pairs
    (define/private (key-translate x y)
      (cond
        [(eq? x 'release) (cons y 'release)]
        [(eq? y 'press)   (cons x 'press)]
        [true             (void)]))
    
    ; Override on-char with the char callback, fed by key-translate-event
    (define/override (on-char key-event)
      (char-callback (key-translate-event key-event)))
    
    ; Set paint callback, minimum width, and minimum height
    (super-new 
     [parent parent]
     [paint-callback (λ (c dc) (paint c dc))]
     [min-width width]
     [min-height height])
    
    ; Focus the canvas
    (send this focus)))

