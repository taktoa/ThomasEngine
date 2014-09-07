; main.rkt
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
  "texture.rkt"
  racket/gui
  racket/runtime-path)

; Path for finding the texture file
(define-runtime-path RUNTIME_DIR ".")

; Define a new frame
(define test-frame
  (new frame%
       [label "Testing"]
       [style '(no-resize-border)]))

; Define a new canvas
(define test-ac
  (new texture-canvas% 
       [parent test-frame]
       [texture-path (build-path RUNTIME_DIR "bigtexture.png")]
       [width 960]
       [height 540]))

; Show the canvas
(send test-frame show #t)

; Move the view area in a circle, continuously, forever
(thread
 (lambda ()
   (let loop ([t 0])
     (define x (round (* (send test-ac max-x) 1/2 (+ 1 (cos (/ t 360))))))
     (define y (round (* (send test-ac max-y) 1/2 (+ 1 (sin (/ t 360))))))
     (send test-ac set-position x y)
     (sleep 1/120)
     (loop (+ t 2)))))

; Refresh the screen at 60 frames per second
(thread
 (lambda ()
   (let loop ()
     (send test-frame refresh)
     (sleep 1/60)
     (loop))))