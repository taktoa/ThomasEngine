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
       [char-callback (λ (c) (thread-send key-thread c))]
       [width 960]
       [height 540]))

; Show the canvas
(send test-frame show #t)

; Move the canvas in the requisite direction
(define (move dir delta canvas)
  (let ([pd delta] [nd (- 0 delta)])
    (match dir
      ['up    (dmv 0 nd canvas)]
      ['down  (dmv 0 pd canvas)]
      ['left  (dmv nd 0 canvas)]
      ['right (dmv pd 0 canvas)]
      [_ (void)])))

; Move canvas by (dx, dy)
(define (dmv dx dy canvas)
  (let ([cx (get-field position-x canvas)]
        [cy (get-field position-y canvas)])
    (send canvas set-position (+ dx cx) (+ dy cy))))

; Key capture thread
(define key-thread
  (thread
   (lambda ()
     (let loop ()
       (move (thread-receive) 20 test-ac)
       (loop)))))

; Refresh the screen at 60 frames per second
(thread
 (lambda ()
   (let loop ()
     (send test-frame refresh)
     (sleep 1/60)
     (loop))))