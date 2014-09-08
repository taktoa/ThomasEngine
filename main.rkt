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
  racket/set
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

; Move canvas by (dx, dy)
(define (dmv dx dy canvas)
  (let ([cx (get-field position-x canvas)]
        [cy (get-field position-y canvas)])
    (send canvas set-position (+ dx cx) (+ dy cy))))

; The set of all currently-pressed keys
(define key-set (mutable-set))

; Velocity in pixels per movement thread update
(define vel 5)

; Add or remove a key from the key-set
(define (set-key-set l)
  (match l
    [(cons x 'press) (set-add! key-set x)]
    [(cons x 'release) (set-remove! key-set x)]
    [_ (void)]))

; Key capture thread
(define key-thread
  (thread
   (lambda ()
     (let loop ()
       (set-key-set (thread-receive))
       (loop)))))

; Movement thread
(define move-thread
  (thread
   (lambda ()
     (let loop ()
       (define up?    (if (set-member? key-set #\w) 1 0))
       (define down?  (if (set-member? key-set #\s) 1 0))
       (define left?  (if (set-member? key-set #\a) 1 0))
       (define right? (if (set-member? key-set #\d) 1 0))
       (define v-x (* vel (- right? left?)))
       (define v-y (* vel (- down? up?)))
       (dmv v-x v-y test-ac)
       (sleep 1/120)
       (loop)))))

; Refresh the screen at 60 frames per second
(define refresh-thread
  (thread
   (lambda ()
     (let loop ()
       (send test-frame refresh)
       (sleep 1/60)
       (loop)))))
