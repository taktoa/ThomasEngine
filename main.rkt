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
  "event.rkt"
  racket/gui)

;; Program parameters
; Canvas width and height
(define canvas-width  960)
(define canvas-height 540)

; Velocity in pixels per movement thread update
(define vel 5)

; Texture file name
(define texture-path "bigtexture.png")

; Frame label
(define main-frame-label "Testing")

; Screen refresh rate in Hz
(define screen-refresh-rate 60)

; Movement refresh rate in Hz
(define move-refresh-rate 120)

;; Utility functions
; Read in texture file at path
(define (get-texture-dc path)
  (define texture-file (read-bitmap texture-path 'unknown))
  (define texture-w (send texture-file get-width))
  (define texture-h (send texture-file get-height))
  (define texture-bm (make-bitmap texture-w texture-h))
  (define texture-dc (new bitmap-dc% [bitmap texture-bm]))
  (send texture-dc draw-bitmap texture-file 0 0)
  texture-bm)

; Move canvas by (dx, dy)
(define (dmv dx dy canvas)
  (let ([cx (get-field position-x canvas)]
        [cy (get-field position-y canvas)])
    (send canvas set-position (+ dx cx) (+ dy cy))))

;; Instantiate relevant objects
; Define a new frame
(define main-frame
  (new frame%
       [label main-frame-label]
       [style '(no-resize-border)]))

; Define a new event handler
(define event-handler (make-object evt-handler%))
(define event-handler-thread (send event-handler get-key-thread))

; Define a new canvas
(define main-ac
  (new texture-canvas% 
       [parent main-frame]
       [texture (get-texture-dc texture-path)]
       [event-callback (λ (c) (thread-send event-handler-thread c))]
       [width canvas-width]
       [height canvas-height]))

;; Timers, callbacks, and threads
; Create a screen refresh timer
(define screen-refresh-timer
  (new timer%
       [notify-callback (λ () (send main-frame refresh))]
       [interval (round (/ 1000 screen-refresh-rate))]))

; Movement update callback
(define (move-callback)
  (define (bool->int b) (if b 1 0))
  (define (pressed? c) (send event-handler is-pressed? c))
  (define (pressedn x) (bool->int (pressed? x)))
  (define v-x (* vel (- (pressedn #\d) (pressedn #\a))))
  (define v-y (* vel (- (pressedn #\s) (pressedn #\w))))
  (dmv v-x v-y main-ac))

; Movement update timer
(define move-timer
  (new timer%
       [notify-callback move-callback]
       [interval (round (/ 1000 move-refresh-rate))]))

;; Initialization
; Show the canvas
(send main-frame show #t)