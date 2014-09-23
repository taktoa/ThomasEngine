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
(require "texture.rkt"
         "event.rkt"
         "utility.rkt"
         "propertylayer.rkt"
         racket/gui)

;; Program parameters
; Canvas width and height
(define canvas-width  500)
(define canvas-height 500)

; Center pixel location
(define (center-pixel-x canvas) (+ (get-field position-x canvas) (/ canvas-width 2)))
(define (center-pixel-y canvas) (+ (get-field position-y canvas) (/ canvas-height 2)))

; Velocity in pixels per movement thread update
(define vel 5)

; Texture file name
(define texture-path "test-texture.png")

; Property layer file name
(define property-layer-path "test.png")

; Frame label
(define main-frame-label "Testing")

; Screen refresh rate in Hz
(define screen-refresh-rate 60)

; Movement refresh rate in Hz
(define move-refresh-rate 120)

; Hash table for property layer functions
(define property-hash (hash "black" 'collision "white" 'nothing))

;; Utility functions
; Read in texture file at path
(define (get-texture path)
  (read-bitmap path 'unknown))

; Move canvas by (dx, dy)
(define (dmv dx dy canvas)
  (let ([cx (get-field position-x canvas)]
        [cy (get-field position-y canvas)])
    (unless (equal? (send  property-layer property-at-pos (+ (center-pixel-x canvas) dx) (+ (center-pixel-y canvas) dy)) 'collision)
        (send canvas set-position (+ dx cx) (+ dy cy)))))


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
       [texture (get-texture texture-path)]
       [event-callback (λ (c) (thread-send event-handler-thread c))]
       [width canvas-width]
       [height canvas-height]))

; Define a collsion detection mask
(define property-layer
  (new property-layer%
       [bitmap (get-texture property-layer-path)]
       [hash-table property-hash]))

;; Timers, callbacks, and threads
; Screen refresh callback
(define (screen-refresh-callback)
  (send main-frame refresh))

; Screen refresh timer
(define screen-refresh-timer
  (create-timer screen-refresh-callback screen-refresh-rate))

; Movement update callback
(define (move-callback)
  (define (pressedn c) (if (send event-handler is-pressed? c) 1 0))
  (define v-x (* vel (- (pressedn #\d) (pressedn #\a))))
  (define v-y (* vel (- (pressedn #\s) (pressedn #\w))))
  (dmv v-x v-y main-ac))

; Movement update timer
(define move-timer
  (create-timer move-callback move-refresh-rate))

;; Initialization
; Show the canvas
(send main-frame show #t)
