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
  "utility.rkt"
  "propertylayer.rkt"
  "sprite-entity.rkt"
  "entity-set.rkt"
  racket/gui
  profile
  errortrace)

(profiling-enabled #t)

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
(define texture-path "../res/texture.png")

; Property layer file name
(define property-layer-path "../res/properties.png")

; Frame label
(define main-frame-label "Testing")

; Define refresh rates in Hz
(define misc-key-refresh-rate 10)
(define screen-refresh-rate 60)
(define move-refresh-rate 120)

; Hash table for property layer functions
(define property-hash
  (hash "black" 'collision
        "white" 'nothing))

; Up, down, left, and right keys
(define up-key    #\w)
(define down-key  #\s)
(define left-key  #\a)
(define right-key #\d)
(define misc-key-hash
  (hash #\q exit))

;; Utility functions
; Read in texture file at path
(define (get-texture path)
  (read-bitmap path 'unknown))

(define grass-bm (get-texture "../res/grass.png"))

(define main-character-entity (new sprite-entity% [sprite grass-bm]))
(send main-character-entity set-rotation! 35)
(send main-character-entity set-scale! 3)
(send main-character-entity set-position! 100 100)

(define main-entity-set (new sprite-entity-set%))
(send main-entity-set add-sprite-entity! 'a main-character-entity)

(define (test-renderer w h x y)
  (send main-entity-set render w h x y))

; Move canvas by (dx, dy)
(define (dmv dx dy canvas)
  (let-values ([(cx cy) (send main-entity-set get-entity-position 'a)])
    (unless 
        (equal?
         (send property-layer property-at-pos (+ dx cx) (+ dy cy))
         'collision)
      (send main-entity-set set-entity-position! 'a (+ dx cx) (+ dy cy)))))

;; Instantiate relevant objects
; Define a new frame
(define main-frame
  (new frame%
       [label main-frame-label]
       [style '(no-resize-border)]
       [stretchable-width #f]
       [stretchable-height #f]))

; Define a new event handler
(define event-handler (make-object evt-handler%))
(define event-handler-thread (send event-handler get-key-thread))

; Define a new canvas
(define main-ac
  (new texture-canvas% 
       [parent main-frame]
       [texture (get-texture texture-path)]
       [event-callback (λ (c) (thread-send event-handler-thread c))]
       [render-callback test-renderer]
       [width canvas-width]
       [height canvas-height]))

; Define a property layer
(define property-layer
  (new property-layer%
       [bitmap (get-texture property-layer-path)]
       [hash-table property-hash]))

;; Timers, callbacks, and threads
; Screen refresh callback
(define (screen-refresh-callback)
  (define w (send main-ac get-width))
  (define h (send main-ac get-height))
  (let-values ([(x y) (send main-entity-set get-entity-position 'a)])
    (send main-ac set-position! (- x (/ w 2)) (- y (/ h 2))))
  (send main-frame refresh))

; Screen refresh timer
(define screen-refresh-timer
  (create-timer screen-refresh-callback screen-refresh-rate))

; Miscellaneous key callback
(define (misc-key-callback)
  (define (when-pressed? c a) (when (send event-handler is-pressed? c) (a)))
  (hash-for-each misc-key-hash (λ (k v) (when-pressed? k v))))

; Miscellaneous key timer
(define misc-key-timer
  (create-timer misc-key-callback misc-key-refresh-rate))

; Check if the requisite keys are being pressed
(define (move-key-checker)
  (define (pressed? c) (send event-handler is-pressed? c))
  (values (pressed? up-key)
          (pressed? down-key)
          (pressed? left-key)
          (pressed? right-key)))

; Movement update callback
(define (move-callback)
  (define-values (u d l r) (move-key-checker))
  (define (keys->vel a b) (* vel (- (if a 1 0) (if b 1 0))))
  (define v-x (keys->vel r l))
  (define v-y (keys->vel d u))
  (dmv v-x v-y main-ac))

; Movement update timer
(define move-timer
  (create-timer move-callback move-refresh-rate))

;; Initialization
; Show the canvas
(send main-frame show #t)
