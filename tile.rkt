#lang racket
(require 2htdp/universe)
(require 2htdp/image)

;; The goal of this file is to render a finite subsection of an infinite
;; grass plane with a character at the center of the screen, controllable
;; with the arrow keys.

;; Grass texture for testing
(define grass (bitmap/file "grass.png"))
;; Tile width and height defined by that texture for testing
(define tile-width (image-width grass))
(define tile-height (image-height grass))
;; Screen width and height are arbitrary for now
(define screen-width 100)
(define screen-height 100)

(define-struct dim [cx cy sw sh tw th])

(define (current-dims cx cy)
  (make-dim cx cy screen-width screen-height tile-width tile-height))

;; Function that gives the tile at the requisite tile coordinates
(define (get-tile tx ty) grass)

(define (half x) (/ x 2))

;; Function that gives the all the tiles on screen
(define (get-tiles dims)
  (range 1))

;; Function that gives the scene centered at the requisite pixel coordinates
(define (get-scene cx cy)
  (tile-scene (empty-scene screen-width screen-height) cx cy))

(define (tile-tesselate tw th) 1)

(define (tile-scene s cx cy) 1)
;; (place-image x y scene)
