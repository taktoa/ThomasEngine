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

(define property-layer%
  (class object%
    
    ; Class field
    (init-field
     bitmap
     hash-table)
    
    ;; Public  function
    (define/public (property-at-pos x y)
      (hash-color x y))
    
    ; Read in collision diskmap at path
    (define bitmap-dc
      (new bitmap-dc% [bitmap bitmap]))

    ; Returns the hash table's value for a color key at (x, y)
    (define/private (hash-color x y) 
      (define color-gotten (make-object color% "white"))
      (send bitmap-dc get-pixel x y color-gotten)
      (hash-ref color-value-hash (color-numbers color-gotten) 'unknown))
    
    ; Utility-type function(?) used to turn a color object into a list of form
    ; '(R G B)
    (define/private (color-numbers color) (list (send color red) (send color green) (send color blue)))
    
    ; General function that maps f over the keys in a hash, while retaining the same values
    (define/private (hash-key-map f h)
      (for/hash ([(k v) (in-hash h)]) (values (f k) v)))
    
    ; Takes a color name (a member of (send the-color-database get-names)) and 
    ; gives its RGB components as a list of form '(R G B)
    (define/private (color-value color-name)
      (define color (make-object color% color-name))
      (color-numbers color))

    ; Takes the hash-table given at initialization and turns it into a more usable form
    (define color-value-hash (hash-key-map (λ (color-name) (color-value color-name)) hash-table))
    
    (super-new)))

;(define (color-numbers color) (list (send color red) (send color green) (send color blue)))
;
;(define (color-values color-list) (map color-value color-list))
;
;(define (hash-key-map f h)
;  (for/hash ([(k v) (in-hash h)]) (values (f k) v)))
;
;(define test-hash (hash "black" 'collision "white" 'nothing "green" 'poison))
;
;(define (color-value color-name)
;  (define color (make-object color% color-name))
;  (color-numbers color))