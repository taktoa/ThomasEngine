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
 (contract-out 
  [property-layer% property-layer%/c]))

;; Contracts
(define color-list (send the-color-database get-names))

(define (in-cdb? color-name) (member color-name color-list))

(define (are-in-cdb? color-lst) (andmap in-cdb? color-lst))

(define (keys-in-cdb? hash-table) (are-in-cdb? (hash-keys hash-table)))

(define (keys-are-strings? hash-table) (andmap string? (hash-keys hash-table)))

(define (values-are-symbols? hash-table) (andmap symbol? (hash-values hash-table)))

(define property-hash/c 
  (flat-named-contract
   'property-hash
   (and/c hash? keys-are-strings? values-are-symbols? keys-in-cdb?)))

(define property-layer%/c
  (class/c
   (init-field
    [hash-table property-hash/c]
    [bitmap (is-a?/c bitmap%)])
   [property-at-pos (integer? integer? . ->m . symbol?)]))

;; Classes
(define property-layer%
  (class object%
    ;; Class fields
    (init-field
     bitmap
     hash-table)
    
    ;; Local variables
    ; Define bitmap drawing context
    (define bitmap-dc
      (send bitmap make-dc))
    
    ;; Private functions
    ; Returns the hash table's value for a color key at (x, y)
    (define/private (hash-color x y)
      (define color-gotten (make-object color% "white"))
      (send bitmap-dc get-pixel x y color-gotten)
      (hash-ref color-value-hash (color-numbers color-gotten) 'unknown))
    
    ; Utility function used to turn a color object into a list of form '(R G B)
    (define/private (color-numbers color)
      (list (send color red) (send color green) (send color blue)))
    
    ; General function that maps f over the keys in a hash, while retaining the same values
    (define/private (hash-key-map f h)
      (for/hash ([(k v) (in-hash h)]) (values (f k) v)))
    
    ; Takes a color name (from the-color-database) and gives its RGB components
    (define/private (color-value color-name)
      (define color (make-object color% color-name))
      (color-numbers color))
    
    ; Takes the hash-table given at initialization and turns it into a more usable form
    (define color-value-hash
      (hash-key-map (λ (color-name) (color-value color-name)) hash-table))
    
    ;; Public functions
    ; Get property at a given position
    (define/public (property-at-pos x y)
      (hash-color x y))
    
    ;; Class initialization
    (super-new)))