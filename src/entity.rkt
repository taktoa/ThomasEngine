; entity.rkt
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
(require)

(provide
 (all-defined-out))

(define (make-entity prop)
  (new entity% [properties prop]))

(define entity%
  (class object%
    ;; Class fields
    (init-field
     [properties (hash)])
    
    ;; Public functions
    ; Change a property
    (define/public prop-update
      (case-lambda
        [(k v) (prop-update properties k v)]
        [(p k v) (if (equal? v 'delete)
                     (hash-remove p k)
                     (hash-set p k v))]))
    
    ; Change multiple properties at once
    (define/public (modify-props changes)
      (define cl (hash->list changes))
      
      ; I almost can't believe this works.
      (define new-props
        ((apply compose
                (map (λ (c)
                       (λ (p) (prop-update p (car c) (cdr c))))
                     cl))
         properties))
      
      (new this% [properties new-props]))
    
    ; Get a specific property of the entity
    (define/public (prop-get k) (hash-ref properties k))
    
    ; Get all properties of this entity
    (define/public (prop-get-all) (hash-copy properties))
    
    ;; Class initialization
    (super-new)))