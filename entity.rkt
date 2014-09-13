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

(provide
 (all-defined-out))

(define entity%
  (class object%
    ;; Class fields
    (init-field
     [update-callback (λ () (hash))])

    (field
     [properties (hash)])

    ;; Public getters and setters
    (define/public (update)
      (prop-merge (update-callback)))

    (define/public (prop-get-all) properties)

    (define/public (prop-get k) (hash-ref properties k))

    (define/private (prop-merge changes)
      (hash-for-each
       changes
       (λ (k v)
         (if (eq? v 'delete)
             (prop-rem k)
             (prop-set k v)))))

    (define/private (prop-set k v) (hash-set! properties k v))
    (define/private (prop-rem k) (hash-remove! properties k))

    (super-new)))

(define display-entity%
  (class entity%
    ;; Class fields
    (inherit
      update
      prop-get
      prop-get-all)

    (init-field
     [render-callback (λ (p) #f)])

    ;; Public getters and setters
    (define/public (render)
      (render-callback (prop-get-all)))

    (super-new)))
