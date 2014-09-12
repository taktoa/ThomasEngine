; utility.rkt
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
  racket/gui)

(provide
 (all-defined-out))

; Bracket x within [a, b]
(define (bound x a b)
  (cond [(> x b) b]
        [(< x a) a]
        [true    x]))

; Convert Hz to milliseconds
(define (hz-to-ms f) (inexact->exact (round (/ 1000 f))))

; Create a timer
(define (create-timer callback rate)
  (new timer%
       [notify-callback callback]
       [interval (hz-to-ms rate)]))