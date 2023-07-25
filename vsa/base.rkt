#lang typed/racket

; Some internals (common requirements and internal/helper functions).

;;;;;;;;;;;;;;;;;;;;
;
;	INTERNAL
;
;;;;;;;;;;;;;;;;;;;;

; Error/TODO signalling because I'm way too scatterbrained this month to actually keep an issue tracker.
(define (__not-implemented)
  (raise 'NotImplemented))

;;;;;;;;;;;;;;;;;;;;
;
;	MATH HELPERS
;
;;;;;;;;;;;;;;;;;;;;

; XORs a list of 0-1 components.
(define 
  (__intxor 
	[components : (Listof Integer)]) 
  : Integer
  (modulo (apply + components) 2))

; XNORs a list of 0-1 components.
(define 
  (__intxnor 
	[components : (Listof Integer)])
  : Integer
  (- 1 (__intxor components)))

; Finds the most common element in a list of 0-1 components.
; Equivalent to the rounded average.
(define 
  (__mode 
	[components : (Listof Integer)])
  : Integer
  (round (/ (apply + components) (length components))))

;;;;;;;;;;;;;;;;;;;;
;
;	MODULE
;
;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out))

