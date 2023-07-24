#lang racket

(require "../vsa.rkt")

; Create a VSA memory store with the given dimensionality.

(define dimensions 10000)
(define country-info-memory (VSA-memory-new dimensions))

; Specify the information we want to know about in our model.

(define interesting-information
  (list 
	'NAME 'AMERICA 'MEXICO
	'CAPITAL 'WASHINGTONDC 'MEXICOCITY
	'CURRENCY 'DOLLAR 'PESO))

; Load all of it into memory.

(for-each 
  (λ (sym) (VSA-memory-insert! country-info-memory sym))
  interesting-information)

; Construct some kind of symbolic statement about our information.

(define (get-vec sym) 
  (VSA-memory-lookup country-info-memory sym))

(define name (get-vec 'NAME))
(define capital (get-vec 'CAPITAL))
(define currency (get-vec 'CURRENCY))

(define america-info 
  (VSA-vec-bundle
	(list
	  (VSA-vec-bind (list name (get-vec 'AMERICA)))
	  (VSA-vec-bind (list capital (get-vec 'WASHINGTONDC)))
	  (VSA-vec-bind (list currency (get-vec 'DOLLAR))))))

(define mexico-info
  (VSA-vec-bundle
	(list
	  (VSA-vec-bind (list name (get-vec 'MEXICO)))
	  (VSA-vec-bind (list capital (get-vec 'MEXICOCITY)))
	  (VSA-vec-bind (list currency (get-vec 'PESO))))))

(define country-info-statement 
  (VSA-vec-bind (list america-info mexico-info)))

; We can now retrieve some information from this statement.

(define dollar-of-mexico 
  (VSA-vec-bind (list country-info-statement (get-vec 'DOLLAR))))

(VSA-memory-recall country-info-memory dollar-of-mexico)
; if all goes well, spits out 'PESO
