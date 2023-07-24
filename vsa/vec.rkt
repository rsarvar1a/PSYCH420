#lang racket

(require "base.rkt")

;;;;;;;;;;;;;;;;;;;;
;
;	VSA MODEL
;
;;;;;;;;;;;;;;;;;;;;

;
; A representation of a binary-spatter-coded (BSC) hypervector.
;
(define-struct VSA-hypervector (data))

; There is no identity element over bundling in the BSC model.
(define (VSA-vec-empty dimensions)
  (__not-implemented))

; Explicitly creates a hypervector with the given contents.
(define (VSA-vec-from data)
  (make-VSA-hypervector 
	(list->array data)))

; Returns a set of hypervectors that are identity elements over binding.
; (bind (VSA-identity (len x)) x) -> x
(define (VSA-vec-identity dimensions)
  (make-VSA-hypervector
	(list->array
	  (build-list dimensions (λ (_) 0)))))

; Returns a hypervector sampled according to the input density at random from n-dimensional boolean space.
(define (VSA-vec-random dimensions #:density [density 0.5])
  (make-VSA-hypervector
	(list->array
	  (build-list 
		dimensions
		(λ (_2) (if (< (random) density) 1 0))))))

;;;;;;;;;;;;;;;;;;;;
;
;	INTERNALS
;
;;;;;;;;;;;;;;;;;;;;

; Gets the fixed dimensionality of a list of hypervectors.
(define (__lst-dimensions lst)
  (length (array->list (VSA-hypervector-data (first lst)))))

; Given a list of hypervectors, creates a list by extracting the component of each hypervector matching the input dimension.
(define (__gather-components lst dim)
  (map 
	(λ (vec) 
	  (array-ref 
		(VSA-hypervector-data vec)
		(make-vector 1 dim)))
	lst))

;;;;;;;;;;;;;;;;;;;;
;
;	OPERATORS
;
;;;;;;;;;;;;;;;;;;;;

; Binds a hypervector v or list of hypervectors vs to the hypervector u.
; Returns a hypervector that is dissimilar to all input vectors via XORing their representations.
(define (VSA-vec-bind vecs)
  (make-VSA-hypervector
	(list->array
	  (build-list
		(__lst-dimensions vecs)
		(λ (dim) (__intxor (__gather-components vecs dim)))))))

; Bundles a hypervector v or list of hypervectors vs to the hypervector u.
; Returns a hypervector that is maximally similar to all input vectors via component-wise majority voting.
(define (VSA-vec-bundle vecs)
  (make-VSA-hypervector
	(list->array
	  (build-list
		(__lst-dimensions vecs)
		(λ (dim) (__mode (__gather-components vecs dim)))))))

; Returns the binding-inverse of u. In the BSC model, each vector is its own inverse.
(define (VSA-vec-inverse vec)
  vec)

; Returns the bundling-inverse of u. In the BSC model, each vector's bundling-inverse is its boolean negation.
(define (VSA-vec-negative vec)
  (make-VSA-hypervector
	(list->array
	  (map
		(λ (component) (- 1 component))
		(array->list (VSA-hypervector-data vec))))))

; Unbinds the hypervector v from the bound hypervector u.
; Returns the resulting hypervector.
(define (VSA-vec-unbind u v)
  (VSA-vec-bind (list u (VSA-vec-inverse v))))

; Unbundles the hypervector v from the bundled hypervector u.
; Returns the resulting hypervector.
(define (VSA-vec-unbundle u v)
  (VSA-vec-bundle (list u (VSA-vec-negative v))))

;;;;;;;;;;;;;;;;;;;;
;
;	MEASURES
;
;;;;;;;;;;;;;;;;;;;;

; Returns the Hamming dissimilarity (or distance) between the two hypervectors.
; The dissimilarity is the 0-1 inverse of the similarity.
(define (VSA-vec-hamming-distance u v)
  (- 1.0 (VSA-vec-hamming-similarity u v)))

; Returns the Hamming similarity between the two hypervectors.
; The similarity is the 0-1 inverse of the dissimilarity.
(define (VSA-vec-hamming-similarity u v)
  (/ 
	(apply 
	  + 
	  (build-list
		(__lst-dimensions (list u))
		(λ (dim) (__intxnor (__gather-components (list u v) dim)))))
	(__lst-dimensions (list u))))

;;;;;;;;;;;;;;;;;;;;
;
;	MODULE
;
;;;;;;;;;;;;;;;;;;;;

(provide VSA-hypervector

		 VSA-vec-empty
		 VSA-vec-from
		 VSA-vec-identity
		 VSA-vec-random

		 VSA-vec-bind
		 VSA-vec-bundle
		 VSA-vec-inverse
		 VSA-vec-negative
		 VSA-vec-unbind
		 VSA-vec-unbundle

		 VSA-vec-hamming-distance
		 VSA-vec-hamming-similarity)

