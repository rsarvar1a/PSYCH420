#lang typed/racket

(require "base.rkt")

;;;;;;;;;;;;;;;;;;;;
;
;	VSA MODEL
;
;;;;;;;;;;;;;;;;;;;;

;
; A representation of a binary-spatter-coded (BSC) hypervector.
;
(define-struct 
  VSA-hypervector 
  ([data : (Listof Integer)]))

; There is no identity element over bundling in the BSC model.
(define 
  (VSA-vec-empty 
	[dimensions : Integer])
  : VSA-hypervector
  (__not-implemented))

; Explicitly creates a hypervector with the given contents.
(define 
  (VSA-vec-from 
	[data : (Listof Integer)]) 
  : VSA-hypervector
  (make-VSA-hypervector data))

; Returns a set of hypervectors that are identity elements over binding.
; (bind (VSA-identity (len x)) x) -> x
(define 
  (VSA-vec-identity 
	[dimensions : Integer]) 
  : VSA-hypervector
  (make-VSA-hypervector
	(build-list dimensions (λ (_) 0))))

; Returns a hypervector sampled according to the input density at random from n-dimensional boolean space.
(define 
  (VSA-vec-random 
	[dimensions : Integer] 
	#:density [density : Real 0.5])
  : VSA-hypervector
  (make-VSA-hypervector
	(build-list 
	  dimensions
	  (λ (_2) (if (< (random) density) 1 0)))))

;;;;;;;;;;;;;;;;;;;;
;
;	INTERNALS
;
;;;;;;;;;;;;;;;;;;;;

; Gets the fixed dimensionality of a list of hypervectors.
(define 
  (__lst-dimensions 
	[lst : (Listof VSA-hypervector)]) 
  : Integer
  (length (VSA-hypervector-data (first lst))))

; Given a list of hypervectors, creates a list by extracting the component of each hypervector matching the input dimension.
(define 
  (__gather-components 
	[lst : (Listof VSA-hypervector)] 
	[dim : Integer]) 
  : (Listof Integer)
  (map 
	(λ ([vec : VSA-hypervector])
	  (list-ref 
		(VSA-hypervector-data vec)
		dim))
	lst))

;;;;;;;;;;;;;;;;;;;;
;
;	OPERATORS
;
;;;;;;;;;;;;;;;;;;;;

; Binds a hypervector v or list of hypervectors vs to the hypervector u.
; Returns a hypervector that is dissimilar to all input vectors via XORing their representations.
(define 
  (VSA-vec-bind 
	[vecs : (Listof VSA-hypervector)])
  : VSA-hypervector
  (make-VSA-hypervector
	(build-list
	  (__lst-dimensions vecs)
	  (λ ([dim : Integer]) : Integer (__intxor (__gather-components vecs dim))))))

; Bundles a hypervector v or list of hypervectors vs to the hypervector u.
; Returns a hypervector that is maximally similar to all input vectors via component-wise majority voting.
(define 
  (VSA-vec-bundle 
	[vecs : (Listof VSA-hypervector)])
  : VSA-hypervector
  (make-VSA-hypervector
	(build-list
	  (__lst-dimensions vecs)
	  (λ ([dim : Integer]) : Integer (__mode (__gather-components vecs dim))))))

; Returns the binding-inverse of u. In the BSC model, each vector is its own inverse.
(define 
  (VSA-vec-inverse 
	[vec : VSA-hypervector])
  : VSA-hypervector
  vec)

; Returns the bundling-inverse of u. In the BSC model, each vector's bundling-inverse is its boolean negation.
(define 
  (VSA-vec-negative 
	[vec : VSA-hypervector])
  : VSA-hypervector
  (make-VSA-hypervector
	(map
	  (λ ([component : Integer]) (- 1 component))
	  (VSA-hypervector-data vec))))

; Unbinds the hypervector v from the bound hypervector u.
; Returns the resulting hypervector.
(define 
  (VSA-vec-unbind 
	[u : VSA-hypervector]
	[v : VSA-hypervector])
  : VSA-hypervector
  (VSA-vec-bind (list u (VSA-vec-inverse v))))

; Unbundles the hypervector v from the bundled hypervector u.
; Returns the resulting hypervector.
(define 
  (VSA-vec-unbundle 
	[u : VSA-hypervector] 
	[v : VSA-hypervector])
  : VSA-hypervector
  (VSA-vec-bundle (list u (VSA-vec-negative v))))

;;;;;;;;;;;;;;;;;;;;
;
;	MEASURES
;
;;;;;;;;;;;;;;;;;;;;

; Returns the Hamming dissimilarity (or distance) between the two hypervectors.
; The dissimilarity is the 0-1 inverse of the similarity.
(define 
  (VSA-vec-hamming-distance 
	[u : VSA-hypervector] 
	[v : VSA-hypervector])
  : Real
  (- 1.0 (VSA-vec-hamming-similarity u v)))

; Returns the Hamming similarity between the two hypervectors.
; The similarity is the 0-1 inverse of the dissimilarity.
(define 
  (VSA-vec-hamming-similarity 
	[u : VSA-hypervector] 
	[v : VSA-hypervector])
  : Real
  (/ 
	(apply 
	  + 
	  (build-list
		(__lst-dimensions (list u))
		(λ ([dim : Integer]) (__intxnor (__gather-components (list u v) dim)))))
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

