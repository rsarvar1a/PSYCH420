#lang racket

(require "base.rkt")
(require "vec.rkt")

;;;;;;;;;;;;;;;;;;;;
;
;	MEMORY MODEL
;
;;;;;;;;;;;;;;;;;;;;

;
; An associative memory store that maps symbols to hypervectors.
;
(define-struct VSA-memory (sym->hv hv->sym dimensions) #:mutable)

; Returns an empty memory store operating with the given dimensionality.
; Internally, the memory store pregenerates a basis in the given hypervector space, and keeps track of associations.
; Thus, the maxmimum number of primitives expressible in a VSA-memory is equal to the memory's dimensionality.
(define (VSA-memory-new dimensions)
  (make-VSA-memory (make-hash) (make-hash) dimensions))

;;;;;;;;;;;;;;;;;;;;
;
;	METHODS
;
;;;;;;;;;;;;;;;;;;;;

; Returns the hypervector that represents the given symbol in this memory store.
(define (VSA-memory-lookup memory symbol)
  (hash-ref 
	(VSA-memory-sym->hv memory)
	symbol))

; Registers a symbol to a corresponding hypervector in associative memory.
(define (VSA-memory-insert! memory symbol)
  (cond 
	[(hash-has-key? (VSA-memory-sym->hv memory) symbol) (hash-ref (VSA-memory-sym->hv memory) symbol)]
	[else
	  (let ([new-vec (VSA-vec-random (VSA-memory-dimensions memory))])
		(hash-set! (VSA-memory-sym->hv memory) symbol new-vec)
		(hash-set! (VSA-memory-hv->sym memory) new-vec symbol)
		new-vec)]))

; Returns the symbol associated with the hypervector in memory that is maximally similar to the input vector.
(define (VSA-memory-recall memory vec)
  (hash-ref 
	(VSA-memory-hv->sym memory)
	(argmax 
	  (λ (known-vec) (VSA-vec-hamming-similarity vec known-vec)) 
	  (hash-keys (VSA-memory-hv->sym memory)))))


; Removes a symbol from the memory store, freeing the associated hypervector.
(define (VSA-memory-remove! memory symbol)
  (cond [(hash-has-key? (VSA-memory-sym->hv memory) symbol)
		 (hash-remove! (VSA-memory-hv->sym memory) (hash-ref (VSA-memory-sym->hv memory) symbol))])
  (hash-remove! (VSA-memory-sym->hv memory) symbol))

;;;;;;;;;;;;;;;;;;;;
;
;	MODULE
;
;;;;;;;;;;;;;;;;;;;;

(provide VSA-memory
		 
		 VSA-memory-new

		 VSA-memory-lookup
		 VSA-memory-insert!
		 VSA-memory-recall
		 VSA-memory-remove!)

