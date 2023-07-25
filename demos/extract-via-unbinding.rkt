#lang typed/racket

(require "../vsa.rkt")

; Create a VSA memory store with the given dimensionality.

(define dimensions 1000)
(define memory (VSA-memory-new dimensions))

; Specify our information.

(define square (VSA-memory-insert! memory 'SQUARE))
(define circle (VSA-memory-insert! memory 'CIRCLE))
(define triangle (VSA-memory-insert! memory 'TRIANGLE))

(define red (VSA-memory-insert! memory 'RED))
(define blue (VSA-memory-insert! memory 'BLUE))
(define green (VSA-memory-insert! memory 'GREEN))

; Create a scene.

(define scene 
  (VSA-vec-bundle
	(VSA-vec-bind red square)
	(VSA-vec-bind blue circle)
	(VSA-vec-bind green triangle)))

; We can now use the unbinding operator to extract information through the bundle.

(define colour-of-square
  (VSA-vec-unbind scene square))

(VSA-memory-recall memory colour-of-square)

