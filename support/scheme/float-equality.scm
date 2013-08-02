(define-module (float-equality)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1) ;; fold
  #:export (=?))

(define-method (=? (a <number>) (b <number>) . rest)
  (let-optional rest ((tolerance 0.001))
   (< (abs (- a b)) tolerance)))

(define-method (=? (a <list>) (b <list>) . rest)
  (fold (lambda (x y prev)
          (and prev (apply =? x y rest))) #t a b))

(define-method (=? (a <pair>) (b <pair>) . rest)
  (and (apply =? (car a) (car b) rest)
       (apply =? (cdr a) (cdr b) rest)))

(define-method (=? (a <vector>) b . rest)
  (apply =? b (vector->list a) rest))

;; Rather than trying to do all pairwise combinations, how about we just
;; handle the first one and rely on the associativity of equals? 
(define-method (=? (a <uvec>) b . rest)
  (apply =? b (array->list a) rest))

(define-method (=? a b . rest)
  #f)

(define-method (=? (tolerance <number>))
  (lambda (a b)
    (=? a b tolerance)))

