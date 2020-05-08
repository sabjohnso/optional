#lang racket/base

(require
 racket/class protocol
 "private/optional.rkt")

(provide
 (all-from-out "private/optional.rkt")
 optional-monad)

(require protocol)

(define OptionalMonad%
  (class* (send monad-fail instance-base)
    ((send monad-fail instance-interface))
    (super-new)
    (define/override (in-context? x) (optional? x))
    (define/override (map/f f mx) (optional-map/f f mx))
    (define/override (return x) (some x))
    (define/override (map/a mf mx) (optional-map/a mf mx))
    (define/override (join mmx) (optional-join mmx))
    (define/override (map/m f mx) (optional-map/m f mx))
    (define/override (fail) (none))))

(define optional-monad
  (new OptionalMonad%))
