#lang typed/racket


(require
 (for-syntax racket/base racket/syntax syntax/parse))

(struct none () #:transparent)
(struct (a) some ([value : a]) #:transparent)

(define-type None none)
(define-type Some (∀ (a) (some a)))
(define-type Optional (∀ (a) (U (Some a) None)))

(: something? (∀ (a) ((Optional a) . -> . Boolean)))
(define (something? x)
  (match x
    [(some _) #t]
    [_ #f]))

(: optional? (Any -> Boolean))
(define (optional? x)
  (match x
    [(or (? none?) (? some?)) #t]
    [_ #f]))

(: map/f (∀ (a b) ((a . -> . b) (Optional a) . -> . (Optional b))))
(define (map/f f mx)
  (match mx
    [(some x) (some (f x))]
    [_ (none)]))

(: map/a (∀ (a b) ((Optional (a . -> . b)) (Optional a) . -> . (Optional b))))
(define (map/a mf mx)
  (match* (mf mx)
    [((some f) (some x)) (some (f x))]
    [(_ _) (none)]))

(: return (∀ (a) (a . -> . (Optional a))))
(define (return x)
  (some x))

(: map/m (∀ (a b) ((a . -> . (Optional b)) (Optional a) . -> . (Optional b))))
(define (map/m f mx)
  (match mx
    [(some x) (f x)]
    [_ (none)]))

(: join (∀ (a) ((Optional (Optional a)) . -> . (Optional a))))
(define (join mmx)
  (match mmx
    [(some (some x)) (some x)]
    [_ (none)]))



(define-syntax begin/m
  (syntax-parser
    [(_ me:expr) #'me]
    [(_ me:expr mes:expr ...+)
     (with-syntax ([ignore (generate-temporary 'ignore)])
       #'(map/m (λ (ignore) (begin/m mes ...)) me))]))

(define-syntax let/m
  (syntax-parser
    #:datum-literals (:)
    [(_ ([x:id mx:expr]) es:expr ...+)
     #'(map/m (λ (x) (begin/m es ...)) mx)]
    
    [(_ ([x:id : t:expr mx:expr]) es:expr ...+)
     #'(map/m (λ ([x : t]) (begin/m es ...)) mx)]
    
    [(_ (binding:expr more-bindings:expr ...+) es:expr ...+)
     #'(let/m (binding) (let/m (more-bindings ...) es ...))]))

(define-syntax let/f
  (syntax-parser
    #:datum-literals (:)
    [(_ ([x:id mx:expr]) e:expr)
     #'(map/f (λ (x) e) mx)]

    [(_ ([x:id : t:expr mx:expr]) e:expr)
     #'(map/f (λ ([x : t]) e) mx)]))

(module+ test
  (require typed/rackunit)

  (: safe-divide (Number Number . -> . (Optional Number)))
  (define (safe-divide x y)
    (if (zero? y) (none)
        (some (/ x y))))

  (check-equal? (safe-divide 3 3) (some 1))
  (check-equal? (safe-divide 3 0) (none))

  (check-equal?
   (let/m ([x : Number (safe-divide 3 0)])
     (safe-divide x 2))
   (none))

  (let/f ([x : Number (safe-divide 3 4)])
    (sqr x))

  (check-equal?
   (let/f ([x : Number (safe-divide 3 0)])
     (sqr x))
   (none)))




