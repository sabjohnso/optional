#lang typed/racket


(require
 (for-syntax racket/base racket/syntax syntax/parse))

(provide
 Optional
 optional? something?
 some none
 optional-value
 optional-map/f optional-map/a optional-map/m
 optional-return optional-join
 begin/m-optional let/m-optional let/f-optional)

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

(: optional-value (∀ (a) ((Optional a) a . -> . a)))
(define (optional-value mx default-value)
  (match mx
    [(some x) x]
    [_ default-value]))

(: optional-map/f (∀ (a b) ((a . -> . b) (Optional a) . -> . (Optional b))))
(define (optional-map/f f mx)
  (match mx
    [(some x) (some (f x))]
    [_ (none)]))

(: optional-map/a (∀ (a b) ((Optional (a . -> . b)) (Optional a) . -> . (Optional b))))
(define (optional-map/a mf mx)
  (match* (mf mx)
    [((some f) (some x)) (some (f x))]
    [(_ _) (none)]))

(: optional-return (∀ (a) (a . -> . (Optional a))))
(define (optional-return x)
  (some x))

(: optional-map/m (∀ (a b) ((a . -> . (Optional b)) (Optional a) . -> . (Optional b))))
(define (optional-map/m f mx)
  (match mx
    [(some x) (f x)]
    [_ (none)]))

(: optional-join (∀ (a) ((Optional (Optional a)) . -> . (Optional a))))
(define (optional-join mmx)
  (match mmx
    [(some (some x)) (some x)]
    [_ (none)]))



(define-syntax begin/m-optional
  (syntax-parser
    [(_ me:expr) #'me]
    [(_ me:expr mes:expr ...+)
     (with-syntax ([ignore (generate-temporary 'ignore)])
       #'(optional-map/m (λ (ignore) (begin/m-optional mes ...)) me))]))

(define-syntax let/m-optional
  (syntax-parser
    #:datum-literals (:)
    [(_ ([x:id mx:expr]) es:expr ...+)
     #'(optional-map/m (λ (x) (begin/m-optional es ...)) mx)]
    
    [(_ ([x:id : t:expr mx:expr]) es:expr ...+)
     #'(optional-map/m (λ ([x : t]) (begin/m-optional es ...)) mx)]
    
    [(_ (binding:expr more-bindings:expr ...+) es:expr ...+)
     #'(let/m-optional (binding) (let/m-optional (more-bindings ...) es ...))]))

(define-syntax let/f-optional
  (syntax-parser
    #:datum-literals (:)
    [(_ ([x:id mx:expr]) e:expr)
     #'(optional-map/f (λ (x) e) mx)]

    [(_ ([x:id : t:expr mx:expr]) e:expr)
     #'(optional-map/f (λ ([x : t]) e) mx)]))

(module+ test
  (require typed/rackunit)

  (: safe-divide (Number Number . -> . (Optional Number)))
  (define (safe-divide x y)
    (if (zero? y) (none)
        (some (/ x y))))

  (check-equal? (safe-divide 3 3) (some 1))
  (check-equal? (safe-divide 3 0) (none))

  (check-equal?
   (let/m-optional ([x : Number (safe-divide 3 0)])
     (safe-divide x 2))
   (none))

  (check-equal?
   (let/f-optional ([x : Number (safe-divide 3 4)])
     (sqr x))
   (some 9/16))

  (check-equal?
   (let/f-optional ([x : Number (safe-divide 3 0)])
     (sqr x))
   (none))

  (check-equal? (optional-value (some 'x) 'y) 'x)
  (check-equal? (optional-value (none) 'y) 'y))





