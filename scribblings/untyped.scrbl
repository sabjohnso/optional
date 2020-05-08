#lang scribble/manual
@require[@for-label[optional racket/base]]

@title["Optional Values"]


@defmodule[optional]

@defproc[(optional? (x any)) boolean?]{
  A type predicate returning @racket[#t] for optional values}

@defproc[(something? (mx optional?)) boolean?]{
  Return @racket[#t] if the input has a value and false if it is @racket[(none)]
}

@defproc[(some (x any)) optional?]{
  Return an optional value with the value present}
  
@defproc[(none) optional?]{
  Return an optionao value with the value missing}

@defproc[(optional-value (mx optional?) (default-value a)) a]{
  Return the value from @racket[mx] if it has a value or @racket[default-value]
  if the value of @racket[mx] is missing.
}

@defproc[(optional-return (x a)) optional?]{
  Return an optional value with the value present and equal to @racket[x].
  @racket[optional-return] is a synonym for @racket[some].
}

@defproc[(optional-map/f (f (-> any any)) (mx optional?)) optional?]{
  Apply a function to an optional value.
}

@defproc[(optional-map/a (mf (optionalof (-> any any))) (mx optional?)) optional?]{
  Apply an optional function to an optional value.
}

@defproc[(optional-map/m (f (-> any optional?)) (mx optional?)) optional?]{
  Apply a function that returns an optional value to an optional value.
}

@defproc[(optional-join (mmx (optionalof optional?))) optional?]{
  Reduce multiple layers of the optional context by one.
}
