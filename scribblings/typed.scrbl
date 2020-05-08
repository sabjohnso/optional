#lang scribble/manual
@require[@for-label[optional/typed typed/racket/base]]
@title["Typed Optional Values"]

@defmodule[optional/typed]

@defform[(Optional a)]{An optional value type @racket[a].}
@defproc[(optional? (x Any)) Boolean]{A predicate for optional values}

@defproc[(something? (x (Optional a))) Boolean]{
  Return @racket[#t] if the input has a value and false if it is @racket[(none)]
}

@defproc[(some (x a)) (Optional a)]{
  Return an optional value with the value present}
  
@defproc[(none) (Optional a)]{
  Return an optional value with the value missing}

@defproc[(optional-value (mx (Optional a)) (default-value a)) a]{
  Return the value from @racket[mx] if it has a value or @racket[default-value]
  if the value of @racket[mx] is missing.
}

@defproc[(optional-return (x a)) (Optional a)]{
  Return an optional value with the value present and equal to @racket[x].
  @racket[optional-return] is a synonym for @racket[some].
}

@defproc[(optional-map/f (f (-> a b)) (mx (Optional a))) (Optional b)]{
  Apply a function to an optional value.
}

@defproc[(optional-map/a (mf (Optional (-> a b))) (mx (Optional a))) (Optional b)]{
  Apply an optional function to an optional value.
}

@defproc[(optional-map/m (f (-> a (Optional b))) (mx (Optional a))) (Optional b)]{
  Apply a function that returns an optional value to an optional value.
}

@defproc[(optional-join (mmx (Optional (Optional a)))) (Optional a)]{
  Reduce multiple layers of the optional context by one.
}

@defform[(begin-m optional-expr ...+)]{
  Chaining of optional value expressions where the intermediate
  optional values are discarded except for the optional value from
  the final expression, which is returned from the form.
}

@defform[(let/m-optional ([id optional-expr] ...+) optional-body ...+)]{
  Monadic binding syntax for optional values.
}


