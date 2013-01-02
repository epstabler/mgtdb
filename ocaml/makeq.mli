module Makeq :
  functor (X : sig type t val le : t -> t -> bool end) ->
    sig
      type t
      val empty : t
      val is_empty : t -> bool
      val add : X.t -> t -> t
      exception Empty
      val extract_min : t -> X.t * t
      val merge : t -> t -> t
    end
