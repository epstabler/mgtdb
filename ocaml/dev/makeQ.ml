(*  file: makeQ.ml   (generated from td-beam.nw)
 creator: Jean-Christophe.Filliatre@lri.fr
 source: https://groups.google.com/group/fa.caml/msg/526cc7a4a9adc664?dmode=source
  date: 30 Jun 2011 20:14:34 +0200
*)

module MakeQ (X : sig type t val le : t -> t -> bool end) :
sig
type t
val empty : t
val is_empty : t -> bool
val add : X.t -> t -> t
exception Empty
val extract_min : t -> X.t * t
val merge : t -> t -> t
end
=  struct

  type t = E | T of int * X.t * t * t

  exception Empty

  let rank = function E -> 0 | T (r,_,_,_) -> r

  let make x a b =
    let ra = rank a and rb = rank b in
      if ra >= rb then T (rb + 1, x, a, b) else T (ra + 1, x, b, a)

  let empty = E

  let is_empty = function E -> true | T _ -> false

  let rec merge h1 h2 = match h1,h2 with
    | E, h | h, E -> 
      h
    | T (_,x,a1,b1), T (_,y,a2,b2) ->
      if X.le x y then make x a1 (merge b1 h2) else make y a2 (merge h1 b2)

  let add x h = merge (T (1, x, E, E)) h

  let extract_min = function
    | E -> raise Empty
    | T (_,x,a,b) -> x, merge a b
end;;
