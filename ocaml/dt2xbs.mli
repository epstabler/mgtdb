open Tktree
open Mgtdbp

type traceIndex = Nx | Ix of int
type xbTree =
    X01 of string * string list
  | X11 of string * xbTree
  | X12 of string * bool * xbTree * xbTree
  | XP0 of string * traceIndex * string list
  | XP1 of string * traceIndex * xbTree
  | XP2 of string * traceIndex * bool * xbTree * xbTree
val xb2t : xbTree -> tree
val traceOf : xbTree -> xbTree
val indexOf : xbTree -> traceIndex
val selectUniqueTripleX :
  string ->
  feature list list ->
  xbTree list -> feature list list * xbTree list * xbTree
val catOfFs : feature list -> string
class gensym :
object val mutable x : int method next : int method reset : unit end
val myindex : gensym
val insertIndex : int -> xbTree -> xbTree
val dt2xbs0 : derivationTree -> feature list list * bool * xbTree list
val dt2xbs : derivationTree -> xbTree list
val dt2xb : derivationTree -> xbTree
