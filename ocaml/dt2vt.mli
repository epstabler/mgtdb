open Tktree
open Mgtdbp

type expr =
    (string list * feature list * int) * (string list * feature list) list
val head2string : string list * feature list * int -> string
val mover2string : string list * feature list -> string
val movers2string : (string list * feature list) list -> string
val expr2string :
  (string list * feature list * int) * (string list * feature list) list ->
  string
type valueTree =
    Lv of expr
  | Xv of expr * valueTree * valueTree
  | Ov of expr * valueTree
val vtRoot : valueTree -> expr
val vt2t : valueTree -> tree
val noOtherMover : string -> ('a * feature list) list -> bool
val selectUniqueMover :
  string ->
  ('a * feature list) list -> ('a * feature list) * ('a * feature list) list
val dt2vt : derivationTree -> valueTree
