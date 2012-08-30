open Tktree
open Mgtdbp

type mcfgTree =
    Lf of feature list list * bool * string list list
  | Xf of feature list list * bool * string list list * mcfgTree * mcfgTree
  | Of of feature list list * bool * string list list * mcfgTree
val stringBool : bool -> string
val stringSSs2 : string list list -> string
val mcfg2t : mcfgTree -> tree
val mcfgtLabel :
  mcfgTree -> feature list list * bool * string list * string list list
val selectUnique3 :
  string ->
  feature list list ->
  'a list list -> feature list list * 'a list list * 'a list
val dt2mcfg : derivationTree -> mcfgTree
