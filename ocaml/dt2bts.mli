open Tktree
open Mgtdbp

type bareTree =
    Lex of string list * feature list
  | LT of bareTree * bareTree
  | GT of bareTree * bareTree
  | Trace
val bt2t : bareTree -> tree
val checkHead : bareTree -> bareTree
val selectUniqueTriple :
  string ->
  feature list list ->
  bareTree list -> feature list list * bareTree list * bareTree
val dt2bts0 : derivationTree -> feature list list * bool * bareTree list
val dt2bts : derivationTree -> bareTree list
val dt2bt : derivationTree -> bareTree
