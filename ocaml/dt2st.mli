open Tktree
open Mgtdbp

type stateTree =
    LS of feature list list
  | XS of feature list list * stateTree * stateTree
  | OS of feature list list * stateTree
val stRoot : stateTree -> feature list list
val st2t : stateTree -> tree
val dt2st : derivationTree -> stateTree
