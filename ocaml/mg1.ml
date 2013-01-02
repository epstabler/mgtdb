open Mgtdbp (* for definition of grammar type *)

let (g0:g) =[ 
  ([], [Sel "T"; Cat "C"]);
  ([], [Sel "T"; Pos "wh"; Cat "C"]);
  ([], [Sel "v"; Pos "k"; Cat "T"]);
  ([], [Sel "V"; Sel "D"; Cat "v"]);
  (["eats"], [Sel "D"; Pos "k"; Cat "V"]);
  (["laughs"], [Cat "V"]);
  (["the"], [Sel "N"; Cat "D"; Neg "k"]);
  (["which"], [Sel "N"; Cat "D"; Neg "k"; Neg "wh"]);
  (["king"], [Cat "N"]);
  (["pie"], [Cat "N"]);
];;

let (startCat:string) = "C"

(* simple SOV grammar with wh movement

    1 -> (0,["the";"king";"laughs"])
  | 2 -> (0,["the";"king";"the";"pie";"eats"])
  | 3 -> (0,["which";"pie";"the";"king";"eats"])
  | -1 -> (0,["the";"king";"the";"pie";"laughs"])
  | -2 -> (0,["the";"king";"pie";"eats"])
  | -3 -> (0,["which";"pie";"the";"king";"eats";"the";"pie"])
*)
