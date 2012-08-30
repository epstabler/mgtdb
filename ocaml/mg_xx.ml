open Mgtdbp (* for definition of grammar type *)

(* example: the copy language over {a,b} with startCatx="T"*)
let (g0:g) =[ 
([],[Cat "T"; Neg "r"; Neg "l"]); ([],[Sel "T"; Pos "r"; Pos "l"; Cat "T"]);
(["a"],[Sel "T"; Pos "r"; Cat "A"; Neg "r"]); (["b"],[Sel "T"; Pos "r"; Cat "B"; Neg "r"]); 
(["a"],[Sel "A"; Pos "l"; Cat "T"; Neg "l"]); (["b"],[Sel "B"; Pos "l"; Cat "T"; Neg "l"]); 
]

let (startCat:string) = "T"
