open Mgtdbp (* for definition of grammar type *)

(* example: "the king knows which wine the queen prefers" etc with startCat="C"*)
let (g0:g) =[
([],[Sel "V"; Cat "C"]);                  ([],[Sel "V"; Pos "wh"; Cat "C"]);
(["the"],[Sel "N"; Cat "D"]);             (["which"],[Sel "N"; Cat "D"; Neg "wh"]);
(["king"],[Cat "N"]);                     (["queen"],[Cat "N"]);
(["wine"],[Cat "N"]);                     (["beer"],[Cat "N"]);
(["knows"],[Sel "C"; Sel "D"; Cat "V"]);  (["says"],[Sel "C"; Sel "D"; Cat "V"]);
(["prefers"],[Sel "D"; Sel "D"; Cat "V"]);(["drinks"],[Sel "D"; Sel "D"; Cat "V"]);
]

let (startCat:string) = "C"
