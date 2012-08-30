(*   File   : mg_nz.pl - naive zapotec VISO
     Author : E Stabler
     Updated: Mar 00
*)
open Mgtdbp (* for definition of grammar type *)

(* example: "praise -s titus lavinia" etc with startCat="C"*)
let (g0:g) =[
([],[Sel "I";Cat "C"]);
(["-s"],[Sel "Pred";Pos "Infl";Cat "I"]);
([],[Sel "Vt";Sel "D";Sel "D";Cat "Pred"]);
([],[Sel "V";Sel "D";Cat "Pred"]);
(["praise"],[Cat "Vt";Neg "Infl"]);
(["laugh"],[Cat "V";Neg "Infl"]);
(["lavinia"],[Cat "D"]);
(["titus"],[Cat "D"]);
]

let (startCat:string) = "C"
