(*   File   : mg_ne.pl - naive english
   Author : E Stabler
   Updated: Mar 00
*)
open Mgtdbp (* for definition of grammar type *)

(* example: 
"titus praise -s lavinia"
"who titus praise -s"
etc, with startCat="C"*)

let (g0:g) =[
([],[Sel "I";Cat "C"]);
([],[Sel "I";Pos "wh";Cat "C"]);
(["-s"],[Sel "Pred";Pos "infl";Pos "k";Cat "I"]);
([],[Sel "Vt";Pos "k";Sel "D";Cat "Pred"]);
([],[Sel "V";Cat "Pred"]);
(["praise"],[Sel "D";Cat "Vt";Neg "infl"]);
(["laugh"],[Sel "D";Cat "V";Neg "infl"]);
(["lavinia"],[Cat "D";Neg "k"]);
(["titus"],[Cat "D";Neg "k"]);
(["who"],[Cat "D";Neg "k";Neg "wh"])
]

let (startCat:string) = "C"
