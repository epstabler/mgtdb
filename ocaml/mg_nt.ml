(*  File   : g-nt.pl - naive Tamil SOVI
    Author : E Stabler
    Updated: Mar 00
*)
open Mgtdbp (* for definition of grammar type *)

(* example: 
"titus lavinia praise -s"
etc, with startCat="C"*)

let (g0:g) = [
([],[Sel "I";Cat "C"]);
(["-s"],[Sel "Pred";Pos "infl";Cat "I"]);
([],[Sel "Vt";Sel "D";Sel "D";Cat "Pred";Neg "infl"]);
([],[Sel "V";Sel "d";Cat "Pred";Neg "infl"]);
(["praise"],[Cat "Vt"]);
(["laugh"],[Cat "V"]);
(["lavinia"],[Cat "D"]);
(["titus"],[Cat "D"])
]

let (startCat:string) = "C"
