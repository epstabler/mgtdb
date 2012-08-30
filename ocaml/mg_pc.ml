(*   File   : mg_pc.pl - an ambiguous propositional calculus
   Author : E Stabler
   Updated: Jun 12
*)
open Mgtdbp (* for definition of grammar type *)

(* example: "not p" "not p or q" *)

let (g0:g) =[
(["p"],[Cat "wff"]);
(["q"],[Cat "wff"]);
(["r"],[Cat "wff"]);
(["s"],[Cat "wff"]);
(["t"],[Cat "wff"]);
(["not"],[Sel "wff";Cat "wff"]);
(["and"],[Sel "wff";Sel "wff";Cat "wff"]);
(["or"],[Sel "wff";Sel "wff";Cat "wff"]);
(["implies"],[Sel "wff";Sel "wff";Cat "wff"]);
]

let (startCat:string) = "wff"
