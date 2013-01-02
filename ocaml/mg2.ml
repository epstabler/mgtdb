(*   File   : mg2.ml
     Author : E Stabler
    Updated: June 2012

    "these PL KING PRES PREFER this SG BEER"
with adjunction:
    "these PL KING PRES PREFER this SG DARK BEER"
    "these PL KING PRES PREFER this SG DARK DARK BEER"
    "these PL KING PRES PREFER this SG DARK BEER on SUNDAY"
*)
open Mgtdbp (* for definition of grammar type *)

let (g0:g) =[
([],[Sel "T";Cat "C"]);
([],[Sel "T";Pos "wh";Cat "C"]);
(["PRES"],[Sel "v";Pos "eppD";Cat "T"]);
([],[Sel "V";Sel "D";Cat "v"]);        (* "little v" introduces the subject *)
(["PREFER"],[Sel "D";Cat "V"]);
(["KNOW"],[Sel "C";Cat "V"]);
(["LAUGH"],[Cat "V"]);
(["which"],[Sel "Num";Cat "D";Neg "wh"]);
(["SG"],[Sel "N";Cat "Num"]);
(["PL"],[Sel "N";Cat "Num"]);
(["KING"],[Cat "N"]);
(["QUEEN"],[Cat "N"]);
(["WINE"],[Cat "N"]);
(["BEER"],[Cat "N"]);             (* Neg "D" should always be available for epp *) 
(["this"],[Sel "Num";Cat "D"]);   (["this"],[Sel "Num";Cat "D";Neg "eppD"]);
(["these"],[Sel "Num";Cat "D"]);  (["these"],[Sel "Num";Cat "D";Neg "eppD"]);
(["MARY"],[Cat "D"]);             (["MARY"],[Cat "D";Neg "eppD"]);
(["JOHN"],[Cat "D"]);             (["JOHN"],[Cat "D";Neg "eppD"]);
(["DENMARK"],[Cat "D"]);          (["DENMARK"],[Cat "D";Neg "eppD"]);
(["SUNDAY"],[Cat "D"]);           (["SUNDAY"],[Cat "D";Neg "eppD"])
]

let (startCat:string) = "C"

(*
(["REALLY"],[LAdj "v"]);
(["RED"],[LAdj "N"]);
(["WHITE"],[LAdj "N"]);
(["DARK"],[LAdj "N"]);
([with],[Sel "D",RAdj "N"]);
([from],[Sel "D",RAdj "N"]);
([on],[Sel "D",LAdj "T"]);
([on],[Sel "D",RAdj "T"]);
([tomorrow],[RAdj "T"]);

epp("D").
startCategory("C").
*)

