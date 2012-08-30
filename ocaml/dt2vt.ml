(*  file: dt2vt.ml
 creator: E Stabler, stabler@ucla.edu
creation date: 2010-09-17 10:30:06 PDT
    purpose: derivation tree to value tree

requires dtState.ml for selectUnique
*)
open Tktree
open Mgtdbp

type expr = (string list * feature list * int) * ((string list * feature list) list)

let rec head2string (string,features,lexical) =
  "("^(spacedstring2 string)^","^(stringFs features)^","^(string_of_int lexical)^")"

let rec mover2string (string,features) = "("^(spacedstring2 string)^","^(stringFs features)^")"

let rec movers2string movers = 
  List.fold_right (function x -> (function y -> (mover2string x)^y)) movers ""

let rec expr2string (h,m) = (head2string h)^(movers2string m)

type valueTree = 
  | Lv of expr
  | Xv of expr * valueTree * valueTree
  | Ov of expr * valueTree

let vtRoot = function Xv (e,_,_) -> e | Ov (e,_) -> e | Lv e -> e

(* value tree to tree *)
let rec vt2t = function
  | Lv e ->  T(expr2string e,[])
  | Xv (e,d1,d2) -> T (expr2string e,[vt2t d1;vt2t d2])
  | Ov (e,d) -> T (expr2string e,[vt2t d])

(* to define move, this auxiliary function to enforce the SMC *)
let rec noOtherMover f = function
  | [] -> true
  | (_,Neg g::_)::_ when f=g -> false
  | _::more -> noOtherMover f more

(* to define move, this deletes Neg f and, if smc is respected,
   returns (mover, list of remainingMovers) *)
let rec selectUniqueMover f = function
  | [] -> failwith "move error in selectUnique"
  | (s,Neg g::fs)::more when f=g -> 
      if noOtherMover f more  (* SMC *)
      then ((s,fs),more)  (* move 1 and 2 *)
      else failwith "SMC violation"
  | (s,fs)::more ->
    let (mover,remainingMovers) = selectUniqueMover f more in
      (mover,(s,fs)::remainingMovers)

let rec dt2vt = function
  | L (s,fs) -> Lv ((s,fs,1),[])
  | X (d1,d2) -> 
    let vt1 = dt2vt d1 in
    let vt2 = dt2vt d2 in
    let ((s1,feats1,i1),m1) = vtRoot vt1 in
    let ((s2,feats2,_),m2) = vtRoot vt2 in
      (	match (feats1,feats2,i1) with
	  ((Sel f1)::fs1,(Cat f2)::fs2,1) when f1=f2 && fs2=[] && i1=1 -> 
	    Xv (((s1@s2,fs1,0),m1@m2),vt1,vt2)                 (* merge 1 *)
	| ((Sel f1)::fs1,(Cat f2)::fs2,0) when f1=f2 && fs2=[] -> 
	  Xv (((s2@s1,fs1,0),m1@m2),vt1,vt2)                   (* merge 2 *)
	| ((Sel f1)::fs1,(Cat f2)::fs2,_) when f1=f2 -> 
	  Xv (((s1,fs1,0),(s2,fs2)::m1@m2),vt1,vt2)            (* merge 3 *)
	| _ -> failwith "dt2vt merge error"
      )
  | O (d) -> 
    let vt1 = dt2vt d in
    let e = vtRoot vt1 in
      match e with
	  ((s1,(Pos f)::fs1,_),m1) ->
	    let ((s2,fs2),remainingMovers) = selectUniqueMover f m1 in
	      if fs2=[]
	      then Ov (((s2@s1,fs1,0),remainingMovers),vt1)
	      else Ov (((s1,fs1,0),(s2,fs2)::remainingMovers),vt1)
	| _ -> failwith "dt2vt move error" (* e.g. if e has no features *)
