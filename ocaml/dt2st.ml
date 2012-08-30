(*  file: dt2st.ml
 creator: E Stabler, stabler@ucla.edu
creation date: 2010-09-17 10:30:06 PDT
  purpose: put "states" of dtState into tree

requires dtState.ml for selectUnique
*)
open Tktree
open Mgtdbp

type stateTree = 
    LS of feature list list
  | XS of feature list list * stateTree * stateTree
  | OS of feature list list * stateTree

let stRoot = function XS (fs,_,_) -> fs | OS (fs,_) -> fs | LS fs -> fs

(* convert state tree to generic string tree, e.g. for printing *)
let rec st2t = function
  | LS ffs -> T(stringFFs ffs,[])
  | XS (ffs,t1,t2) -> T(stringFFs ffs,[st2t t1;st2t t2])
  | OS (ffs,t1) -> T(stringFFs ffs,[st2t t1])

let rec dt2st dt = match dt with
    L (_,fs) -> LS ([fs])
  | X (d1,d2) -> 
    let st1 = dt2st d1 in
    let st2 = dt2st d2 in
      (	match (stRoot st1,stRoot st2) with
	    (((Sel f1)::fs1)::movers1,((Cat f2)::fs2)::movers2) when f1=f2 -> 
	      if fs2 = []
	      then XS(fs1::movers1@movers2,st1,st2)                 (* merge 1 and 2 *)
	      else XS(fs1::fs2::movers1@movers2,st1,st2)            (* merge 3 *)
	  | _ -> failwith "dt2st merge error"
      )
  | O (d) -> 
    let st = dt2st d in
      match stRoot st with
	  ((Pos f)::fs)::movers -> OS(fs::selectUnique f movers,st)
	| _ -> failwith "dt2st move error"
