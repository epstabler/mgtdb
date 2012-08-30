(*   file: dt2bts.ml
  creator: E Stabler, stabler@ucla.edu
creation date: 2010-09-17 10:30:06 PDT
   purpose: dt is first mapped to (state, bool, baretrees) triples,
            where the length of state = length of baretrees,
            and bool is true iff tree is lexical.
            Then the baretrees can be projected out.

This file is one step from dt2st.ml and dtState.ml, but requires more cases!

* Tuple based grammars like the one in Stabler 2001 categorize strings
  with feature lists, but in bar trees the feature lists are at the
  heads. Deleting features thus requires some minor tree surgery --
  violating Chomsky's "extension condition" -- to delete features.
  This surgery is done by the function checkHead.

*)
open Tktree
open Mgtdbp

(* now for bare trees *)
type bareTree = 
  | Lex of string list * feature list
  | LT of bareTree * bareTree
  | GT of bareTree * bareTree
  | Trace

let rec bt2t t = match t with
    Lex (string,fs) -> T("("^(spacedstring2 string)^","^stringFs fs^")",[])
  | LT (t1,t2) -> T("<",[bt2t t1;bt2t t2])
  | GT (t1,t2) -> T(">",[bt2t t1;bt2t t2])
  | Trace -> T("t",[])

(* dtState defines selectUnique, which we here extend to selectUniqueTriple,
   which returns a triple (newMovers,newTrees,MovingTree).
   This is almost identical to selectUnique3 in dt2mcfg.ml
   except that Trace is not a string *)
let rec checkHead = function
  | Lex (s,_::fs) -> Lex (s,fs)
  | LT (bt1,bt2) -> LT(checkHead bt1,bt2)
  | GT (bt1,bt2) -> GT(bt1,checkHead bt2)
  | Trace -> failwith "checkHead error1"
  | Lex (_,[]) -> failwith "checkHead error2"

let rec selectUniqueTriple f movers trees = match (movers,trees) with
  | ((Neg g::fs)::more,t::ts) when f=g -> 
      if noOther f more  (* SMC *)
      then (
	if fs=[]
	then (more,ts,checkHead t)               (* move 1 *)
	else (fs::more,(checkHead t)::ts,Trace)    (* move 2 *)
      )  
      else failwith "SMC violation"
  | (fs::more,t::ts) -> 
    let (newMovers,newTrees,movingTree) = selectUniqueTriple f more ts in
      (fs::newMovers,t::newTrees,movingTree)
  | (_,_) -> failwith "move error in selectUniqueTriple"

let rec dt2bts0 = function
  | L (strings,fs) -> ([fs],true,[Lex (strings,fs)])
  | X (d1,d2) ->
    let (fs1,lex1,bt1,bts1) = splitThird (dt2bts0 d1) in
    let (fs2,_,bt2,bts2) = splitThird (dt2bts0 d2) in
      (	match (fs1,lex1,fs2) with
	  (((Sel f1)::fs1)::movers1,true,((Cat f2)::fs2)::movers2) when f1=f2 -> 
	    if fs2 = []
	    then (fs1::movers1@movers2,false,LT(checkHead bt1,checkHead bt2)::bts1@bts2)  (* merge 1 *)
	    else (fs1::fs2::movers1@movers2,false,LT(checkHead bt1,Trace)::checkHead bt2::bts1@bts2)     (* merge 3 *)
	| (((Sel f1)::fs1)::movers1,false,((Cat f2)::fs2)::movers2) when f1=f2 -> 
	  if fs2 = []
	  then (fs1::movers1@movers2,false,GT(checkHead bt2,checkHead bt1)::bts1@bts2)                   (* merge 2 *)
	  else (fs1::fs2::movers1@movers2,false,GT(Trace,checkHead bt1)::checkHead bt2::bts1@bts2)       (* merge 3 *)
	| (_,_,_) -> failwith "dt2bts0 merge error"
      )
  | O (d) -> 
    let (fs,_,bt,bts) = splitThird (dt2bts0 d) in
      match fs with
	  ((Pos f)::fs)::movers -> 
	    let (newMovers,newTrees,movingTree) = selectUniqueTriple f movers bts in
	      (fs::newMovers,false, GT(movingTree,checkHead bt)::newTrees)
	| _ -> failwith "dt2bts0 move error"

let dt2bts dt = let (_,_,bts) = dt2bts0 dt in bts

let dt2bt dt = 
  let (_,_,bts) = dt2bts0 dt in
    match bts with
	t::[] -> t
      | _ -> failwith "dt2bt error: unique tree not found"
