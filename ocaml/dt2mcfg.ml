(*         file: dt2mcfg.ml
        creator: E Stabler, stabler@ucla.edu
  creation date: 2010-09-17 10:30:06 PDT
        purpose: dt is mapped to mcfgTree,
                 where MG state tuple + string tuple label internal nodes

* To handle head movement, tuple based grammars like the one in Stabler 2001
  separate the pronounced head from the strings to the left and right,
  so the first 3 string lists are for the head of the phrase.
  We use that strategy here.

requires noOther
*)
open Tktree
open Mgtdbp

type mcfgTree = 
  | Lf of feature list list * bool * string list list
  | Xf of feature list list * bool * string list list * mcfgTree * mcfgTree
  | Of of feature list list * bool * string list list * mcfgTree

let stringBool = function true -> "1" | false -> "0"

let rec stringSSs2 = function
  | [] -> ""
  | s::[] -> spacedstring2 s
  | s1::s2::more -> (spacedstring2 s1)^","^(stringSSs2 (s2::more))

let rec mcfg2t t = match t with
    Lf (ffs,lex,strings) ->
      T((stringFFs ffs)^(stringBool lex)^"("^(stringSSs2 strings)^")",[])
  | Xf (ffs,lex,strings,t1,t2) ->
      T((stringFFs ffs)^(stringBool lex)^"("^(stringSSs2 strings)^")",[mcfg2t t1;mcfg2t t2])
  | Of (ffs,lex,strings,t1) ->
      T((stringFFs ffs)^(stringBool lex)^"("^(stringSSs2 strings)^")",[mcfg2t t1])

let mcfgtLabel = function
  | Lf (ffs,lex,s::strings) -> (ffs,lex,s,strings)
  | Xf (ffs,lex,s::strings,_,_) -> (ffs,lex,s,strings)
  | Of (ffs,lex,s::strings,_) -> (ffs,lex,s,strings)
  | _ -> failwith "mcfgtLabel error"

(* dtState defines selectUnique, which we here extend to selectUnique3,
   which returns a triple (newMovers,newTrees,MovingString).
   This is almost identical to selectUniqueTriple in dt2bts.ml,
   except that MovingString is not a baretree
*)
let rec selectUnique3 f movers trees = match (movers,trees) with
    ((Neg g::fs)::more,t::ts) when f=g -> 
      if noOther f more  (* SMC *)
      then (
	if fs=[]
	then (more,ts,t)               (* move 1 *)
	else (fs::more,t::ts,[])       (* move 2 *)
      )  
      else failwith "SMC violation"
  | (fs::more,t::ts) -> 
    let (newMovers,newTrees,movingTree) = selectUnique3 f more ts in
      (fs::newMovers,t::newTrees,movingTree)
  | (_,_) -> failwith "move error in selectUnique3"

let rec dt2mcfg dt = match dt with
    L (strings,fs) -> Lf ([fs],true,[strings])
  | X (d1,d2) ->
    let t1 = dt2mcfg d1 in
    let t2 = dt2mcfg d2 in
    let (fs1,lex1,s1,ss1) = mcfgtLabel t1 in
    let (fs2,   _,s2,ss2) = mcfgtLabel t2 in
      (	match (fs1,lex1,fs2) with
	  (((Sel f1)::fs1)::movers1,true,((Cat f2)::fs2)::movers2) when f1=f2 -> 
	    if fs2 = []
	    then Xf (fs1::movers1@movers2,false,(s1@s2)::ss1@ss2,t1,t2)             (* merge 1 *)
	    else Xf (fs1::fs2::movers1@movers2,false,s1::s2::ss1@ss2,t1,t2)        (* merge 3 *)
	| (((Sel f1)::fs1)::movers1,false,((Cat f2)::fs2)::movers2) when f1=f2 -> 
	  if fs2 = []
	  then Xf (fs1::movers1@movers2,false,(s2@s1)::ss1@ss2,t1,t2)               (* merge 2 *)
	  else Xf (fs1::fs2::movers1@movers2,false,s1::s2::ss1@ss2,t1,t2)         (* merge 3 *)
	| (_,_,_) -> failwith "dt2mcfg merge error"
      )
  | O (d) -> 
    let t = dt2mcfg d in
    let (fs1,_,s1,ss1) = mcfgtLabel t in
      match fs1 with
	  ((Pos f)::fs)::movers -> 
	    let (newMovers,newStrings,movingString) = selectUnique3 f movers ss1 in
	      Of (fs::newMovers,false,(movingString@s1)::newStrings,t)
	| _ -> failwith "dt2mcfg move error"
