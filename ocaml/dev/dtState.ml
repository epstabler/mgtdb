(*         file: dtState.ml
        creator: E Stabler, stabler@ucla.edu
  creation date: 2010-09-17 10:30:06 PDT
        purpose: check MG derivations

This is a finite state frontier-to-root tree automaton whose states are sequences of feature sequences.
dtState maps each well-formed derivation tree to its "state", a feature list list.
The initial states are the singleton lists [fs] such that L(string,fs) is in Lex.
ACCEPT iff state = [[Cat c]] for start category c.
This can be checked if startCat is defined -- see def of dtAccepted below.
*)

type stateTree = 
    LS of feature list list
  | XS of feature list list * stateTree * stateTree
  | OS of feature list list * stateTree;;

(* to define move, this auxiliary function to enforce the SMC *)
let rec noOther f movers = match movers with
    [] -> true
  | (Neg g::_)::_ when f=g -> false
  | _::more -> noOther f more;;

(* to define move, this deletes Neg f and, if smc is respected,
   returns the list newMovers *)
let rec selectUnique f movers = match movers with
    [] -> failwith "move error in selectUnique"
  | (Neg g::fs)::more when f=g -> 
      if noOther f more  (* SMC *)
      then (if fs=[] then more else fs::more)  (* move 1 and 2, respectively *)
      else failwith "SMC violation"
  | fs::more -> fs::selectUnique f more;;

let rec dtState dt = match dt with
    L (_,fs) -> [fs]  (* we ignore first string argument of leaf *)
  | X (d1,d2) ->
      (	match (dtState d1,dtState d2) with
	    (((Sel f1)::fs1)::movers1,((Cat f2)::fs2)::movers2) when f1=f2 -> 
	      if fs2 = []
	      then fs1::movers1@movers2                 (* merge 1 and 2 *)
	      else fs1::fs2::movers1@movers2            (* merge 3 *)
	  | _ -> failwith "merge error"
      )
  | O (d) -> 
      match dtState d with
	  ((Pos f)::fs)::movers -> fs::selectUnique f movers
	| _ -> failwith "move error";;

let dtAccepted dt = match dtState dt with
(*  ((Cat x)::[])::[] when (startCat x) -> true  use this line if startCat is defined*)
    ((Cat _)::[])::[] -> true
  | _ -> false;;
