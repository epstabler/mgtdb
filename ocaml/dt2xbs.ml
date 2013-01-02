(*   file: dt2xbs.ml
  creator: E Stabler, stabler@ucla.edu
creation date: 2010-09-17 10:30:06 PDT
   purpose: dt is first mapped to (state, bool, xbtrees) triples,
            where the length of state = length of xbtrees,
            and bool is true iff tree is lexical.
            Then the xbtrees can be projected out.

This file is similar to dt2bts.ml, but requires more cases.
This is the most complex translation in the toolset.
I have tried to label the cases carefully, and the type constructors help a lot!

requires noOther
requires splitThird
*)
open Tktree
open Mgtdbp

type traceIndex = Nx | Ix of int
type xbTree =   (* for branching categories, we use bool first? to tell if head is first subtree *)
    X01 of string * string list                      (* (cat,ph) *)
  | X11 of string * xbTree                           (* (cat,subtree) for non-branching X' *)
  | X12 of string * bool * xbTree * xbTree           (* (cat,first?,subtree1,subtree2) for branching X' *)
  | XP0 of string * traceIndex * string list              (* (cat,tindex,ph) for 0-ary XP = traces, lexical XPs *)
  | XP1 of string * traceIndex * xbTree                   (* (cat,tindex,subtree) for unary XP *)
  | XP2 of string * traceIndex * bool * xbTree * xbTree   (* (cat,tindex,first?,subtree1,subtree2) for binary XP *)

let rec xb2t = function
  | X01 (cat,string) -> T(cat,[T(spacedstring2 string,[])])
  | X11 (cat,t1) -> T(cat^"'",[xb2t t1])
  | X12 (cat,_,t1,t2) -> T(cat^"'",[xb2t t1;xb2t t2])
  | XP0 (cat,Nx,string) -> T(cat^"P",[T(joinStrings "" string,[])])
  | XP0 (cat,Ix n,string) -> T(cat^"P("^(string_of_int n)^")",[T(joinStrings "" string,[])])
  | XP1 (cat,Nx,t1) -> T(cat^"P",[xb2t t1])
  | XP1 (cat,Ix n,t1) -> T(cat^"P("^(string_of_int n)^")",[xb2t t1])
  | XP2 (cat,Nx,_,t1,t2) -> T(cat^"P",[xb2t t1;xb2t t2])
  | XP2 (cat,Ix n,_,t1,t2) -> T(cat^"P("^(string_of_int n)^")",[xb2t t1;xb2t t2])

(* dtState defines selectUnique, which we here extend to selectUniqueTriple,
   which returns a triple (newMovers,newTrees,MovingTree).
   This is almost identical to selectUnique3 in dt2mcfg.ml
   except that Trace is not a string *)

let traceOf = function
  | XP0(cat,Ix i,_) -> XP0(cat,Ix i,[])
  | XP1(cat,Ix i,_) -> XP0(cat,Ix i,[])
  | XP2(cat,Ix i,f,t1,t2) -> XP0(cat,Ix i,[])
  | _ -> failwith "traceOf error"

let indexOf = function
  | XP0(cat,i,st) -> i
  | XP1(cat,i,t) -> i
  | XP2(cat,i,f,t1,t2) -> i
  | _ -> failwith "indexOf error"

let rec selectUniqueTripleX f movers trees = match (movers,trees) with
  | ((Neg g::fs)::more,t::ts) when f=g -> 
      if noOther f more  (* SMC *)
      then (
	if fs=[]
	then (more,ts,t)                  (* move 1 *)
	else (fs::more,t::ts,traceOf t)   (* move 2 *)
      )  
      else failwith "SMC violation"
  | (fs::more,t::ts) -> 
    let (newMovers,newTrees,movingTree) = selectUniqueTripleX f more ts in
      (fs::newMovers,t::newTrees,movingTree)
  | (_,_) -> failwith "move error in selectUniqueTripleX"

let rec catOfFs = function
  | Cat x::_ -> x
  | _::fs -> catOfFs fs
  | _ -> failwith "catOfFs error"

(* we use an object to provide "fresh" indices wherever we need them *)
class gensym =
object
  val mutable x = 0
  method next = (x<-x+1;x)
  method reset = (x<-0)
end

let myindex = new gensym (* myindex#next provides new value *)

let insertIndex i = function
  | XP0(cat,Nx,st) -> XP0(cat,Ix i,st)
  | XP1(cat,Nx,t) -> XP1(cat,Ix i,t)
  | XP2(cat,Nx,f,t1,t2) -> XP2(cat,Ix i,f,t1,t2)
  | _ -> failwith "insertIndex error"

let rec dt2xbs0 = function
  | L (strings,Cat cat::fs) -> 
	([Cat cat::fs],true,[XP0(cat,Nx,strings)])
  | L (strings,fs) -> 
	([fs],true,[X01(catOfFs fs,strings)])
  | X (d1,d2) ->
    let (fs1,lex1,xb1,xbs1) = splitThird (dt2xbs0 d1) in
    let (fs2,_,xb2,xbs2) = splitThird (dt2xbs0 d2) in
      (	match (fs1,lex1,fs2) with

	  (((Sel f1)::Cat c::fs1)::movers1,true,((Cat f2)::fs2)::movers2) when f1=f2 -> 
	    if fs2 = []
	    then ((Cat c::fs1)::movers1@movers2,
		  false,
		  XP2(c,Nx,true,xb1,xb2)::xbs1@xbs2) (* merge 1, XP *)
	    else 
	      let i = myindex#next in 
	      let newxb2 = insertIndex i xb2 in
		((Cat c::fs1)::fs2::movers1@movers2,
		 false,
		 XP2(c,Nx,true,xb1,XP0(f2,Ix i,[]))::newxb2::xbs1@xbs2) (* merge 3, XP *)
	| (((Sel f1)::Cat c::fs1)::movers1,false,((Cat f2)::fs2)::movers2) when f1=f2 -> 
	  if fs2 = []
	  then ((Cat c::fs1)::movers1@movers2,
		false,
		XP2(c,Nx,false,xb2,xb1)::xbs1@xbs2) (* merge 2, XP *)
	  else 
	    let i = myindex#next in 
	    let newxb2 = insertIndex i xb2 in
	      ((Cat c::fs1)::fs2::movers1@movers2,
	       false,
	       XP2(c,Nx,false,XP0(f2,Ix i,[]),xb1)::newxb2::xbs1@xbs2) (* merge 3, XP *)

	| (((Sel f1)::fs1)::movers1,true,((Cat f2)::fs2)::movers2) when f1=f2 -> 
	  if fs2 = []
	  then (fs1::movers1@movers2,
		false,
		X12(catOfFs fs1,true,xb1,xb2)::xbs1@xbs2)  (* merge 1, X' *)
	  else 
	    let i = myindex#next in 
	    let newxb2 = insertIndex i xb2 in
	      (fs1::fs2::movers1@movers2,
	       false,
	       X12(catOfFs fs1,true,xb1,XP0(f2,Ix i,[]))::newxb2::xbs1@xbs2) (* merge 3, X' *)
	| (((Sel f1)::fs1)::movers1,false,((Cat f2)::fs2)::movers2) when f1=f2 -> 
	  if fs2 = []
	  then (fs1::movers1@movers2,
		false,
		X12(catOfFs fs1,false,xb2,xb1)::xbs1@xbs2) (* merge 2, X' *)
	  else 
	    let i = myindex#next in 
	    let newxb2 = insertIndex i xb2 in
	      (fs1::fs2::movers1@movers2,
	       false,
	       X12(catOfFs fs1,false,XP0(f2,Ix i,[]),xb1)::newxb2::xbs1@xbs2) (* merge 3, X' *)

	| (_,_,_) -> failwith "dt2xbs0 merge error"
      )
  | O (d) -> 
    let (fs,_,xb,xbs) = splitThird (dt2xbs0 d) in
      match fs with
	  ((Pos f)::Cat c::fs)::movers -> 
	    let (newMovers,newTrees,movingTree) = selectUniqueTripleX f movers xbs in
	      ((Cat c::fs)::newMovers,false,XP2(c,Nx,false,movingTree,xb)::newTrees)      (* move, XP *)
	| ((Pos f)::fs)::movers -> 
	    let (newMovers,newTrees,movingTree) = selectUniqueTripleX f movers xbs in
	      (fs::newMovers,false,X12(catOfFs fs,false,movingTree,xb)::newTrees)        (* move, X' *)
	| fs::_ -> failwith ("dt2xbs0 move error: "^stringFs fs)
	| [] -> failwith "dt2xbs0 move error"

let dt2xbs dt = let (_,_,xbs) = (myindex#reset; dt2xbs0 dt;) in xbs

let dt2xb dt = 
  let (_,_,xbs) = (myindex#reset; dt2xbs0 dt;) in
    match xbs with
	t::[] -> t
      | _ -> failwith "dt2xb error: unique tree not found"
