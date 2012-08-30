(* mgtdbp-dev.ml  comments welcome: stabler@ucla.edu

   This file extends mgtdb-dev.ml to a parser, by keeping a tree in each partial analysis.
   Note that, although this is a TD parser, derivation nodes are not expanded left-to-right,
     so we record their positions with indices (similar to indexing of predicted cats).
     To each indexed category (iCat) we add its own dtree node index,
        and we also add a list of the features checked in its own projection so far.
     To each derivation (der) we add its list of indexed category dtree node indices.
     In each step of the derivation, we extend the parents node index,
        putting the results into the derivation list, and in the respective children.
   For the moment, we compute just the most probable parse, returning its 
        dnode list or error instead of just true or false as the recognizer does.
   TODO: implement more sophisticated pruning rule (cf Roark) and more sophisticated
        determination of which trees should be returned.
   This is a working version, with print routines and examples.
   Load with: #use "mgtdb-dev.ml";;

To get the parse from the rules, I modified them so that instead of returning
  (remaining_input, list_of_new_predictions),
it returns the input that the rule consumed (which is zero in all cases except scan):
  (input_w_consumed, list_of_new_predictions).
To accommodate that change, we pass the input as an argument to insertNewParses.

   * For cats that lack subtrees in lex, tA is not set. This does not matter,
     but we could probably get rid of tA altogether.
   * sA sets all features, but lA only needs non-empty lexTrees.
   * We might speed things up by numbering all subtrees of the lexArray,
     and using int list arrays to encode the whole lexTree.

*)

(* string library for Str.regexp -> string -> string list = <fun>, used by process *)
#load "str.cma";;
(* labltk for tree display *)
#directory "+labltk";;
#load "labltk.cma";;
#use "tktree.ml";;

open Str 
exception Not_Accepted

let pptree tree =
  let rec tab n = if n <= 0 then () else (Printf.fprintf stdout " "; tab (n-1);) in
  let rec pptree_aux n = function T(s,ts) -> 
    ( Printf.fprintf stdout "\n"; tab n; Printf.fprintf stdout "T(\"%s\",[" s; 
      List.iter (pptree_aux (n+4)) ts; Printf.fprintf stdout "]);";
    ) in pptree_aux 0 tree;;
let pptrees = List.iter pptree;;

(* REPRESENTING GRAMMAR FOR HUMANS *)
type feature = Sel of string | Cat of string | Neg of string | Pos of string
type lexItem = string list * feature list
type g = lexItem list

(* example *)
let (mg0:g) =[ 
([],[Sel "V"; Cat "C"]);                  ([],[Sel "V"; Pos "wh"; Cat "C"]);
(["the"],[Sel "N"; Cat "D"]);             (["which"],[Sel "N"; Cat "D"; Neg "wh"]);
(["king"],[Cat "N"]);                     (["queen"],[Cat "N"]);
(["wine"],[Cat "N"]);                     (["beer"],[Cat "N"]);
(["knows"],[Sel "C"; Sel "D"; Cat "V"]);  (["says"],[Sel "C"; Sel "D"; Cat "V"]);
(["prefers"],[Sel "D"; Sel "D"; Cat "V"]);(["drinks"],[Sel "D"; Sel "D"; Cat "V"]);
];;
let startCat0 = "C";;

(* BEAUTIFY GRAMMAR: for practice and for tracing later *)
let btfyFeat: feature -> string = 
  function Sel x -> "="^x | Cat x -> x | Pos x -> "+"^x | Neg x -> "-"^x;;

(* example: btfyFeat (Sel "D");; *)

let rec joinStrings sep = function  (* join is builtin in many programming langs *)
  | [] -> ""
  | s::[] -> s
  | s::t::more -> s^sep^(joinStrings sep (t::more))

let rec stringFs fs = joinStrings " " (List.map btfyFeat fs);;
let rec stringFFs ffs = joinStrings "," (List.map stringFs ffs);;
let spacedstring2 = function [] -> "e" | l -> joinStrings " " l;;

let btfyLexItem: lexItem -> string = fun (ss,fs) ->
  (joinStrings " " ss)^"::"^(joinStrings " " (List.map btfyFeat fs));;

(* example: btfyLexItem (["prefers"], [Sel "D"; Sel "D"; Cat "V"]);; *)

let showGrammar: g -> unit =
  List.iter (fun x -> print_string (btfyLexItem x); print_string "\n");;

(* example: showGrammar mg0;; *)

(* REPRESENTING GRAMMAR WITH A TREE FOR THE RECOGNIZER *)
type ifeature = int * int  (* type, value *)
type iLexItem = string list * ifeature list
type lexTree = W of string list | N of (ifeature * lexTree list)
type sArray = string array (* string values of the features *)
type tArray = int array (* the types of each element of lexArray roots *)
type lexArray = lexTree list array (* lexTrees *)
type lexArrays = sArray * lexArray * tArray

(* TYPES FOR INDEXED CONSTITUENTS *)
type movers = lexTree list array (* NB: same as lexArray *)
type cat = lexTree list * movers (* intuitively:lexTree list=children of head *)
type ix = int list
(* derivation trees *)
type dnode = Nd of ix  | Ld of (ix * iLexItem);; (* for each step of each derivation *)
type dtuple = (ifeature list * ix * ifeature list array)
type idtree =  Li of string list * ifeature list | Xi of idtree * idtree | Oi of idtree;;
type derivationTree = 
    L of string list * feature list
  | X of derivationTree * derivationTree
  | O of derivationTree;;

type iCat = cat * (ix * ix array) * dtuple

(*** MakeQ priority queue functor
creator: Jean-Christophe.Filliatre@lri.fr
source: https://groups.google.com/group/fa.caml/msg/526cc7a4a9adc664?dmode=source
  date: 30 Jun 2011 20:14:34 +0200
****)
module MakeQ (X : sig type t val le : t -> t -> bool end) :
sig
  type t
  val empty : t
  val is_empty : t -> bool
  val add : X.t -> t -> t
  exception Empty
  val extract_min : t -> X.t * t
(*  val merge : t -> t -> t *)
(*  val size : t -> int *)
(*  val listOf : t -> X.t list *)
end
  =  struct
    type t = E | T of int * X.t * t * t
    exception Empty
    let rank = function E -> 0 | T (r,_,_,_) -> r
    let make x a b =
      let ra = rank a and rb = rank b in
	if ra >= rb then T (rb + 1, x, a, b) else T (ra + 1, x, b, a)
    let empty = E
    let is_empty = function E -> true | T _ -> false
    let rec merge h1 h2 = match h1,h2 with
      | E, h | h, E -> h
      | T (_,x,a1,b1), T (_,y,a2,b2) ->
	if X.le x y then make x a1 (merge b1 h2) else make y a2 (merge h1 b2)
    let add x h = merge (T (1, x, E, E)) h
    let extract_min = function
      | E -> raise Empty
      | T (_,x,a,b) -> x, merge a b
    let rec size = function
      | E -> 0
      | (T(_,_,a,b)) -> 1 + size a + size b
    let listOf = 
      let rec listOf0 l = function
	| E -> l
	| (T(_,e,a,b)) -> listOf0 (listOf0 (e::l) a) b
      in fun q -> listOf0 [] q
  end;;
(*** END MakeQ ****)

(* minIndex should only check indices of filled mover positions.
   No mover has an empty index, so we can ignore them. *)
let minIndex sofar =
  Array.fold_left (fun x y -> if y<>[] then (min x y) else x) sofar;;

(* example: minIndex [1] [|[0];[]|];;  (* this is [0] *) *)

(* for queues of predictions *)
module ICatPoset = 
  struct type t = iCat
       let le = fun ((_,(i1,a1),_):iCat) ((_,(i2,a2),_):iCat) -> 
	 compare (minIndex i1 a1) (minIndex i2 a2) < 1
end;;

module IQ = MakeQ (ICatPoset);;    (* for the queues of predictions *)

(* split into (nonterminals,terminals)
     removing the type constructors and sorting each after reversing the indices.
   The indices are kept in reverse order in dq, since they are easier to build that way,
     and they are never needed by the recognizer itself. We reverse them just when
     we want to construct at tree to look at or interpret *)
let rec splitDnodes: 
    (int list list * (int list * iLexItem) list) -> dnode list -> 
    (int list list * (int list * iLexItem) list) = fun (n,t) -> function 
    | [] -> List.sort compare n,List.sort compare t
    | Nd i::ts -> splitDnodes (List.rev i::n,t) ts
    | Ld (i,lex)::ts -> splitDnodes (n,(List.rev i,lex)::t) ts;;

let rec child = function 
  | ([],_::[]) -> true
  | (x::xs,y::ys) when x=y -> child (xs,ys)
  | _ -> false;;
  
let rec buildIDtreeFromDnodes: int list -> (int list list * (int list * iLexItem) list)
    -> idtree * int list list * (int list * iLexItem) list = fun parent -> function
  | (i0::nonterms,terms) when child (parent,i0) -> 
    let (child0,nts0,ts0) = buildIDtreeFromDnodes i0 (nonterms,terms) in 
      ( match (nts0,ts0) with
	| (i1::nts,ts) when child (parent,i1) -> 
	  let (child1,nts1,ts1) = buildIDtreeFromDnodes i1 (nts,ts) in 
	    Xi (child0,child1), nts1, ts1
	| _ -> Oi child0, nts0, ts0;
      )
  | (nonterms,(j,(s,f))::terms) when parent=j -> Li (s,f), nonterms, terms
  | _ -> failwith "error: buildIDtreesFromDnodes";;

let dnodes2idtree dns = 
  let (nts,ts) = splitDnodes ([],[]) dns in 
  let (dt,nts1,ts1) = buildIDtreeFromDnodes (List.hd nts) (List.tl nts,ts) in
    if nts1=[] && ts1=[] then dt else failwith "error2: dnodes2idtree";;

(* for the derivation queue *)
type der = string list * IQ.t * float * dnode list;;  (* type of partial derivations *)

module DerivationPoset = 
struct type t = der
  let le = fun ((_,_,p1,_):der) ((_,_,p2,_):der) -> compare p2 p1 < 1
end;;

module DQ = MakeQ (DerivationPoset);;  (* the queue of (partial) derivations *)

(* REPRESENT GRAMMAR AS TREE *)
let stringVal: feature -> string = function
  | Cat s -> s
  | Sel s -> s
  | Pos s -> s
  | Neg s -> s;;

(* (ensureMember e list) adds e to list if not already there *)
let rec ensureMember : 'a -> 'a list -> 'a list = fun e -> function
  | [] -> [e]
  | x::y when x=e -> x::y
  | x::y -> x::ensureMember e y;;

let rec stringValsOfG: string list -> g -> string list = fun sofar -> function
  | (w,f::fs)::more -> 
      stringValsOfG (ensureMember (stringVal f) sofar) ((w,fs)::more)
  | (_,[])::more -> stringValsOfG sofar more
  | [] -> sofar;;
(* example 
let ss0 = stringValsOfG [] mg0;;
let (sA0:sArray) = Array.of_list (stringValsOfG [] mg0);;
*)

let intsOfF: sArray -> feature -> ifeature = fun sA -> 
  let arrayNth: sArray -> string -> int = fun sA s ->
    let rec arrayNth0 i n s =
      if i<n 
      then (if sA.(i)=s then i else arrayNth0 (i+1) n s)
      else failwith ("error: arrrayNth sA "^s) in
      arrayNth0 0 (Array.length sA) s in
    function
      | Cat s -> 0,arrayNth sA s
      | Sel s -> 1,arrayNth sA s
      | Neg s -> 2,arrayNth sA s
      | Pos s -> 3,arrayNth sA s;;

(* example: let startInts0 = intsOfF sA0 (Cat startCat0);; *)

let fOfInts: sArray -> ifeature -> feature = fun sA (i,j) ->
  if i=0 then Cat sA.(j)
  else if i=1 then Sel sA.(j)
  else if i=2 then Neg sA.(j)
  else if i=3 then Pos sA.(j)
  else failwith "error: fOfInts";;
(* example: btfyFeat (fOfInts sA0 (1,2));; *)

(* with features in reverse order, extend lexTrees to encode lexical item *)
let rec revItemIntoLexTrees: sArray -> lexTree list * lexItem -> lexTree list = 
  fun sA -> function
  | [],(w,[]) -> W w::[]
  | [],(w,f::fs) -> N(intsOfF sA f,revItemIntoLexTrees sA ([],(w,fs)))::[]
  | W y::trees,(w,[]) ->
    if w=y
    then W y::trees
    else W y::revItemIntoLexTrees sA (trees,(w,[]))
  | N (ij,ts)::trees,(w,f::fs) -> 
    if intsOfF sA f=ij
    then N (ij,revItemIntoLexTrees sA (ts,(w,fs)))::trees
    else N (ij,ts)::revItemIntoLexTrees sA (trees,(w,(f::fs)))
  | t::trees,(w,fs) -> t::revItemIntoLexTrees sA (trees,(w,fs));;

let rootLabel = function N(f,_) -> f | _ -> failwith "error: rootLabel";;
let subtreesOf = function N(_,ts) -> ts | _ -> failwith "error:subtreesOf";;
let termLabel = function W s -> s | _ -> failwith "error: termLabel";;

(* for print out: *)
let rec lexTree2stringTree: sArray -> lexTree -> tree = fun sA -> function
  | N (ij,ts) -> T (btfyFeat (fOfInts sA ij),lexTrees2stringTrees sA ts)
  | W w -> T (joinStrings " " w,[])
and lexTrees2stringTrees: sArray -> lexTree list -> tree list = fun sA -> function
  | [] -> []
  | t::ts -> lexTree2stringTree sA t::lexTrees2stringTrees sA ts;;

(* to get trees in the array, insert the root feature determined by the index*)
let lexArrays2lexTrees: lexArrays -> lexTree list = fun (sA,lA,tA) ->
  let rec totrees i res =
    if i < 0 then res else totrees (i - 1) (N ((tA.(i),i),lA.(i)) :: res) in
    totrees (Array.length lA - 1) [];;

let lexArrays2stringTrees (sA,lA,tA) =
  lexTrees2stringTrees sA (lexArrays2lexTrees (sA,lA,tA));;
(* example 
revItemIntoLexTrees sA0 ([], (["says"], [Cat "V"; Sel "D"; Sel "C"]));; 
rootLabel (List.nth 
    (revItemIntoLexTrees sA0 ([], (["says"], [Cat "V"; Sel "D"; Sel "C"]))) 0);;
fOfInts sA0 (rootLabel (List.nth 
   (revItemIntoLexTrees sA0 ([], (["says"], [Cat "V"; Sel "D"; Sel "C"]))) 0));;
*)

let gIntoLexArrayTypeArray: sArray -> g -> lexArray * tArray = fun sA g -> 
  let trees = List.fold_left (fun ts (w,fs) -> 
    revItemIntoLexTrees sA (ts,(w,List.rev fs))) [] g in
  let (lA:lexTree list array) = Array.make (Array.length sA) [] in
  let (tA:int array) = Array.make (Array.length sA) 0 in
    ( List.iter (fun t -> 
      if lA.(snd (rootLabel t)) = []
      then ( lA.(snd (rootLabel t)) <- subtreesOf t;
	     tA.(snd (rootLabel t)) <- fst (rootLabel t); )
      else failwith "gIntoLexArray: Cat and Neg values must be disjoint" ) trees;
      (lA,tA); ) ;;
(* example 
let (lA0,tA0) = gIntoLexArrayTypeArray sA0 mg0;;
let lexArrays0 = (sA0,lA0,tA0);;
*)

(* example: display the trees
pptree (T (".", lexArrays2stringTrees lexArrays0));;

#directory "+labltk";;
#load "labltk.cma";;
#use "tktree.ml";;
tktree (T (".", lexArrays2stringTrees lexArrays0));;
*)

(* PRINT FUNCTIONS FOR INDEXED CATEGORIES - optional, only needed for tracing*)
let btfyIndex = List.fold_left (fun l i -> l^(string_of_int i)) "";;

let rec deleteEmpties: tree list * string list -> tree list * string list = function
  | [],[] -> [],[]
  | T(_,[])::ts,_::is -> deleteEmpties (ts,is)
  | t::ts,i::is -> let (us,js) = deleteEmpties (ts,is) in t::us,i::js
  | _ -> failwith "error: deleteEmpties";;

let rec printCatI = function
  | t::ts,i::is -> pptree t ; print_string i; print_string " ,"; printCatI (ts,is)
  | [],[] -> ()
  | _ -> failwith "error: printCatI";;

let printIcat: lexArrays -> iCat -> unit = fun (sA,lA,tA) ((h,mA),(ih,iA),d) ->
  let ihS = btfyIndex ih in   (* d is for dtree, ignored here *) 
  let iASs = List.map btfyIndex  (Array.to_list iA) in
  let hTs =  lexTrees2stringTrees sA h in
  let mTs = lexArrays2stringTrees (sA,mA,tA) in
    ( List.iter pptree hTs; print_string ihS; print_string " ,";
      printCatI (deleteEmpties (mTs,iASs)); print_string "\n"; );;
(* example 
let ic0:iCat = 
  let h = lA0.(0) in  (* the verb subtree *)
  let m = Array.make (Array.length sA0) [] in
  let mx = Array.make (Array.length sA0) [] in
  let dx = ([],[]) in
  let hx = [0;1;1] in
    ( m.(2) <- lA0.(2);   (* the which subtree *)
      mx.(2) <- [0;0];    
      ((h,m),(hx,mx),dx); );;
printIcat lexArrays0 ic0;;
*)

let rec printIQ = fun lexArrays iq0 ->
  if IQ.is_empty iq0 then ()
  else let (ic,iq1) = IQ.extract_min iq0 in 
	 printIcat lexArrays ic; Printf.fprintf stdout " ... end ic";
	 printIQ lexArrays iq1;;
(* example 
let iq0 = IQ.add ic0 IQ.empty;;
let iq0 = IQ.add ic0 iq0;;
printIQ lexArrays0 iq0;;
*)

let rec printDQ lexArrays dq0 =
  if DQ.is_empty dq0 then ()
  else let ((input,iq,p,dt),dq1) = DQ.extract_min dq0 in 
	 Printf.fprintf stdout "%f: [%s]" p (joinStrings "," input);
	 printIQ lexArrays iq; Printf.fprintf stdout " ---- end parse\n";
	 printDQ lexArrays dq1;;
(* example 
let dq0 = DQ.add (["this";"is";"the";"input"],iq0,0.1,[]) DQ.empty;;
printDQ lexArrays0 dq0;;
let dq0 = DQ.add (["this";"is";"the";"input"],iq0,0.1,[]) dq0;;
printDQ lexArrays0 dq0;;
*)

(* RECOGNIZER STEPS: SCAN/UNMERGE/UNMOVE *)
let emptyListArray: lexTree list array -> bool = (* true iff all empty lists *)
  Array.fold_left (fun b x -> (x=[] && b)) true;;

let rec terminalsOf: lexTree list -> (lexTree list * lexTree list) = function
  | [] -> ([],[])
  | (W w)::ts -> let (terms,nonterms) = terminalsOf ts in ((W w)::terms,nonterms)
  | (N p)::ts -> let (terms,nonterms) = terminalsOf ts in (terms,(N p)::nonterms);;

(* we could use exception instead of bool *)
let rec prefixT : (string list * string list) -> (bool * string list) = function
  | ([],remainder) -> (true,remainder)
  | (x::xs,y::ys) when x=y -> prefixT (xs,ys)
  | _ -> (false,[]);;

(* we could use exception instead of bool *)
let rec memberN: int -> lexTree list -> (bool * lexTree) = fun f -> function
    | [] -> (false,N((-1,-1),[]))
    | N ((t,g),ts)::more -> if g=f then (true,N((t,g),ts)) else memberN f more
    | W _::more -> memberN f more;;

let scan : string list -> string list -> lexArray -> ix array -> dtuple ->
  (string list * iCat list) list -> (string list * iCat list) list =
  fun w input m mx dt sofar ->
  if emptyListArray m
  then let (ok,_) = prefixT (w,input) in
	 if ok then (w,[(([],m),([],mx),dt)])::sofar else sofar (* unlike recognizer, return w *)
  else sofar;;

let merge1: lexArray -> string list -> lexTree list -> int -> iCat -> 
  (string list * iCat list) list -> (string list * iCat list) list =
  fun lA input terms i ((_,m),(hx,mx),(ifs,dx,mifs)) sofar ->
    if terms <> []
    then 
      let empty_m: lexTree list array = Array.make (Array.length m) [] in
      let empty_mx: ix array  = Array.make (Array.length mx) [] in
      let empty_mifs: ifeature list array = Array.make (Array.length mx) [] in
	([],[((terms,empty_m),(hx@[0],empty_mx),((1,i)::ifs,0::dx,empty_mifs));
	     ((lA.(i),m),(hx@[1],mx),((0,i)::[],1::dx,mifs)) (* movers passed to complement *)
	    ])::sofar
    else sofar;;

let merge2: lexArray -> string list -> lexTree list -> int -> iCat -> 
  (string list * iCat list) list -> (string list * iCat list) list =
  fun lA input nonterms i ((_,m),(hx,mx),(ifs,dx,mifs)) sofar ->
    if nonterms <> []
    then
      let empty_m: lexTree list array = Array.make (Array.length m) [] in
      let empty_mx: ix array  = Array.make (Array.length mx) [] in
      let empty_mifs: ifeature list array = Array.make (Array.length mx) [] in
	([],[((nonterms,m),(hx@[1],mx),((1,i)::ifs,0::dx,mifs));
	     ((lA.(i),empty_m),(hx@[0],empty_mx),((0,i)::[],1::dx,empty_mifs))
	    ])::sofar
    else sofar;;

(* Note treatment of ifs here:
   Movers enter the derivation with move1, continue with move2, land with merge3,4.
   The begin to accumulate non-empty ifs as soon as they enter the derivation,
   but only when they land do they correspond to a node in the derivation tree,
   and at that point they get an index from the parent of merge3,4.
*)
let rec merge3: string list -> lexTree list -> int -> iCat -> int -> int ->
  (string list * iCat list) list -> (string list * iCat list) list =
  fun input terms i ((h,m),(hx,mx),(ifs,dx,mifs)) next stop sofar -> 
    if terms <> [] && next < stop
    then 
      let continue = merge3 input terms i ((h,m),(hx,mx),(ifs,dx,mifs)) (next+1) stop sofar in
      let (ok,matchingTree) = memberN i m.(next) in  (* if match, it's unique *)
	if ok
	then 
	  let ts = subtreesOf matchingTree in 
	  let tsx = mx.(next) in
	  let ifs0 = mifs.(next) in
	  let empty_m: lexTree list array = Array.make (Array.length m) [] in
	  let empty_mx: ix array  = Array.make (Array.length mx) [] in
	  let empty_mifs: ifeature list array  = Array.make (Array.length mifs) [] in
	  let n = Array.copy m in
	  let nx = Array.copy mx in
	  let nifs = Array.copy mifs in
	    ( n.(next) <- [];  (* we used the "next" licensee, so now empty *)
	      nx.(next) <- [];
	      nifs.(next) <- [];
	      ([],[((terms,empty_m),(hx,empty_mx),((1,i)::ifs,0::dx,empty_mifs)); 
		   ((ts,n),(tsx,nx),((0,i)::ifs0,1::dx,nifs))  (* movers passed to complement *)
		  ])::continue; )
	else continue
    else sofar;;

let rec merge4: string list -> lexTree list -> int -> iCat -> int -> int ->
  (string list * iCat list) list -> (string list * iCat list) list =
  fun input nonterms i ((h,m),(hx,mx),(ifs,dx,mifs)) next stop sofar ->
    if nonterms <> [] && next < stop
    then 
      let continue = merge4 input nonterms i ((h,m),(hx,mx),(ifs,dx,mifs)) (next+1) stop sofar in
      let (ok,matchingTree) = memberN i m.(next) in  (* if match, it's unique *)
	if ok
	then 
	  let ts = subtreesOf matchingTree in 
	  let tsx = mx.(next) in
	  let ifs0 = mifs.(next) in
	  let empty_m: lexTree list array = Array.make (Array.length m) [] in
	  let empty_mx: ix array  = Array.make (Array.length mx) [] in
	  let empty_mifs: ifeature list array  = Array.make (Array.length mifs) [] in
	  let n = Array.copy m in
	  let nx = Array.copy mx in
	  let nifs = Array.copy mifs in
	    ( n.(next) <- [];  (* we used the "next" licensee, so now empty *)
	      nx.(next) <- [];
	      nifs.(next) <- [];
	      ([],[((nonterms,n),(hx,nx),((1,i)::ifs,0::dx,nifs));  (* remaining movers kept in head *)
		   ((ts,empty_m),(tsx,empty_mx),((0,i)::ifs0,1::dx,empty_mifs))
		  ])::continue; )
	else continue
    else sofar;;

let move1: lexArray -> string list -> lexTree list -> int -> iCat -> 
  (string list * iCat list) list -> (string list * iCat list) list =
  fun lA input ts i ((_,m),(hx,mx),(ifs,dx,mifs)) sofar -> 
    if m.(i)=[]                           (* SMC *)
    then 
      let n = Array.copy m in
      let nx = Array.copy mx in
      let nifs = Array.copy mifs in
	( n.(i) <- lA.(i);
	  nx.(i) <- hx@[0];
	  nifs.(i) <- (2,i)::[];
	  ([],[((ts,n),(hx@[1],nx),((3,i)::ifs,0::dx,nifs))])::sofar; )
    else sofar;;

let rec move2: string list -> lexTree list -> int -> iCat -> int -> int ->
  (string list * iCat list) list -> (string list * iCat list) list =
  fun input ts i ((h,m),(hx,mx),(ifs,dx,mifs)) next stop sofar ->
    if next < stop
    then 
      let continue = move2 input ts i ((h,m),(hx,mx),(ifs,dx,mifs)) (next+1) stop sofar in
      let (ok,matchingTree) = memberN i m.(next) in  (* if match, it's unique *)
      let (rootType,rootF) = rootLabel matchingTree in
	if ok && (rootF=next || m.(rootF)=[]) (* SMC *)
	then 
	  let mts = subtreesOf matchingTree in
	  let mtsx = mx.(next) in
	  let ifs0 = mifs.(next) in
	  let n = Array.copy m in
	  let nx = Array.copy mx in
	  let nifs = Array.copy mifs in
	    ( n.(next) <- [];  (* we have used the "next" licensee, so now empty *)
	      nx.(next) <- [];
	      nifs.(next) <- [];
	      n.(rootF) <- mts;
	      nx.(rootF) <- mtsx;
	      nifs.(rootF) <- (2,i)::ifs0;
	      ([],[((ts,n),(hx,nx),((3,i)::ifs,0::dx,nifs))])::continue; )
	else continue
    else sofar;;

(* apply all possible rules to compute expansions of iCat *)
let rec exps : lexArrays -> string list -> iCat -> 
  (string list * iCat list) list -> (string list * iCat list) list = 
  fun (sA,lA,tA) input ((h,m),(hx,mx),dt) sofar -> match h with
    | [] -> sofar
    | W w::is -> 
      let sc = scan w input m mx dt sofar in
	exps (sA,lA,tA) input ((is,m),(hx,mx),dt) sc
    | N ((1,i),ts)::is -> (* 1 = select, triggers merge *)
      let (terms,nonterms) = terminalsOf ts in
      let r1 = merge1 lA input terms i ((h,m),(hx,mx),dt) sofar in
      let r2 = merge2 lA input nonterms i ((h,m),(hx,mx),dt) r1 in
      let r3 = merge3 input terms i ((h,m),(hx,mx),dt) 0 (Array.length m) r2 in
      let r4 = merge4 input nonterms i ((h,m),(hx,mx),dt) 0 (Array.length m) r3 in
	exps (sA,lA,tA) input ((is,m),(hx,mx),dt) r4
    | N ((3,i),ts)::is -> (* 3 = pos, licensor, triggers movement *)
      let v1 = move1 lA input ts i ((h,m),(hx,mx),dt) sofar in
      let v2 = move2 input ts i ((h,m),(hx,mx),dt) 0 (Array.length m) v1 in
	exps (sA,lA,tA) input ((is,m),(hx,mx),dt) v2
    | _ -> failwith "error: exps";;

(* example 
let ic0:iCat = 
  let h = lA0.(1) in
  let m = Array.make (Array.length sA0) [] in
  let mx = Array.make (Array.length sA0) [] in
  let hx = [] in
    ((h,m),(hx,mx));;
printIcat lexArrays0 ic0;;

let input = ["the";"wine"] in
  printIcat lexArrays0 ic0;
  print_string "===== =====\n";
  exps lexArrays0 input ic0 [];;

let iq0 = IQ.add ic0 IQ.empty;;
printIQ lexArrays0 iq0;;
let input = ["the";"wine"] in
let prob = 1. in
let dq0 = DQ.add (input,iq0,prob,[]) DQ.empty in
printDQ lexArrays0 dq0;;
*)

let rec insertNewParses: string list -> float -> float -> IQ.t -> DQ.t -> dnode list ->
  (string list * iCat list) list -> DQ.t = 
  fun input p new_p q dq dns -> function
    | [] -> dq
    | (w,[(([],_),_,(ifs,dx,_))])::more -> (* scan is a special case *)
      let (ok,remainder) = prefixT (w,input) in
	if ok
	then let newParse = (remainder,q,p,Ld (dx,(w,ifs))::dns) in
	       insertNewParses input p new_p q (DQ.add newParse dq) dns more
	else failwith "error1: insertNewParses"
    | ([],ics)::more -> (* in all other cases: we push ics onto iq with new_p *)
      let new_q = List.fold_left (fun iq i -> IQ.add i iq) q ics in
      let new_dns = List.fold_left (fun l (_,_,(_,x,_)) -> Nd x::l) dns ics in
      let newParse = (input,new_q,new_p,new_dns) in 
	insertNewParses input p new_p q (DQ.add newParse dq) dns more
    | _ -> failwith "error2: insertNewParses";;

(* EXTEND THE BEAM RECURSIVELY *)
let rec derive : lexArrays -> float -> DQ.t -> (dnode list * DQ.t) = fun lexAs min dq -> 
  if (DQ.is_empty dq)
  then 
    raise Not_Accepted
  else 
    let ((input,iq,p,dns),new_dq) = DQ.extract_min dq in
(*      Printf.fprintf stdout "-- just popped this DQ:\n"; printDQ lexAs dq; *)
      if IQ.is_empty iq && input=[] then (dns,new_dq) (* success! *)
      else if IQ.is_empty iq
      then derive lexAs min new_dq
      else
        let (ic,new_iq) = IQ.extract_min iq in
	let xs = exps lexAs input ic [] in
(*	  Printf.fprintf stdout "\n ****** List.length(xs)=%i\n" (List.length xs); *)
	  if (List.length xs) = 0 (* avoids divide by 0 *)
	  then derive lexAs min new_dq
	  else let new_p = p /. (float_of_int (List.length xs)) in
		 if new_p > min    (* only keep parses more probable than min *)
		 then derive lexAs min (insertNewParses input p new_p new_iq new_dq dns xs)
		 else derive lexAs min new_dq;;

(* convert the derivation tree from integer features to (Sel string)-type features *)
let rec idtree2dtree: sArray -> idtree -> derivationTree = fun sA -> function
  | Li (ss,ifs) -> L (ss,List.map (fOfInts sA) ifs)
  | Oi t -> O (idtree2dtree sA t)
  | Xi (t1,t2) -> X (idtree2dtree sA t1,idtree2dtree sA t2);;

let parseDQ (sA,lA,tA) min dq0 =
(*  Printf.fprintf stdout "\n ****** min=%f\n" min; printDQ (sA,lA,tA) dq0; *)
  let (dns,dq) = derive (sA,lA,tA) min dq0 in
    (idtree2dtree sA (dnodes2idtree dns),dq)

(* INITIALIZE AND BEGIN: create the initial derivation queue and parse -- 
  analyses with p<min are discarded *)
let parse: g -> string -> float -> string list -> (derivationTree * DQ.t) = 
  fun lex start min input ->
    let (sA:sArray) = Array.of_list (stringValsOfG [] lex) in
    let (lA,tA) = gIntoLexArrayTypeArray sA lex in
    let lAs: lexArrays = (sA,lA,tA) in
    let startInts: ifeature = intsOfF sA (Cat start) in
    let h = lA.(snd startInts) in
    let m: lexArray = Array.make (Array.length sA) [] in
    let mx: ix array = Array.make (Array.length sA) [] in
    let ifs: ifeature list = startInts::[] in
    let dx: ix = [] in
    let mifs: ifeature list array = Array.make (Array.length sA) [] in
    let dt: dtuple = (ifs,dx,mifs) in
    let ic: iCat = ((h,m),([],mx),dt) in
    let iq  = IQ.add ic IQ.empty in
    let prob: float = 1. in
    let dq = (DQ.add (input,iq,prob,Nd []::[]) DQ.empty) in
      parseDQ lAs min dq;;

(* examples:
let input =  ["the";"wine"]
  in parse mg0 "D" 0.01 input;;
let input =  ["the";"king";"knows";"which";"wine";"the";"queen";"prefers"]
  in parse mg0 "C" 0.001 input;;
let input =  ["the";"king";"knows";"which";"queen";"prefers";"the";"wine"]
  in parse mg0 "C" 0.001 input;;
let input =  ["the";"queen";"prefers";"the";"wine"]
  in parse mg0 "C" 0.001 input;;
let input =  ["which";"wine";"the";"queen";"prefers"]
  in parse mg0 "C" 0.001 input;;
let input =  ["the";"king";"knows";"the";"queen";"prefers";"the";"wine"]
  in parse mg0 "C" 0.001 input;;
*)

(* derivation tree to string tree, with pairs at the leaves *)
let rec dt2t = function
  | L (string,fs) ->  T("("^(spacedstring2 string)^","^(stringFs fs)^")",[])
  | X (d1,d2) -> T ("*",[dt2t d1;dt2t d2])
  | O (d) -> T ("o",[dt2t d]);;

(*
let input =  ["the";"king";"knows";"the";"queen";"prefers";"the";"wine"]
  in tktree (dt2t (parse mg0 "C" 0.001 input));;
let input =  ["the";"king";"knows";"which";"wine";"the";"queen";"prefers"]
  in tktree (dt2t (parse mg0 "C" 0.001 input));;
*)

#use "dtState.ml";;

(*
let input =  ["the";"king";"knows";"which";"wine";"the";"queen";"prefers"]
  in dtState (parse mg0 "C" 0.001 input);;
*)

#use "dt2st.ml";;

(*
let input =  ["the";"king";"knows";"which";"wine";"the";"queen";"prefers"]
  in dt2st (parse mg0 "C" 0.001 input);;
let input =  ["the";"king";"knows";"which";"wine";"the";"queen";"prefers"]
  in st2t (dt2st (parse mg0 "C" 0.001 input));;
let input =  ["the";"king";"knows";"which";"wine";"the";"queen";"prefers"]
  in tktree (st2t (dt2st (parse mg0 "C" 0.001 input)));;
*)

#use "dt2bts.ml";;

(*
let input =  ["the";"king";"knows";"which";"wine";"the";"queen";"prefers"]
  in dt2bt (parse mg0 "C" 0.001 input);;
let input =  ["the";"king";"knows";"which";"wine";"the";"queen";"prefers"]
  in bt2t (dt2bt (parse mg0 "C" 0.001 input));;
let input =  ["the";"king";"knows";"which";"wine";"the";"queen";"prefers"]
  in tktree (bt2t (dt2bt (parse mg0 "C" 0.001 input)));;
*)

#use "dt2xbs.ml";;

(*
let input =  ["the";"king";"knows";"which";"wine";"the";"queen";"prefers"]
  in dt2xb (parse mg0 "C" 0.001 input);;
let input =  ["the";"king";"knows";"which";"wine";"the";"queen";"prefers"]
  in xb2t (dt2xb (parse mg0 "C" 0.001 input));;
let input =  ["the";"king";"knows";"which";"wine";"the";"queen";"prefers"]
  in tktree (xb2t (dt2xb (parse mg0 "C" 0.001 input)));;
*)

#use "dt2vt.ml";;

(*
let input =  ["the";"king";"knows";"which";"wine";"the";"queen";"prefers"]
  in dt2vt (parse mg0 "C" 0.001 input);;
let input =  ["the";"king";"knows";"which";"wine";"the";"queen";"prefers"]
  in vt2t (dt2vt (parse mg0 "C" 0.001 input));;
let input =  ["the";"king";"knows";"which";"wine";"the";"queen";"prefers"]
  in tktree (vt2t (dt2vt (parse mg0 "C" 0.001 input)));;
*)

#use "dt2mcfg.ml";;

(*
let input =  ["the";"king";"knows";"which";"wine";"the";"queen";"prefers"]
  in dt2mcfg (parse mg0 "C" 0.001 input);;
let input =  ["the";"king";"knows";"which";"wine";"the";"queen";"prefers"]
  in mcfg2t (dt2mcfg (parse mg0 "C" 0.001 input));;
let input =  ["the";"king";"knows";"which";"wine";"the";"queen";"prefers"]
  in tktree (mcfg2t (dt2mcfg (parse mg0 "C" 0.001 input)));;
*)

let time f x =
    let t = Sys.time() in
    let fx = f x in
    ( Printf.printf "parse found in %fs\n" (Sys.time() -. t); flush stdout ; fx; )

let rec process tree =
  print_string ("(h for help): ");
  let linestring = read_line() in
  let input = split (regexp "[\ \t]+") linestring in
    if List.length input > 0 && List.hd input = "h"
    then (print_string "  d for derivation tree\n";
	  print_string "  s for state tree\n";
	  print_string "  x for X' tree\n"; 
	  print_string "  b for bare tree\n";
	  print_string "  v for value tree\n";
	  print_string "  m for mcfg derivation tree (states as mcfg categories)\n"; 
	  print_string "  ; to search for another parse of current input\n"; 
	  print_string "  l to display lexical tree\n"; 
	  print_string "  n to get prompt for for next input\n"; 
	  print_string "or for linemode interaction:\n"; 
	  print_string "  pd to prettyprint derivation tree\n";
	  print_string "  ps to prettyprint state tree\n";
	  print_string "  px to prettyprint X' tree\n"; 
	  print_string "  pb to prettyprint bare tree\n";
	  print_string "  pv to prettyprint value tree\n";
	  print_string "  pm to prettyprint mcfg derivation tree (states as mcfg categories)\n"; 
	  process tree; )
    else if List.length input > 0 && List.hd input = "q"
    then 0

    else if List.length input > 0 && List.hd input = "pd"
    then (pptree (dt2t tree); print_string "\n"; process tree; )
    else if List.length input > 0 && List.hd input = "ps"
    then (pptree (st2t (dt2st tree)); print_string "\n";  process tree; )
    else if List.length input > 0 && List.hd input = "px"
    then (pptree (xb2t (dt2xb tree)); print_string "\n";  process tree; )
    else if List.length input > 0 && List.hd input = "pb"
    then (pptree (bt2t (dt2bt tree)); print_string "\n";  process tree; )
    else if List.length input > 0 && List.hd input = "pv"
    then (pptree (vt2t (dt2vt tree)); print_string "\n";  process tree; )
    else if List.length input > 0 && List.hd input = "pm"
    then (pptree (mcfg2t (dt2mcfg tree)); print_string "\n";  process tree; )

    else if List.length input > 0 && List.hd input = "d"
    then (tktree (dt2t tree); process tree; )
    else if List.length input > 0 && List.hd input = "s"
    then (tktree (st2t (dt2st tree)); process tree; )
    else if List.length input > 0 && List.hd input = "x"
    then (tktree (xb2t (dt2xb tree)); process tree; )
    else if List.length input > 0 && List.hd input = "b"
    then (tktree (bt2t (dt2bt tree)); process tree; )
    else if List.length input > 0 && List.hd input = "v"
    then (tktree (vt2t (dt2vt tree)); process tree; )
    else if List.length input > 0 && List.hd input = "m"
    then (tktree (mcfg2t (dt2mcfg tree)); process tree; )
    else if List.length input > 0 && List.hd input = "n"
    then 1
    else if List.length input > 0 && List.hd input = ";"
    then -1
    else 1

let rec loop1 (sA,lA,tA) h0 ifs0 dx0 min0 dq0 linestring = (* parse and process result *)
  try
    let (tree,dq) = time (parseDQ (sA,lA,tA) min0) dq0 in
(*  let (tree,dq) = parseDQ (sA,lA,tA) min0 dq0 in  *)
    let n = process tree in
      if n=0 (* done *)
      then ()
      else if n=1 (* get next input, build new dq *)
      then loop0 (sA,lA,tA) h0 ifs0 dx0 min0
      else (* if n=-1, look for another parse in current dq *)
	loop1 (sA,lA,tA) h0 ifs0 dx0 min0 dq linestring
  with
      Not_Accepted -> 
	( print_string ("Not accepted: "^linestring^"\n"); 
	  loop0 (sA,lA,tA) h0 ifs0 dx0 min0;
	)
and loop0 (sA,lA,tA) h0 ifs0 dx0 min0 =  (* get input and set up mutable parts of initial dq *)
  let m: lexArray = Array.make (Array.length sA) [] in
  let mx: ix array = Array.make (Array.length sA) [] in
  let mifs: ifeature list array = Array.make (Array.length sA) [] in
  let dt: dtuple = (ifs0,dx0,mifs) in
  let ic: iCat = ((h0,m),([],mx),dt) in
  let iq  = IQ.add ic IQ.empty in
  let prob: float = 1. in
    print_string ("\n: ");
    let linestring = read_line() in
    let input = split (regexp "[\ \t]+") linestring in
    let dq = (DQ.add (input,iq,prob,Nd []::[]) DQ.empty) in
      loop1 (sA,lA,tA) h0 ifs0 dx0 min0 dq linestring

let go = fun lex start min0 ->
    let (sA:sArray) = Array.of_list (stringValsOfG [] lex) in
    let (lA,tA) = gIntoLexArrayTypeArray sA lex in
    let lAs: lexArrays = (sA,lA,tA) in
    let startInts: ifeature = intsOfF sA (Cat start) in
    let h = lA.(snd startInts) in
    let ifs: ifeature list = startInts::[] in
    let dx: ix = [] in
      loop0 lAs h ifs dx min0

(* using our example grammar:

   go mg0 "C" 0.00000001

 to use another grammar, you must load it 
 (or copy it into this file and reload this file)
*)
