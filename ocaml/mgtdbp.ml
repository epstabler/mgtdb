open Tktree (* type tree is imported from Tktree *)
open Makeq (* for the priority queues *)

type feature = Sel of string | Cat of string | Neg of string | Pos of string
type lexItem = string list * feature list
type g = lexItem list
type ifeature = int * int
type iLexItem = string list * ifeature list
type lexTree = W of string list | N of (ifeature * lexTree list)
type sArray = string array
type tArray = int array
type lexArray = lexTree list array
type lexArrays = sArray * lexArray * tArray
type movers = lexTree list array
type cat = lexTree list * movers
type ix = int list
type dnode = Nd of ix  | Ld of (ix * iLexItem)
type dtuple = (ifeature list * ix * ifeature list array)
type idtree =  Li of string list * ifeature list | Xi of idtree * idtree | Oi of idtree
type derivationTree = 
    L of string list * feature list
  | X of derivationTree * derivationTree
  | O of derivationTree
type iCat = cat * (ix * ix array) * dtuple

let minIndex sofar =
  Array.fold_left (fun x y -> if y<>[] then (min x y) else x) sofar

module ICatPoset = 
  struct
    type t = iCat
    let le = fun ((_,(i1,a1),_):iCat) ((_,(i2,a2),_):iCat) -> 
      compare (minIndex i1 a1) (minIndex i2 a2) < 1
  end

module IQ = Makeq (ICatPoset)

type der = string list * IQ.t * float * dnode list

module DerivationPoset = 
  struct 
    type t = der
    let le = fun ((_,_,p1,_):der) ((_,_,p2,_):der) -> compare p2 p1 < 1
  end

module DQ = Makeq (DerivationPoset)

(* in many function definitions, I explicitly indicate the type *)
let btfyFeat: feature -> string = 
  function Sel x -> "="^x | Cat x -> x | Pos x -> "+"^x | Neg x -> "-"^x

let rec joinStrings sep = function
  | [] -> ""
  | s::[] -> s
  | s::t::more -> s^sep^(joinStrings sep (t::more))

let rec stringFs fs = joinStrings " " (List.map btfyFeat fs)
let rec stringFFs ffs = joinStrings "," (List.map stringFs ffs)
let spacedstring2 = function [] -> "e" | l -> joinStrings " " l

let btfyLexItem: lexItem -> string = fun (ss,fs) ->
  (joinStrings " " ss)^"::"^(joinStrings " " (List.map btfyFeat fs))

let showGrammar: g -> unit =
  List.iter (fun x -> print_string (btfyLexItem x); print_string "\n")

let rec splitDnodes: 
    (int list list * (int list * iLexItem) list) -> dnode list -> 
    (int list list * (int list * iLexItem) list) = fun (n,t) -> function 
    | [] -> List.sort compare n,List.sort compare t
    | Nd i::ts -> splitDnodes (List.rev i::n,t) ts
    | Ld (i,lex)::ts -> splitDnodes (n,(List.rev i,lex)::t) ts

let rec child = function 
  | ([],_::[]) -> true
  | (x::xs,y::ys) when x=y -> child (xs,ys)
  | _ -> false
  
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
  | _ -> failwith "error: buildIDtreesFromDnodes"

let dnodes2idtree dns = 
  let (nts,ts) = splitDnodes ([],[]) dns in 
  let (dt,nts1,ts1) = buildIDtreeFromDnodes (List.hd nts) (List.tl nts,ts) in
    if nts1=[] && ts1=[] then dt else failwith "error2: dnodes2idtree"

let stringVal: feature -> string = function
  | Cat s -> s
  | Sel s -> s
  | Pos s -> s
  | Neg s -> s

let rec ensureMember : 'a -> 'a list -> 'a list = fun e -> function
  | [] -> [e]
  | x::y when x=e -> x::y
  | x::y -> x::ensureMember e y

let rec stringValsOfG: string list -> g -> string list = fun sofar -> function
  | (w,f::fs)::more -> 
      stringValsOfG (ensureMember (stringVal f) sofar) ((w,fs)::more)
  | (_,[])::more -> stringValsOfG sofar more
  | [] -> sofar

let intsOfF: sArray -> feature -> ifeature = fun sA -> 
  let arrayNth: sArray -> string -> int = fun sA s ->
    let rec arrayNth0 i n s =
      if i<n 
      then (if sA.(i)=s then i else arrayNth0 (i+1) n s)
      else failwith ("error: arrayNth sA "^s) in
      arrayNth0 0 (Array.length sA) s in
    function
      | Cat s -> 0,arrayNth sA s
      | Sel s -> 1,arrayNth sA s
      | Neg s -> 2,arrayNth sA s
      | Pos s -> 3,arrayNth sA s

let fOfInts: sArray -> ifeature -> feature = fun sA (i,j) ->
  if i=0 then Cat sA.(j)
  else if i=1 then Sel sA.(j)
  else if i=2 then Neg sA.(j)
  else if i=3 then Pos sA.(j)
  else failwith "error: fOfInts"

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
  | t::trees,(w,fs) -> t::revItemIntoLexTrees sA (trees,(w,fs))

let rootLabel = function N(f,_) -> f | _ -> failwith "error: rootLabel"
let subtreesOf = function N(_,ts) -> ts | _ -> failwith "error:subtreesOf"
let termLabel = function W s -> s | _ -> failwith "error: termLabel"

let rec lexTree2stringTree: sArray -> lexTree -> tree = fun sA -> function
  | N (ij,ts) -> T (btfyFeat (fOfInts sA ij),lexTrees2stringTrees sA ts)
  | W w -> T (joinStrings " " w,[])
and lexTrees2stringTrees: sArray -> lexTree list -> tree list = fun sA -> function
  | [] -> []
  | t::ts -> lexTree2stringTree sA t::lexTrees2stringTrees sA ts

let lexArrays2lexTrees: lexArrays -> lexTree list = fun (sA,lA,tA) ->
  let rec totrees i res =
    if i < 0 then res else totrees (i - 1) (N ((tA.(i),i),lA.(i)) :: res) in
    totrees (Array.length lA - 1) []

let lexArrays2stringTrees (sA,lA,tA) =
  lexTrees2stringTrees sA (lexArrays2lexTrees (sA,lA,tA))

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
      (lA,tA); ) 

let btfyIndex = List.fold_left (fun l i -> l^(string_of_int i)) ""

let rec deleteEmpties: tree list * string list -> tree list * string list = function
  | [],[] -> [],[]
  | T(_,[])::ts,_::is -> deleteEmpties (ts,is)
  | t::ts,i::is -> let (us,js) = deleteEmpties (ts,is) in t::us,i::js
  | _ -> failwith "error: deleteEmpties"

let emptyListArray: lexTree list array -> bool =
  Array.fold_left (fun b x -> (x=[] && b)) true

let rec terminalsOf: lexTree list -> (lexTree list * lexTree list) = function
  | [] -> ([],[])
  | (W w)::ts -> let (terms,nonterms) = terminalsOf ts in ((W w)::terms,nonterms)
  | (N p)::ts -> let (terms,nonterms) = terminalsOf ts in (terms,(N p)::nonterms)

let rec prefixT : (string list * string list) -> (bool * string list) = function
  | ([],remainder) -> (true,remainder)
  | (x::xs,y::ys) when x=y -> prefixT (xs,ys)
  | _ -> (false,[])

let rec memberN: int -> lexTree list -> (bool * lexTree) = fun f -> function
    | [] -> (false,N((-1,-1),[]))
    | N ((t,g),ts)::more -> if g=f then (true,N((t,g),ts)) else memberN f more
    | W _::more -> memberN f more

let scan : string list -> string list -> lexArray -> ix array -> dtuple ->
  (string list * iCat list) list -> (string list * iCat list) list =
  fun w input m mx dt sofar ->
  if emptyListArray m
  then let (ok,_) = prefixT (w,input) in
	 if ok then (w,[(([],m),([],mx),dt)])::sofar else sofar
  else sofar

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
    else sofar

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
    else sofar

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
    else sofar

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
    else sofar

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
    else sofar

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
    else sofar

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
    | _ -> failwith "error: exps"

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
    | _ -> failwith "error2: insertNewParses"

exception Not_Accepted

let rec derive : lexArrays -> float -> DQ.t -> (dnode list * DQ.t) = fun lexAs min dq -> 
  if (DQ.is_empty dq)
  then 
    raise Not_Accepted
  else 
    let ((input,iq,p,dns),new_dq) = DQ.extract_min dq in
      if IQ.is_empty iq && input=[] then (dns,new_dq) (* success! *)
      else if IQ.is_empty iq
      then derive lexAs min new_dq
      else
        let (ic,new_iq) = IQ.extract_min iq in
	let xs = exps lexAs input ic [] in
	  if (List.length xs) = 0 (* avoids divide by 0 *)
	  then derive lexAs min new_dq
	  else let new_p = p /. (float_of_int (List.length xs)) in
		 if new_p > min    (* only keep parses more probable than min *)
		 then derive lexAs min (insertNewParses input p new_p new_iq new_dq dns xs)
		 else derive lexAs min new_dq;;

let rec idtree2dtree: sArray -> idtree -> derivationTree = fun sA -> function
  | Li (ss,ifs) -> L (ss,List.map (fOfInts sA) ifs)
  | Oi t -> O (idtree2dtree sA t)
  | Xi (t1,t2) -> X (idtree2dtree sA t1,idtree2dtree sA t2)

let parseDQ (sA,lA,tA) min dq0 =
  let (dns,dq) = derive (sA,lA,tA) min dq0 in
    (idtree2dtree sA (dnodes2idtree dns),dq)

let rec dt2t = function
  | L (string,fs) ->  T("("^(spacedstring2 string)^","^(stringFs fs)^")",[])
  | X (d1,d2) -> T ("*",[dt2t d1;dt2t d2])
  | O (d) -> T ("o",[dt2t d])

(* to define move, this auxiliary function to enforce the SMC *)
let rec noOther f = function 
  | [] -> true
  | (Neg g::_)::_ when f=g -> false
  | _::more -> noOther f more

(* to define move, this deletes Neg f and, if smc is respected,
   returns the list newMovers *)
let rec selectUnique f movers = match movers with
    [] -> failwith "move error in selectUnique"
  | (Neg g::fs)::more when f=g -> 
      if noOther f more  (* SMC *)
      then (if fs=[] then more else fs::more)  (* move 1 and 2, respectively *)
      else failwith "SMC violation"
  | fs::more -> fs::selectUnique f more

let splitThird (x,y,z) = match z with
  | t::ts -> (x,y,t,ts)
  | _ -> failwith "splitThird error"

let pptree tree =
  let rec tab n = if n <= 0 then () else (Printf.fprintf stdout " "; tab (n-1);) in
  let rec pptree_aux n = function T(s,ts) -> 
    ( Printf.fprintf stdout "\n"; tab n; Printf.fprintf stdout "T(\"%s\",[" s; 
      List.iter (pptree_aux (n+4)) ts; Printf.fprintf stdout "]);";
    ) in pptree_aux 0 tree;;

let pptrees = List.iter pptree;;
