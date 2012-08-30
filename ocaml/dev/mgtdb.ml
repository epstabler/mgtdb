(* mgtdb.ml   comments welcome: stabler@ucla.edu
   everything in 1 file for now.
   load with: #use "mgtdb.ml";;
   see examples at bottom of file

   * for cats that lack subtrees in lex, tA is not set. This does not matter,
     but we could probably get rid of tA altogether.
   * sA sets all features, but lA only needs non-empty lexTrees.
   * We might speed things up by numbering all subtrees of the lexArray,
     and using int arrays to encode the whole lexTree.
*)

type tree = T of string * tree list  (* generic string tree *)

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
]
let startCat0 = "C"

(* REPRESENTING GRAMMAR WITH A TREE FOR THE RECOGNIZER *)
type ifeature = int * int  (* type, value *)
type lexTree = W of string list | N of (ifeature * lexTree list)
type sArray = string array (* string values of the features *)
type tArray = int array (* the types of each element of lexArray roots *)
type lexArray = lexTree list array (* lexTrees *)
type lexArrays = sArray * lexArray * tArray

(* TYPES FOR INDEXED CONSTITUENTS *)
type movers = lexTree list array (* NB: same as lexArray *)
type cat = lexTree list * movers (* intuitively:lexTree list=children of head *)
type ix = int list
type iCat = cat * (ix * ix array) (* cat, indices of head, movers, respectively *)

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
  val merge : t -> t -> t
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
  end
(*** END MakeQ ****)

(* minIndex should only check indices of filled mover positions.
   No mover has an empty index, so we can ignore them. *)
let minIndex sofar =
  Array.fold_left (fun x y -> if y<>[] then (min x y) else x) sofar

(* for queues of predictions *)
module ICatPoset = (* order by comparing least index, an int list *)
struct type t = iCat
  let le = fun ((_,(i1,a1)):iCat) ((_,(i2,a2)):iCat) -> 
    compare (minIndex i1 a1) (minIndex i2 a2) < 1
end

module IQ = MakeQ (ICatPoset)    (* for the queues of predictions *)

(* for the derivation queue *)
type der = string list * IQ.t * float  (* type of partial derivations *)

module DerivationPoset = (* order by (decreasing) probability, a float *)
struct type t = der
  let le = fun ((_,_,p1):der) ((_,_,p2):der) -> compare p2 p1 < 1
end

module DQ = MakeQ (DerivationPoset)  (* the queue of (partial) derivations *)

let btfyFeat: feature -> string = 
  function Sel x -> "="^x | Cat x -> x | Pos x -> "+"^x | Neg x -> "-"^x

(* REPRESENT GRAMMAR AS TREE *)
let stringVal: feature -> string = function
  | Cat s -> s
  | Sel s -> s
  | Pos s -> s
  | Neg s -> s

(* (ensureMember e list) adds e to list if not already there *)
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
      else failwith ("error: arrrayNth sA "^s) in
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
  | t::trees,(w,fs) -> t::revItemIntoLexTrees sA (trees,(w,fs))

let rootLabel = function N(f,_) -> f | _ -> failwith "error: rootLabel"
let subtreesOf = function N(_,ts) -> ts | _ -> failwith "error:subtreesOf"
let termLabel = function W s -> s | _ -> failwith "error: termLabel"

(* to get trees in the array, insert the root feature determined by the index*)
let lexArrays2lexTrees: lexArrays -> lexTree list = fun (sA,lA,tA) ->
  let rec totrees i res =
    if i < 0 then res else totrees (i - 1) (N ((tA.(i),i),lA.(i)) :: res) in
    totrees (Array.length lA - 1) []

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

(* RECOGNIZER STEPS: SCAN/UNMERGE/UNMOVE *)
let emptyListArray: lexTree list array -> bool = (* true iff all empty lists *)
  Array.fold_left (fun b x -> (x=[] && b)) true

let rec terminalsOf: lexTree list -> (lexTree list * lexTree list) = function
  | [] -> ([],[])
  | (W w)::ts -> let (terms,nonterms) = terminalsOf ts in ((W w)::terms,nonterms)
  | (N p)::ts -> let (terms,nonterms) = terminalsOf ts in (terms,(N p)::nonterms)

(* we could use exception instead of bool *)
let rec prefixT : (string list * string list) -> (bool * string list) = function
  | ([],remainder) -> (true,remainder)
  | (x::xs,y::ys) when x=y -> prefixT (xs,ys)
  | _ -> (false,[])

(* we could use exception instead of bool *)
let rec memberN: int -> lexTree list -> (bool * lexTree) = fun f -> function
    | [] -> (false,N((-1,-1),[]))
    | N ((t,g),ts)::more -> if g=f then (true,N((t,g),ts)) else memberN f more
    | W _::more -> memberN f more

let scan : string list -> string list -> lexArray -> ix array -> 
  (string list * iCat list) list -> (string list * iCat list) list =
  fun w input m mx sofar ->
  if emptyListArray m
  then let (ok,remainder) = prefixT (w,input) in
	 if ok then (remainder,[(([],m),([],mx))])::sofar else sofar
  else sofar

let merge1: lexArray -> string list -> lexTree list -> int -> iCat -> 
  (string list * iCat list) list -> (string list * iCat list) list =
  fun lA input terms i ((_,m),(hx,mx)) sofar ->
    if terms <> []
    then 
      let empty_m: lexTree list array = Array.make (Array.length m) [] in
      let empty_mx: ix array  = Array.make (Array.length mx) [] in
	(input,[((terms,empty_m),(hx@[0],empty_mx));
		((lA.(i),m),(hx@[1],mx)) (* movers passed to complement *)
	       ])::sofar
    else sofar

let merge2: lexArray -> string list -> lexTree list -> int -> iCat -> 
  (string list * iCat list) list -> (string list * iCat list) list =
  fun lA input nonterms i ((_,m),(hx,mx)) sofar ->
    if nonterms <> []
    then
      let empty_m: lexTree list array = Array.make (Array.length m) [] in
      let empty_mx: ix array  = Array.make (Array.length mx) [] in
	(input,[((nonterms,m),(hx@[1],mx));
		((lA.(i),empty_m),(hx@[0],empty_mx))
	       ])::sofar
    else sofar

let rec merge3: string list -> lexTree list -> int -> iCat -> int -> int ->
  (string list * iCat list) list -> (string list * iCat list) list =
  fun input terms i ((h,m),(hx,mx)) next stop sofar -> 
    if terms <> [] && next < stop
    then 
      let continue = merge3 input terms i ((h,m),(hx,mx)) (next+1) stop sofar in
      let (ok,matchingTree) = memberN i m.(next) in  (* if match, it's unique *)
	if ok
	then 
	  let ts = subtreesOf matchingTree in 
	  let tsx = mx.(next) in
	  let empty_m: lexTree list array = Array.make (Array.length m) [] in
	  let empty_mx: ix array  = Array.make (Array.length mx) [] in
	  let n = Array.copy m in
	  let nx = Array.copy mx in
	    ( n.(next) <- [];  (* we used the "next" licensee, so now empty *)
	      nx.(next) <- [];
	      (input,[((terms,empty_m),(hx,empty_mx));
		      ((ts,n),(tsx,nx))  (* movers passed to complement *)
		     ])::continue; )
	else continue
    else sofar

let rec merge4: string list -> lexTree list -> int -> iCat -> int -> int ->
  (string list * iCat list) list -> (string list * iCat list) list =
  fun input nonterms i ((h,m),(hx,mx)) next stop sofar ->
    if nonterms <> [] && next < stop
    then 
      let continue = merge4 input nonterms i ((h,m),(hx,mx)) (next+1) stop sofar in
      let (ok,matchingTree) = memberN i m.(next) in  (* if match, it's unique *)
	if ok
	then 
	  let ts = subtreesOf matchingTree in 
	  let tsx = mx.(next) in
	  let empty_m: lexTree list array = Array.make (Array.length m) [] in
	  let empty_mx: ix array  = Array.make (Array.length mx) [] in
	  let n = Array.copy m in
	  let nx = Array.copy mx in
	    ( n.(next) <- [];  (* we used the "next" licensee, so now empty *)
	      nx.(next) <- [];
	      (input,[((nonterms,n),(hx,nx));  (* remaining movers kept in head *)
		      ((ts,empty_m),(tsx,empty_mx))
		     ])::continue; )
	else continue
    else sofar

let move1: lexArray -> string list -> lexTree list -> int -> iCat -> 
  (string list * iCat list) list -> (string list * iCat list) list =
  fun lA input ts i ((_,m),(hx,mx)) sofar -> 
    if m.(i)=[]                           (* SMC *)
    then 
      let n = Array.copy m in
      let nx = Array.copy mx in
	( n.(i) <- lA.(i);
	  nx.(i) <- hx@[0];
	  (input,[((ts,n),(hx@[1],nx))])::sofar; )
    else sofar

let rec move2: string list -> lexTree list -> int -> iCat -> int -> int ->
  (string list * iCat list) list -> (string list * iCat list) list =
  fun input ts i ((h,m),(hx,mx)) next stop sofar ->
    if next < stop
    then 
      let continue = move2 input ts i ((h,m),(hx,mx)) (next+1) stop sofar in
      let (ok,matchingTree) = memberN i m.(next) in  (* if match, it's unique *)
      let (rootType,rootF) = rootLabel matchingTree in
	if ok && (rootF=next || m.(rootF)=[]) (* SMC *)
	then 
	  let mts = subtreesOf matchingTree in 
	  let mtsx = mx.(next) in
	  let n = Array.copy m in
	  let nx = Array.copy mx in
	    ( n.(next) <- [];  (* we have used the "next" licensee, so now empty *)
	      nx.(next) <- [];
	      n.(rootF) <- mts;
	      nx.(rootF) <- mtsx;
	      (input,[((ts,n),(hx,nx))])::continue; )
	else continue
    else sofar

(* apply all possible rules to compute expansions of iCat *)
let rec exps : lexArrays -> string list -> iCat -> 
  (string list * iCat list) list -> (string list * iCat list) list = 
  fun (sA,lA,tA) input ((h,m),(hx,mx)) sofar -> match h with
    | [] -> sofar
    | W w::is -> 
      let sc = scan w input m mx sofar in
	exps (sA,lA,tA) input ((is,m),(hx,mx)) sc
    | N ((1,i),ts)::is -> (* 1 = select, triggers merge *)
      let (terms,nonterms) = terminalsOf ts in
      let r1 = merge1 lA input terms i ((h,m),(hx,mx)) sofar in
      let r2 = merge2 lA input nonterms i ((h,m),(hx,mx)) r1 in
      let r3 = merge3 input terms i ((h,m),(hx,mx)) 0 (Array.length m) r2 in
      let r4 = merge4 input nonterms i ((h,m),(hx,mx)) 0 (Array.length m) r3 in
	exps (sA,lA,tA) input ((is,m),(hx,mx)) r4
    | N ((3,i),ts)::is -> (* 3 = pos, licensor, triggers movement *)
      let v1 = move1 lA input ts i ((h,m),(hx,mx)) sofar in
      let v2 = move2 input ts i ((h,m),(hx,mx)) 0 (Array.length m) v1 in
	exps (sA,lA,tA) input ((is,m),(hx,mx)) v2
    | _ -> failwith "error: exps"

let rec insertNewParses: float -> float -> IQ.t -> DQ.t -> (string list * iCat list) list -> DQ.t = 
  fun p new_p q dq -> function
  | [] -> dq
  | (input,[(([],_),_)])::more -> (* scan is a special case *)
    let newParse = (input,q,p) in
      insertNewParses p new_p q (DQ.add newParse dq) more
  | (input,ics)::more -> (* in all other cases: we push ics onto iq with new_p *)
   let newParse = (input,(List.fold_left (fun iq i -> IQ.add i iq) q ics),new_p) in
      insertNewParses p new_p q (DQ.add newParse dq) more

(* EXTEND THE BEAM RECURSIVELY *)
let rec derive : lexArrays -> float -> DQ.t -> bool = fun lexAs min dq -> 
  if (DQ.is_empty dq)
  then false
  else 
    let ((input,iq,p),new_dq) = DQ.extract_min dq in
      if IQ.is_empty iq && input=[] then true
      else if IQ.is_empty iq
      then derive lexAs min new_dq
      else
        let (ic,new_iq) = IQ.extract_min iq in
	let xs = exps lexAs input ic [] in
	let new_p = p /. (float_of_int (List.length xs)) in
	  if new_p > min    (* only keep parses more probable than min *)
	  then derive lexAs min (insertNewParses p new_p new_iq new_dq xs)
	  else derive lexAs min new_dq

(* INITIALIZE AND BEGIN: create the initial derivation queue and parse -- 
  analyses with p<min are discarded *)
let recognize: g -> string -> float -> string list -> bool = fun lex start min input ->
  let (sA:sArray) = Array.of_list (stringValsOfG [] lex) in
  let (lA,tA) = gIntoLexArrayTypeArray sA lex in
  let lAs: lexArrays = (sA,lA,tA) in
  let startInts: ifeature = intsOfF sA (Cat start) in
  let h = lA.(snd startInts) in
  let m: lexArray = Array.make (Array.length sA) [] in
  let mx: ix array = Array.make (Array.length sA) [] in
  let ic: iCat = ((h,m),([],mx)) in
  let iq  = IQ.add ic IQ.empty in
  let prob: float = 1. in
  let dq = (DQ.add (input,iq,prob) DQ.empty) in
     derive lAs min dq;;

(* examples:
let input =  ["the";"wine"]
  in recognize mg0 "D" 0.01 input;;
let input =  ["the";"king";"knows";"which";"wine";"the";"queen";"prefers"]
  in recognize mg0 "C" 0.001 input;;
let input =  ["the";"king";"knows";"which";"queen";"prefers";"the";"wine"]
  in recognize mg0 "C" 0.001 input;;
let input =  ["the";"king";"knows";"the";"queen";"prefers";"the";"wine"]
  in recognize mg0 "C" 0.001 input;;
let input =  ["the";"queen";"prefers";"the";"wine"]
  in recognize mg0 "C" 0.001 input;;
*)
