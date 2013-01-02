(* mgtdb-dev.ml  comments welcome: stabler@ucla.edu
   this is a working version, with print routines and examples.
   load with: #use "mgtdb-dev.ml";;
   see examples at bottom of file

   * for cats that lack subtrees in lex, tA is not set. This does not matter,
     but we could probably get rid of tA altogether.
   * sA sets all features, but lA only needs non-empty lexTrees.
   * We might speed things up by numbering all subtrees of the lexArray,
     and using int list arrays to encode the whole lexTree.
*)

type tree = T of string * tree list  (* generic string tree *)

let pptree tree =
  let rec tab n = if n <= 0 then () else (Printf.fprintf stdout " "; tab (n-1);) in
  let rec pptree_aux n = function T(s,ts) -> 
    ( Printf.fprintf stdout "\n"; tab n; Printf.fprintf stdout "T(\"%s\",[" s; 
      List.iter (pptree_aux (n+4)) ts; Printf.fprintf stdout "]);";
    ) in pptree_aux 0 tree;;

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

let (mg0:g) =[ 
(*
([],[Sel "V"; Cat "C"]); 
(["king"],[Cat "N"]);                     (["queen"],[Cat "N"]);
                     (["beer"],[Cat "N"]);
(["knows"],[Sel "C"; Sel "D"; Cat "V"]);  (["says"],[Sel "C"; Sel "D"; Cat "V"]);
(["drinks"],[Sel "D"; Sel "D"; Cat "V"]);
*)
([],[Sel "V"; Pos "wh"; Cat "C"]);
(["the"],[Sel "N"; Cat "D"]);             (["which"],[Sel "N"; Cat "D"; Neg "wh"]);
(["wine"],[Cat "N"]);
(["prefers"],[Sel "D"; Sel "D"; Cat "V"]);
];;
let startCat0 = "C";;

(* example: the copy language over {a,b} with startCatx="T"*)
let (mgxx:g) =[ 
([],[Cat "T"; Neg "r"; Neg "l"]); ([],[Sel "T"; Pos "r"; Pos "l"; Cat "T"]);
(["a"],[Sel "T"; Pos "r"; Cat "A"; Neg "r"]); (["b"],[Sel "T"; Pos "r"; Cat "B"; Neg "r"]); 
(["a"],[Sel "A"; Pos "l"; Cat "T"; Neg "l"]); (["b"],[Sel "B"; Pos "l"; Cat "T"; Neg "l"]); 
]
let startCatxx = "T";;

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
  val rank : t -> int
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
  end;;
(*** END MakeQ ****)

(* minIndex should only check indices of filled mover positions.
   No mover has an empty index, so we can ignore them. *)
let minIndex sofar =
  Array.fold_left (fun x y -> if y<>[] then (min x y) else x) sofar;;

(* example: minIndex [1] [|[0];[]|];;  (* this is [0] *) *)

module IntPoset = 
struct type t = int
  let le = fun i1 i2 -> compare i1 i2 < 1
end;;

module IN=MakeQ (IntPoset);;

let in0 = IN.add 33 IN.empty;;
let in1 = IN.add 56 in0;;
let in2 = IN.add 6 in1;;
let in3 = IN.add 26 in2;;
let in4 = IN.add 0 in3;;
Printf.fprintf stdout "\nafter 33, rank in0=%i\n" (IN.rank in0);;
Printf.fprintf stdout "after 56, rank in1=%i\n" (IN.rank in1);;
Printf.fprintf stdout "after 6,  rank in2=%i\n" (IN.rank in2);;
Printf.fprintf stdout "after 26, rank in3=%i\n" (IN.rank in3);;
Printf.fprintf stdout "after  0, rank in4=%i\n" (IN.rank in4);;

(* for queues of predictions *)
module ICatPoset = (* order by comparing least index, an int list *)
struct type t = iCat
  let le = fun ((_,(i1,a1)):iCat) ((_,(i2,a2)):iCat) -> 
    compare (minIndex i1 a1) (minIndex i2 a2) < 1
end;;

module IQ = MakeQ (ICatPoset);;    (* for the queues of predictions *)

(* for the derivation queue *)
type der = string list * IQ.t * float;;  (* type of partial derivations *)

module DerivationPoset = (* order by (decreasing) probability, a float *)
struct type t = der
  let le = fun ((_,_,p1):der) ((_,_,p2):der) -> compare p2 p1 < 1
end;;

module DQ = MakeQ (DerivationPoset);;  (* the queue of (partial) derivations *)

(* BEAUTIFY GRAMMAR: for practice and for tracing later *)
let btfyFeat: feature -> string = 
  function Sel x -> "="^x | Cat x -> x | Pos x -> "+"^x | Neg x -> "-"^x;;

(* example: btfyFeat (Sel "D");; *)

let rec joinStrings sep = function  (* join is builtin in many programming langs *)
  | [] -> ""
  | s::[] -> s
  | s::t::more -> s^sep^(joinStrings sep (t::more))

let btfyLexItem: lexItem -> string = fun (ss,fs) ->
  (joinStrings " " ss)^"::"^(joinStrings " " (List.map btfyFeat fs));;

(* example: btfyLexItem (["prefers"], [Sel "D"; Sel "D"; Cat "V"]);; *)

let showGrammar: g -> unit =
  List.iter (fun x -> print_string (btfyLexItem x); print_string "\n");;

(* example: showGrammar mg0;; *)

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

(* example: let startInts0 = intsOfF sA0 (Cat startCat0);def intsOfF(sL,(ftype,fval)):
    if ftype=='cat':
        return (0,listNth(fval,sL))
    elif ftype=='sel':
        return (1,listNth(fval,sL))
    elif ftype=='neg':
        return (2,listNth(fval,sL))
    elif ftype=='pos':
        return (3,listNth(fval,sL))
    else:
        raise RuntimeError('error: intsOfF'); *)

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

let printIcat: lexArrays -> iCat -> unit = fun (sA,lA,tA) ((h,mA),(ih,iA)) ->
  let ihS = btfyIndex ih in
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
  let hx = [0;1;1] in
    ( m.(2) <- lA0.(2);   (* the which subtree *)
      mx.(2) <- [0;0];    
      ((h,m),(hx,mx)); );;
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
  else let ((input,iq,p),dq1) = DQ.extract_min dq0 in 
	 Printf.fprintf stdout "%f: [%s]" p (joinStrings "," input);
	 printIQ lexArrays iq; Printf.fprintf stdout " ---- end parse\n";
	 printDQ lexArrays dq1;;

(* example 
let dq0 = DQ.add (["this";"is";"the";"input"],iq0,0.1) DQ.empty;;
printDQ lexArrays0 dq0;;
let dq0 = DQ.add (["this";"is";"the";"input"],iq0,0.1) dq0;;
printDQ lexArrays0 dq0;;
*)

(* RECOGNIZER STEPS: SCAN/UNMERGE/UNMOVE *)
let emptyListArray: lexTree list array -> bool = (* true iff all empty lists *)
  Array.fold_left (fun b x -> (x=[] && b)) true;;

let rec terminalsOf: lexTree list -> (lexTree list * lexTree list) = function
  | [] -> ([],[])
  | (W w)::ts -> let (terms,nonterms) = terminalsOf ts in ((W w)::terms,nonterms)
  | (N p)::ts -> let (terms,nonterms) = terminalsOf ts in (terms,(N p)::nonterms);;

(* prefixT(w,lst) returns (true,remainder) if lst=w@remainder; else (false,[]) *)
(* we could use exception instead of bool *)
let rec prefixT : (string list * string list) -> (bool * string list) = function
  | ([],remainder) -> (true,remainder)
  | (x::xs,y::ys) when x=y -> prefixT (xs,ys)
  | _ -> (false,[]);;

(* memberN(i,lst) returns (true,e) if e is an element of lst such that
  (rootLabel e)=(_,i); else (false,_) if no match *)
(* we could use exception instead of bool *)
let rec memberN: int -> lexTree list -> (bool * lexTree) = fun f -> function
    | [] -> (false,N((-1,-1),[]))
    | N ((t,g),ts)::more -> if g=f then (true,N((t,g),ts)) else memberN f more
    | W _::more -> memberN f more;;

let scan : string list -> string list -> lexArray -> ix array -> 
  (string list * iCat list) list -> (string list * iCat list) list =
  fun w input m mx sofar ->
  if emptyListArray m
  then let (ok,remainder) = prefixT (w,input) in
	 if ok then (remainder,[(([],m),([],mx))])::sofar else sofar
  else sofar;;

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
    else sofar;;

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
    else sofar;;

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
    else sofar;;

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
    else sofar;;

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
    else sofar;;

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
    else sofar;;

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
let dq0 = DQ.add (input,iq0,prob) DQ.empty in
printDQ lexArrays0 dq0;;
*)

let rec insertNewParses: float -> float -> IQ.t -> DQ.t -> (string list * iCat list) list -> DQ.t = 
  fun p new_p q dq -> function
  | [] -> dq
  | (input,[(([],_),_)])::more -> (* scan is a special case *)
    let newParse = (input,q,p) in
      insertNewParses p new_p q (DQ.add newParse dq) more
  | (input,ics)::more -> (* in all other cases: we push ics onto iq with new_p *)
    let newParse = (input,(List.fold_left (fun iq i -> IQ.add i iq) q ics),new_p) in
      insertNewParses p new_p q (DQ.add newParse dq) more;;

(* EXTEND THE BEAM RECURSIVELY *)
let rec derive : lexArrays -> float -> DQ.t -> bool = fun lexAs min dq -> 
  if (DQ.is_empty dq)
  then false
  else 
    let ((input,iq,p),new_dq) = DQ.extract_min dq in
      Printf.fprintf stdout "-- just popped this DQ:\n"; printDQ lexAs dq;
      if IQ.is_empty iq && input=[] then true
      else if IQ.is_empty iq
      then derive lexAs min new_dq
      else
        let (ic,new_iq) = IQ.extract_min iq in
	let xs = exps lexAs input ic [] in
 (* should we check List.length xs > 0 ???? *)
	let new_p = p /. (float_of_int (List.length xs)) in
	  if new_p > min    (* only keep parses more probable than min *)
	  then derive lexAs min (insertNewParses p new_p new_iq new_dq xs)
	  else derive lexAs min new_dq;;

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
let input =  ["the";"queen";"prefers";"the";"wine"]
  in recognize mg0 "C" 0.001 input;;
let input =  ["the";"king";"knows";"the";"queen";"prefers";"the";"wine"]
  in recognize mg0 "C" 0.001 input;;

let input =  ["a";"a"]
  in recognize mgxx "T" 0.001 input;;
let input =  ["a";"b";"a";"b"]
  in recognize mgxx "T" 0.00001 input;;
*)

