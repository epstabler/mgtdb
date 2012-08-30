(* mgtdbp.fsx comments welcome: stabler@ucla.edu
   Ported from OCaml to fsharp by Victor Chou: victorchou@ucla.edu
   This file extends mgtdbp-dev.ml to a parser, by keeping a tree in each partial analysis.
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
   Load with: #use "mgtdbp-dev.ml";;
   See examples at bottom of file

   * For cats that lack subtrees in lex, tA is not set. This does not matter,
     but we could probably get rid of tA altogether.
   * sA sets all features, but lA only needs non-empty lexTrees.
   * We might speed things up by numbering all subtrees of the lexArray,
     and using int list arrays to encode the whole lexTree.
*)

type tree = T of string * tree list  (* generic string tree *) 

let pptree tree =
    let rec tab n =
        if n <= 0
        then ()
        else
            (Printf.fprintf stdout " "; tab (n-1);) in
    let rec pptree_aux n = function
        T(s,ts) ->
            (Printf.fprintf stdout "\n"; tab n; 
             Printf.fprintf stdout "T(\"%s\",[" s;List.iter (pptree_aux (n+4)) ts;
             Printf.fprintf stdout "]);";) in 
            pptree_aux 0 tree

let pptrees = List.iter pptree

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

(* example: the copy language over {a,b} with startCatx="T"*)
let (mgxx:g) =[ 
([],[Cat "T"; Neg "r"; Neg "l"]); ([],[Sel "T"; Pos "r"; Pos "l"; Cat "T"]);
(["a"],[Sel "T"; Pos "r"; Cat "A"; Neg "r"]); (["b"],[Sel "T"; Pos "r"; Cat "B"; Neg "r"]); 
(["a"],[Sel "A"; Pos "l"; Cat "T"; Neg "l"]); (["b"],[Sel "B"; Pos "l"; Cat "T"; Neg "l"]); 
]

(* BEAUTIFY GRAMMAR: for practice and for tracing later *)
let btfyFeat: feature -> string = 
    function Sel x -> "="+x | Cat x -> x | Pos x -> "+"+x | Neg x -> "-"+x
(* example: btfyFeat (Sel "D");; *)

let rec joinStrings sep = function  (* join is builtin in many programming langs *)
    | [] -> ""
    | s::[] -> s
    | s::t::more -> s+sep+(joinStrings sep (t::more))

let rec stringFs fs = joinStrings " " (List.map btfyFeat fs)
let rec stringFFs ffs = joinStrings "," (List.map stringFs ffs)
let spacedstring2 = function [] -> "e" | l -> joinStrings " " l

let btfyLexItem: lexItem -> string = fun (ss,fs) ->
    (joinStrings " " ss)+"::"+(joinStrings " " (List.map btfyFeat fs))
(* example: btfyLexItem (["prefers"], [Sel "D"; Sel "D"; Cat "V"]);; *)

let showGrammar: g -> unit =
    List.iter (fun x -> printf "%s" (btfyLexItem x); printf "\n")
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
    | L of string list * feature list
    | X of derivationTree * derivationTree
    | O of derivationTree

type iCat = cat * (ix * ix array) * dtuple

(* minIndex should only check indices of filled mover positions.
   No mover has an empty index, so we can ignore them. *)
let minIndex sofar =
    Array.fold (fun x y -> if y<>[] then (min x y) else x) sofar
(* example: minIndex [1] [|[0];[]|];;  (* this is [0] *) *)

(* for queues of predictions *)
module ICatPoset = (* order by comparing least index, an int list *)
    type t = iCat
    let le = fun ((_,(i1,a1),_):iCat) ((_,(i2,a2),_):iCat) -> 
        compare (minIndex i1 a1) (minIndex i2 a2) < 1

(*** priorityQ.ml
creator: Jean-Christophe.Filliatre@lri.fr, based on Okasaki
source: https://groups.google.com/group/fa.caml/msg/526cc7a4a9adc664?dmode=source
date: 30 Jun 2011 20:14:34 +0200
adapted for F# by: E Stabler, 23 July 2012 10:05:09
****)
module IQ =    (* for the queues of predictions *)
    type qtype = ICatPoset.t (* set this to whatever type you want *)

    type qt = QE | QT of int * qtype * qt * qt

    exception QEmpty

    let qrank = function QE -> 0 | QT (r,_,_,_) -> r

    let qmake x a b =
        let ra = qrank a in
        let rb = qrank b in
        if ra >= rb then QT (rb + 1, x, a, b) else QT (ra + 1, x, b, a)

    let emptyQ = QE

    let qis_empty = function QE -> true | QT _ -> false

    let rec qmerge h1 h2 =
        match h1,h2 with
        | QE, h | h, QE -> h
        | QT (_,x,a1,b1), QT (_,y,a2,b2) ->
            if ICatPoset.le x y  (* replace with approp comparison function as necessary *)
            then qmake x a1 (qmerge b1 h2)
            else qmake y a2 (qmerge h1 b2)

    let qadd x h = qmerge (QT (1, x, QE, QE)) h

    let qextract_min = function
        | QE -> raise QEmpty
        | QT (_,x,a,b) -> x, qmerge a b

(*** END priorityQ.ml ****)
(* sample session with fsharp interpreter:
 //assuming qtype = int and comparison function is int x <= y

> let q0 = qadd 3 emptyQ;;

val q0 : qt = QT (1,3,QE,QE)

> let q1 = qadd 17 q0;;

val q1 : qt = QT (1,3,QT (1,17,QE,QE),QE)

> let q2 = qadd 1 q1;;

val q2 : qt = QT (1,1,QT (1,3,QT (1,17,QE,QE),QE),QE)

> let (min1,q3) = qextract_min q2;;

val q3 : qt = QT (1,3,QT (1,17,QE,QE),QE)
val min1 : qtype = 1

> let (min2,q4) = qextract_min q3;;

val q4 : qt = QT (1,17,QE,QE)
val min2 : qtype = 3

> let (min3,q5) = qextract_min q4;;

val q5 : qt = QE
val min3 : qtype = 17

*)

(* split into (nonterminals,terminals)
     removing the type constructors and sorting each after reversing the indices.
   The indices are kept in reverse order in dq, since they are easier to build that way,
     and they are never needed by the recognizer itself. We reverse them just when
     we want to construct at tree to look at or interpret *)
let rec splitDnodes:
    (int list list * (int list * iLexItem) list) -> dnode list -> 
    (int list list * (int list * iLexItem) list) = fun (n,t) -> function 
        | [] -> List.sortWith compare n,List.sortWith compare t
        | Nd i::ts -> splitDnodes (List.rev i::n,t) ts
        | Ld (i,lex)::ts -> splitDnodes (n,(List.rev i,lex)::t) ts

let rec child = function 
    | ([],_::[]) -> true
    | (x::xs,y::ys) when x=y -> child (xs,ys)
    | _ -> false

let rec buildIDtreeFromDnodes: int list -> (int list list * (int list * iLexItem) list) ->
    idtree * int list list * (int list * iLexItem) list = fun parent -> function
        | (i0::nonterms,terms) when child (parent,i0) -> 
            let (child0,nts0,ts0) = buildIDtreeFromDnodes i0 (nonterms,terms) in 
            (match (nts0,ts0) with
                | (i1::nts,ts) when child (parent,i1) -> 
                    let (child1,nts1,ts1) = buildIDtreeFromDnodes i1 (nts,ts) in 
                    Xi (child0,child1), nts1, ts1
                | _ -> Oi child0, nts0, ts0;)
        | (nonterms,(j,(s,f))::terms) when parent=j -> Li (s,f), nonterms, terms
        | _ -> failwith "error: buildIDtreesFromDnodes"

let dnodes2idtree dns = 
    let (nts,ts) = splitDnodes ([],[]) dns in 
    let (dt,nts1,ts1) = buildIDtreeFromDnodes (List.head nts) (List.tail nts,ts) in
        if nts1=[] && ts1=[] then dt else failwith "error2: dnodes2idtree"

(* for the derivation queue *)
type der = string list * IQ.qt * float * dnode list  (* type of partial derivations *)

module DerivationPoset = (* order by (decreasing) probability, a float *)
    type t = der
    let le = fun ((_,_,p1,_):der) ((_,_,p2,_):der) -> compare p2 p1 < 1
(*** priorityQ.ml
creator: Jean-Christophe.Filliatre@lri.fr, based on Okasaki
source: https://groups.google.com/group/fa.caml/msg/526cc7a4a9adc664?dmode=source
date: 30 Jun 2011 20:14:34 +0200
adapted for F# by: E Stabler, 23 July 2012 10:05:09
****)
module DQ =  (* the queue of (partial) derivations *)
    type qtype = DerivationPoset.t (* set this to whatever type you want *)

    type qt = QE | QT of int * qtype * qt * qt

    exception QEmpty

    let qrank = function QE -> 0 | QT (r,_,_,_) -> r

    let qmake x a b =
        let ra = qrank a in
        let rb = qrank b in
        if ra >= rb then QT (rb + 1, x, a, b) else QT (ra + 1, x, b, a)

    let emptyQ = QE

    let qis_empty = function QE -> true | QT _ -> false

    let rec qmerge h1 h2 =
        match h1,h2 with
        | QE, h | h, QE -> h
        | QT (_,x,a1,b1), QT (_,y,a2,b2) ->
            if DerivationPoset.le x y  (* replace with approp comparison function as necessary *)
            then qmake x a1 (qmerge b1 h2)
            else qmake y a2 (qmerge h1 b2)

    let qadd x h = qmerge (QT (1, x, QE, QE)) h

    let qextract_min = function
        | QE -> raise QEmpty
        | QT (_,x,a,b) -> x, qmerge a b

(*** END priorityQ.ml ****)

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

(* example 
let ss0 = stringValsOfG [] mg0;;
let (sA0:sArray) = Array.ofList (stringValsOfG [] mg0);;
*)

let intsOfF: sArray -> feature -> ifeature = fun sA -> 
    let arrayNth: sArray -> string -> int = fun sA s ->
        let rec arrayNth0 i n s =
            if i<n 
            then (if sA.[i]=s then i else arrayNth0 (i+1) n s)
            else failwith ("error: arrrayNth sA "+s) in
            arrayNth0 0 (Array.length sA) s in
            function
                | Cat s -> 0,arrayNth sA s
                | Sel s -> 1,arrayNth sA s
                | Neg s -> 2,arrayNth sA s
                | Pos s -> 3,arrayNth sA s

(* example: let startInts0 = intsOfF sA0 (Cat startCat0);; *)

let fOfInts: sArray -> ifeature -> feature = fun sA (i,j) ->
    if i=0 then Cat sA.[j]
    elif i=1 then Sel sA.[j]
    elif i=2 then Neg sA.[j]
    elif i=3 then Pos sA.[j]
    else failwith "error: fOfInts"

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
        | t::trees,(w,fs) -> t::revItemIntoLexTrees sA (trees,(w,fs))

let rootLabel = function N(f,_) -> f | _ -> failwith "error: rootLabel"
let subtreesOf = function N(_,ts) -> ts | _ -> failwith "error:subtreesOf"
let termLabel = function W s -> s | _ -> failwith "error: termLabel"

(* for print out: *)
let rec lexTree2stringTree: sArray -> lexTree -> tree = fun sA -> function
    | N (ij,ts) -> T (btfyFeat (fOfInts sA ij),lexTrees2stringTrees sA ts)
    | W w -> T (joinStrings " " w,[])
and lexTrees2stringTrees: sArray -> lexTree list -> tree list = fun sA -> function
    | [] -> []
    | t::ts -> lexTree2stringTree sA t::lexTrees2stringTrees sA ts

(* to get trees in the array, insert the root feature determined by the index*)
let lexArrays2lexTrees: lexArrays -> lexTree list = fun (sA,lA,tA) ->
    let rec totrees i res =
        if i < 0 then res else totrees (i - 1) (N ((tA.[i],i),lA.[i]) :: res) in
        totrees (Array.length lA - 1) []

let lexArrays2stringTrees (sA,lA,tA) =
    lexTrees2stringTrees sA (lexArrays2lexTrees (sA,lA,tA))

(* example 
revItemIntoLexTrees sA0 ([], (["says"], [Cat "V"; Sel "D"; Sel "C"]));; 
rootLabel (List.nth 
            (revItemIntoLexTrees sA0 ([], (["says"], [Cat "V"; Sel "D"; Sel "C"]))) 0);;
fOfInts sA0 (rootLabel (List.nth 
                (revItemIntoLexTrees sA0 ([], (["says"], [Cat "V"; Sel "D"; Sel "C"]))) 0));;
*)

let gIntoLexArrayTypeArray: sArray -> g -> lexArray * tArray = fun sA g -> 
    let trees = List.fold (fun ts (w,fs) -> 
        revItemIntoLexTrees sA (ts,(w,List.rev fs))) [] g in
    let (lA:lexTree list array) = Array.create (Array.length sA) [] in
    let (tA:int array) = Array.create (Array.length sA) 0 in
        (List.iter (fun t -> 
            if lA.[snd (rootLabel t)] = []
            then (lA.[snd (rootLabel t)] <- subtreesOf t;
                tA.[snd (rootLabel t)] <- fst (rootLabel t); )
            else failwith "gIntoLexArray: Cat and Neg values must be disjoint" ) trees;
                (lA,tA); )
(* example 
let (lA0,tA0) = gIntoLexArrayTypeArray sA0 mg0;;
let lexArrays0 = (sA0,lA0,tA0);;
*)

(* example: display the trees
pptree (T (".", lexArrays2stringTrees lexArrays0));;
*)

(* PRINT FUNCTIONS FOR INDEXED CATEGORIES - optional, only needed for tracing*)
let btfyIndex = List.fold (fun l (i:int) -> l+(string i)) ""

let rec deleteEmpties: tree list * string list -> tree list * string list = function
    | [],[] -> [],[]
    | T(_,[])::ts,_::is -> deleteEmpties (ts,is)
    | t::ts,i::is -> let (us,js) = deleteEmpties (ts,is) in t::us,i::js
    | _ -> failwith "error: deleteEmpties"

let rec printCatI = function
    | t::ts,i::is -> pptree t ; printf "%s" i; printf "%s" " ,"; printCatI (ts,is)
    | [],[] -> ()
    | _ -> failwith "error: printCatI"

let printIcat: lexArrays -> iCat -> unit = fun (sA,lA,tA) ((h,mA),(ih,iA),d) ->
    let ihS = btfyIndex ih in (* d is for dtree, ignored here *) 
    let iASs = List.map btfyIndex  (Array.toList iA) in
    let hTs =  lexTrees2stringTrees sA h in
    let mTs = lexArrays2stringTrees (sA,mA,tA) in
    (List.iter pptree hTs; printf "%s" ihS; printf "%s" " ,";
        printCatI (deleteEmpties (mTs,iASs)); printf "%s" "\n"; )

//TYPO hERE FOR EXAMPLE?
(* example 
let ic0:iCat = 
    let h = lA0.[0] in  (* the verb subtree *)
    let m = Array.create (Array.length sA0) [] in
    let mx = Array.create (Array.length sA0) [] in
    let dx = ([],[],(Array.create (Array.length sA0) [])) in
    let hx = [0;1;1] in
    (m.[2] <- lA0.[2];   (* the which subtree *)
        mx.[2] <- [0;0];    
        ((h,m),(hx,mx),dx); );;

printIcat lexArrays0 ic0;;
*)

let rec printIQ = fun lexArrays iq0 ->
        if IQ.qis_empty iq0
        then ()
        else let (ic,iq1) = IQ.qextract_min iq0 in 
                printIcat lexArrays ic; Printf.fprintf stdout " ... end ic";
                printIQ lexArrays iq1

(*
let iq0 = IQ.qadd ic0 IQ.emptyQ;;
let iq0 = IQ.qadd ic0 iq0;;
printIQ lexArrays0 iq0;;
*)

let rec printDQ lexArrays dq0 =
    if DQ.qis_empty dq0
    then ()
    else let ((input,iq,p,dt),dq1) = DQ.qextract_min dq0 in 
            Printf.fprintf stdout "%f: [%s]" p (joinStrings "," input);
            printIQ lexArrays iq; Printf.fprintf stdout " ---- end parse\n";
            printDQ lexArrays dq1

(* example 
let dq0 = DQ.qadd (["this";"is";"the";"input"],iq0,0.1,[]) DQ.emptyQ;;
printDQ lexArrays0 dq0;;
let dq0 = DQ.qadd (["this";"is";"the";"input"],iq0,0.1,[]) dq0;;
printDQ lexArrays0 dq0;;
*)

(* RECOGNIZER STEPS: SCAN/UNMERGE/UNMOVE *)
let emptyListArray: lexTree list array -> bool = (* true iff all empty lists *)
    Array.fold (fun b x -> (x=[] && b)) true

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

let scan : string list -> string list -> lexArray -> ix array -> dtuple ->
    (string list * iCat list) list -> (string list * iCat list) list =
            fun w input m mx dt sofar ->
                if emptyListArray m //if movers empty can scan
                then let (ok,_) = prefixT (w,input) in //if at front of input, can scan
                        if ok then (w,[(([],m),([],mx),dt)])::sofar else sofar
                else sofar

let merge1: lexArray -> string list -> lexTree list -> int -> iCat -> 
    (string list * iCat list) list -> (string list * iCat list) list =
            fun lA input terms i ((_,m),(hx,mx),(ifs,dx,mifs)) sofar ->
                if terms <> []
                then //create empty mover list and mover indices list
                    let empty_m: lexTree list array = Array.create (Array.length m) [] in
                    let empty_mx: ix array  = Array.create (Array.length mx) [] in
                    ([],[((terms,empty_m),(hx@[0],empty_mx),((1,i)::ifs,0::dx,mifs));   
                        ((lA.[i],m),(hx@[1],mx),((0,i)::[],1::dx,mifs)) (* movers passed to complement *)
                        ])::sofar
                else sofar

let merge2: lexArray -> string list -> lexTree list -> int -> iCat -> 
    (string list * iCat list) list -> (string list * iCat list) list =
            fun lA input nonterms i ((_,m),(hx,mx),(ifs,dx,mifs)) sofar ->
                if nonterms <> []
                then
                    let empty_m: lexTree list array = Array.create (Array.length m) [] in
                    let empty_mx: ix array  = Array.create (Array.length mx) [] in
                    ([],[((nonterms,m),(hx@[1],mx),((1,i)::ifs,0::dx,mifs));
                        ((lA.[i],empty_m),(hx@[0],empty_mx),((0,i)::[],1::dx,mifs))
                        ])::sofar
                else sofar

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
                then //continue changed to continueMerge
                    let continueMerge = merge3 input terms i ((h,m),(hx,mx),(ifs,dx,mifs)) (next+1) stop sofar in
                    let (ok,matchingTree) = memberN i m.[next] in  (* if match, it's unique *)
                    if ok
                    then 
                        let ts = subtreesOf matchingTree in 
                        let tsx = mx.[next] in
                        let ifs0 = mifs.[next] in
                        let empty_m: lexTree list array = Array.create (Array.length m) [] in
                        let empty_mx: ix array  = Array.create (Array.length mx) [] in
                        let empty_ifs: ifeature list array  = Array.create (Array.length mifs) [] in
                        let n = Array.copy m in
                        let nx = Array.copy mx in
                        let nifs = Array.copy mifs in
                        (n.[next] <- [];  (* we used the "next" licensee, so now empty *)
                        nx.[next] <- [];
                        nifs.[next] <- [];
                        ([],[((terms,empty_m),(hx,empty_mx),((1,i)::ifs,0::dx,empty_ifs));
                            ((ts,n),(tsx,nx),((0,i)::ifs0,1::dx,nifs))  (* movers passed to complement *)
                            ])::continueMerge; ) //continue changed to continueMerge
                    else continueMerge //continue changed to continueMerge 
                else sofar

let rec merge4: string list -> lexTree list -> int -> iCat -> int -> int ->
    (string list * iCat list) list -> (string list * iCat list) list =
            fun input nonterms i ((h,m),(hx,mx),(ifs,dx,mifs)) next stop sofar ->
                if nonterms <> [] && next < stop
                then  //continue changed to continueMerge
                    let continueMerge = merge4 input nonterms i ((h,m),(hx,mx),(ifs,dx,mifs)) (next+1) stop sofar in
                    let (ok,matchingTree) = memberN i m.[next] in  (* if match, it's unique *)
                    if ok
                    then 
                        let ts = subtreesOf matchingTree in 
                        let tsx = mx.[next] in
                        let ifs0 = mifs.[next] in
                        let empty_m: lexTree list array = Array.create (Array.length m) [] in
                        let empty_mx: ix array  = Array.create (Array.length mx) [] in
                        let n = Array.copy m in
                        let nx = Array.copy mx in
                        let nifs = Array.copy mifs in
                        (n.[next] <- [];  (* we used the "next" licensee, so now empty *)
                        nx.[next] <- [];
                        nifs.[next] <- [];
                        ([],[((nonterms,n),(hx,nx),((1,i)::ifs,0::dx,mifs));  (* remaining movers kept in head *)
                            ((ts,empty_m),(tsx,empty_mx),((0,i)::ifs0,1::dx,nifs))
                            ])::continueMerge; )  //continue changed to continueMerge
                    else continueMerge  //continue changed to continueMerge
                else sofar

let move1: lexArray -> string list -> lexTree list -> int -> iCat -> 
    (string list * iCat list) list -> (string list * iCat list) list =
            fun lA input ts i ((_,m),(hx,mx),(ifs,dx,mifs)) sofar -> 
                if m.[i]=[]                           (* SMC *)
                then 
                    let n = Array.copy m in
                    let nx = Array.copy mx in
                    let nifs = Array.copy mifs in
                    (n.[i] <- lA.[i];
                    nx.[i] <- hx@[0];
                    nifs.[i] <- (2,i)::[];
                    ([],[((ts,n),(hx@[1],nx),((3,i)::ifs,0::dx,nifs))])::sofar; )
                else sofar

let rec move2: string list -> lexTree list -> int -> iCat -> int -> int ->
    (string list * iCat list) list -> (string list * iCat list) list =
            fun input ts i ((h,m),(hx,mx),(ifs,dx,mifs)) next stop sofar ->
                if next < stop
                then //continue changed to continueMove
                    let continueMove = move2 input ts i ((h,m),(hx,mx),(ifs,dx,mifs)) (next+1) stop sofar in
                    let (ok,matchingTree) = memberN i m.[next] in  (* if match, it's unique *)
                    let (rootType,rootF) = rootLabel matchingTree in
                    if ok && (rootF=next || m.[rootF]=[]) (* SMC *)
                    then 
                        let mts = subtreesOf matchingTree in 
                        let mtsx = mx.[next] in
                        let ifs0 = mifs.[next] in
                        let n = Array.copy m in
                        let nx = Array.copy mx in
                        let nifs = Array.copy mifs in
                        (n.[next] <- [];  (* we have used the "next" licensee, so now empty *)
                        nx.[next] <- [];
                        nifs.[next] <- [];
                        n.[rootF] <- mts;
                        nx.[rootF] <- mtsx;
                        nifs.[next] <- (3,i)::ifs0;
                        ([],[((ts,n),(hx,nx),((2,i)::ifs,0::dx,nifs))])::continueMove; ) //continue changed to continueMove
                    else continueMove //continue changed to continueMove
                else sofar

(* apply all possible rules to compute expansions of iCat *)
let rec exps : lexArrays -> string list -> iCat -> 
    (string list * iCat list) list -> (string list * iCat list) list = 
            fun (sA,lA,tA) input ((h,m),(hx,mx),dt) sofar ->
                match h with
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

(* example
let ic0:iCat = 
    let h = lA0.[1] in
    let m = Array.create (Array.length sA0) [] in
    let mx = Array.create (Array.length sA0) [] in
    let dx = ([],[],(Array.create (Array.length sA0) [])) in
    let hx = [] in
    ((h,m),(hx,mx),dx);;
printIcat lexArrays0 ic0;;

let input = ["the";"wine"] in

//NO STRINg INPUT FROM ThIS RESPONSE?
    printIcat lexArrays0 ic0;
    printf "%s" "===== =====\n";
    exps lexArrays0 input ic0 [];;

let iq0 = IQ.qadd ic0 IQ.emptyQ;;
printIQ lexArrays0 iq0;;
let input = ["the";"wine"] in
let prob = 1. in
let dq0 = DQ.qadd (input,iq0,prob,[]) DQ.emptyQ in
printDQ lexArrays0 dq0;;
*)

let rec insertNewParses: string list -> float -> float -> IQ.qt -> DQ.qt -> dnode list ->
    (string list * iCat list) list -> DQ.qt = 
        fun input p new_p q dq dns -> function
            | [] -> dq
            | (w,[(([],_),_,(ifs,dx,_))])::more -> (* scan is a special case *)
                let (ok,remainder) = prefixT (w,input) in
                if ok
                then
                    let newParse = (remainder,q,p,Ld (dx,(w,ifs))::dns) in
                    insertNewParses input p new_p q (DQ.qadd newParse dq) dns more
                else failwith "error1: insertNewParses"
            | ([],ics)::more -> (* in all other cases: we push ics onto iq with new_p *)
                let new_q = List.fold (fun iq i -> IQ.qadd i iq) q ics in
                let new_dns = List.fold (fun l (_,_,(_,x,_)) -> Nd x::l) dns ics in
                let newParse = (input,new_q,new_p,new_dns) in 
                insertNewParses input p new_p q (DQ.qadd newParse dq) dns more
            | _ -> failwith "error2: insertNewParses"

(* EXTEND THE BEAM RECURSIVELY *)
let rec derive : lexArrays -> float -> DQ.qt -> dnode list = fun lexAs min dq -> 
    if (DQ.qis_empty dq)
    then failwith "not accepted"
    else 
        let ((input,iq,p,dns),new_dq) = DQ.qextract_min dq in
        Printf.fprintf stdout "-- just popped this DQ:\n"; printDQ lexAs dq;
        if IQ.qis_empty iq && input=[] then dns
        elif IQ.qis_empty iq then derive lexAs min new_dq
        else
            let (ic,new_iq) = IQ.qextract_min iq in
            let xs = exps lexAs input ic [] in
            let new_p = p / (float (List.length xs)) in
            if new_p > min    (* only keep parses more probable than min *)
            then derive lexAs min (insertNewParses input p new_p new_iq new_dq dns xs)
            else derive lexAs min new_dq

(* convert the derivation tree from integer features to (Sel string)-type features *)
let rec idtree2dtree: sArray -> idtree -> derivationTree = fun sA -> function
    | Li (ss,ifs) -> L (ss,List.map (fOfInts sA) ifs)
    | Oi t -> O (idtree2dtree sA t)
    | Xi (t1,t2) -> X (idtree2dtree sA t1,idtree2dtree sA t2)

(* INITIALIZE AND BEGIN: create the initial derivation queue and parse -- 
  analyses with p<min are discarded *)
let parse: g -> string -> float -> string list -> derivationTree = fun lex start min input ->
    let (sA:sArray) = Array.ofList (stringValsOfG [] lex) in
    let (lA,tA) = gIntoLexArrayTypeArray sA lex in
    let lAs: lexArrays = (sA,lA,tA) in
    let startInts: ifeature = intsOfF sA (Cat start) in
    let h = lA.[snd startInts] in
    let m: lexArray = Array.create (Array.length sA) [] in
    let mx: ix array = Array.create (Array.length sA) [] in
    let ifs: ifeature list = startInts::[] in
    let dx: ix = [] in
    let mifs: ifeature list array = Array.create (Array.length sA) [] in
    let dt: dtuple = (ifs,dx,mifs) in
    let ic: iCat = ((h,m),([],mx),dt) in
    let iq  = IQ.qadd ic IQ.emptyQ in
    let prob: float = 1. in
    let dq = (DQ.qadd (input,iq,prob,Nd []::[]) DQ.emptyQ) in
    idtree2dtree sA (dnodes2idtree (derive lAs min dq))
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
    | L (string,fs) ->  T("("+(spacedstring2 string)+","+(stringFs fs)+")",[])
    | X (d1,d2) -> T ("*",[dt2t d1;dt2t d2])
    | O (d) -> T ("o",[dt2t d])

(*
let input =  ["the";"king"]
    in dt2t (parse mg0 "D" 0.001 input);;
let input =  ["the";"king";"prefers";"the";"wine"]
    in dt2t (parse mg0 "C" 0.001 input);;
let input =  ["the";"king";"knows";"the";"queen";"prefers";"the";"wine"]
    in dt2t (parse mg0 "C" 0.001 input);;
let input =  ["the";"king";"knows";"which";"wine";"the";"queen";"prefers"]
    in dt2t (parse mg0 "C" 0.001 input);;
let input =  ["a";"b";"a";"b"]
    in dt2t (parse mgxx "T" 0.0001 input);;
*)
