open Tktree (* type tree is imported from Tktree *)
open Makeq (* for the priority queues *)

(* REPRESENTING GRAMMAR FOR HUMANS *)
type feature = Sel of string | Cat of string | Neg of string | Pos of string
type lexItem = string list * feature list
type g = lexItem list

(* REPRESENTING GRAMMAR WITH A TREE FOR THE RECOGNIZER *)
type ifeature = int * int (* type, value *)
type iLexItem = string list * ifeature list 
type lexTree = W of string list | N of (ifeature * lexTree list)
type sArray = string array  (* string values of the features *)
type tArray = int array  (* the types of each element of lexArray roots *)
type lexArray = lexTree list array  (* lexTrees *)
type lexArrays = sArray * lexArray * tArray

(* types for indexed constituents *)
type movers = lexTree list array  (* NB: same as lexArray *)
type cat = lexTree list * movers  (* intuitively:lexTree list=children of head *)
type ix = int list

(* derivation trees *)
type dnode = Nd of ix | Ld of (ix * iLexItem)  (* for each step of each derivation *)
type dtuple = ifeature list * ix * ifeature list array
type idtree =
    Li of string list * ifeature list
  | Xi of idtree * idtree
  | Oi of idtree
type derivationTree =
    L of string list * feature list
  | X of derivationTree * derivationTree
  | O of derivationTree
type iCat = cat * (ix * ix array) * dtuple

(* minIndex should only check indices of filled mover positions.
   No mover has an empty index, so we can ignore them. *)
val minIndex : 'a list -> 'a list array -> 'a list

(* for queues of predictions *)
module ICatPoset : sig type t = iCat val le : iCat -> iCat -> bool end
module IQ :
  sig
    type t = Makeq(ICatPoset).t
    val empty : t
    val is_empty : t -> bool
    val add : ICatPoset.t -> t -> t
    exception Empty
    val extract_min : t -> ICatPoset.t * t
    val merge : t -> t -> t
  end

(* for the derivation queue *)
type der = string list * IQ.t * float * dnode list   (* type of partial derivations *)
module DerivationPoset : sig type t = der val le : der -> der -> bool end

(* the queue of (partial) derivations *)
module DQ :
  sig
    type t = Makeq(DerivationPoset).t
    val empty : t
    val is_empty : t -> bool
    val add : DerivationPoset.t -> t -> t
    exception Empty
    val extract_min : t -> DerivationPoset.t * t
    val merge : t -> t -> t
  end

(* BEAUTIFY GRAMMAR: for practice and for tracing later *)
(* example: btfyFeat (Sel "D") *)
val btfyFeat : feature -> string

val joinStrings : string -> string list -> string   (* join is builtin in many programming langs *)
val stringFs : feature list -> string
val stringFFs : feature list list -> string
val spacedstring2 : string list -> string

(* example: btfyLexItem (["prefers"], [Sel "D"; Sel "D"; Cat "V"]);; *)
val btfyLexItem : lexItem -> string

(* example: showGrammar mg0;; *)
val showGrammar : g -> unit

(* split into (nonterminals,terminals)
     removing the type constructors and sorting each after reversing the indices.
   The indices are kept in reverse order in dq, since they are easier to build that way,
     and they are never needed by the recognizer itself. We reverse them just when
     we want to construct at tree to look at or interpret *)
val splitDnodes :
  int list list * (int list * iLexItem) list ->
  dnode list -> int list list * (int list * iLexItem) list
val child : 'a list * 'a list -> bool
val buildIDtreeFromDnodes :
  int list ->
  int list list * (int list * iLexItem) list ->
  idtree * int list list * (int list * iLexItem) list
val dnodes2idtree : dnode list -> idtree

(* REPRESENT GRAMMAR AS TREE *)
val stringVal : feature -> string

(* (ensureMember e list) adds e to list if not already there *)
val ensureMember : 'a -> 'a list -> 'a list

(* example 
let ss0 = stringValsOfG [] mg0;;
let (sA0:sArray) = Array.of_list (stringValsOfG [] mg0);;
*)
val stringValsOfG : string list -> g -> string list

(* example: let startInts0 = intsOfF sA0 (Cat startCat0);; *)
val intsOfF : sArray -> feature -> ifeature

(* example: btfyFeat (fOfInts sA0 (1,2));; *)
val fOfInts : sArray -> ifeature -> feature

(* with features in reverse order, extend lexTrees to encode lexical item *)
val revItemIntoLexTrees : sArray -> lexTree list * lexItem -> lexTree list

val rootLabel : lexTree -> ifeature
val subtreesOf : lexTree -> lexTree list
val termLabel : lexTree -> string list

(* for print out: *)
val lexTree2stringTree : sArray -> lexTree -> tree
val lexTrees2stringTrees : sArray -> lexTree list -> tree list

(* to get trees in the array, insert the root feature determined by the index*)
val lexArrays2lexTrees : lexArrays -> lexTree list

(* example 
revItemIntoLexTrees sA0 ([], (["says"], [Cat "V"; Sel "D"; Sel "C"]));; 
rootLabel (List.nth 
    (revItemIntoLexTrees sA0 ([], (["says"], [Cat "V"; Sel "D"; Sel "C"]))) 0);;
fOfInts sA0 (rootLabel (List.nth 
   (revItemIntoLexTrees sA0 ([], (["says"], [Cat "V"; Sel "D"; Sel "C"]))) 0));;
*)
val lexArrays2stringTrees : sArray * lexArray * tArray -> tree list

(* example 
let (sA0:sArray) = Array.of_list (stringValsOfG [] mg0)
let (lA0,tA0) = gIntoLexArrayTypeArray sA0 mg0
let lexArrays0 = (sA0,lA0,tA0)

let (sAxx:sArray) = Array.of_list (stringValsOfG [] mgxx)
let (lAxx,tAxx) = gIntoLexArrayTypeArray sAxx mgxx
let lexArraysxx = (sAxx,lAxx,tAxx)
*)
val gIntoLexArrayTypeArray : sArray -> g -> lexArray * tArray

(* PRINT FUNCTIONS FOR INDEXED CATEGORIES - optional, only needed for tracing*)
val btfyIndex : int list -> string
val deleteEmpties : tree list * string list -> tree list * string list

(* RECOGNIZER STEPS: SCAN/UNMERGE/UNMOVE *)
val emptyListArray : lexTree list array -> bool  (* true iff all empty lists *)
val terminalsOf : lexTree list -> lexTree list * lexTree list

(* Given (s,t) return (true,u) such that su=t, else (false,[])
    We could use exception instead of bool *)
val prefixT : string list * string list -> bool * string list

(* we could use exception instead of bool *)
val memberN : int -> lexTree list -> bool * lexTree

val scan :
  string list ->
  string list ->
  lexArray ->
  ix array ->
  dtuple -> (string list * iCat list) list -> (string list * iCat list) list
val merge1 :
  lexArray ->
  string list ->
  lexTree list ->
  int ->
  iCat -> (string list * iCat list) list -> (string list * iCat list) list
val merge2 :
  lexArray ->
  string list ->
  lexTree list ->
  int ->
  iCat -> (string list * iCat list) list -> (string list * iCat list) list

(* Movers enter the derivation with move1, continue with move2, land
   with merge3,4.  The begin to accumulate non-empty ancestor feature
   lists as soon as they enter the derivation, but only when they land
   do they correspond to a node in the derivation tree, and at that
   point they get an index from the parent of merge3,4.
*)
val merge3 :
  string list ->
  lexTree list ->
  int ->
  iCat ->
  int ->
  int -> (string list * iCat list) list -> (string list * iCat list) list
val merge4 :
  string list ->
  lexTree list ->
  int ->
  iCat ->
  int ->
  int -> (string list * iCat list) list -> (string list * iCat list) list
val move1 :
  lexArray ->
  string list ->
  lexTree list ->
  int ->
  iCat -> (string list * iCat list) list -> (string list * iCat list) list
val move2 :
  string list ->
  lexTree list ->
  int ->
  iCat ->
  int ->
  int -> (string list * iCat list) list -> (string list * iCat list) list

(* apply all possible rules to compute expansions of iCat *)
val exps :
  lexArrays ->
  string list ->
  iCat -> (string list * iCat list) list -> (string list * iCat list) list

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
val insertNewParses :
  string list ->
  float ->
  float ->
  IQ.t -> DQ.t -> dnode list -> (string list * ICatPoset.t list) list -> DQ.t

(* EXTEND THE BEAM RECURSIVELY *)
exception Not_Accepted
val derive : lexArrays -> float -> DQ.t -> (dnode list * DQ.t)

(* convert the derivation tree from integer features to (Sel string)-type features *)
val idtree2dtree : sArray -> idtree -> derivationTree

(* INITIALIZE AND BEGIN: create the initial derivation queue and parse -- 
  analyses with p<min are discarded *)
val parseDQ : sArray * lexArray * tArray -> float -> DQ.t -> derivationTree * DQ.t

(* derivation tree to string tree, with pairs at the leaves *)
val dt2t : derivationTree -> tree

val noOther : string -> feature list list -> bool
val selectUnique : string -> feature list list -> feature list list
val splitThird : 'a * 'b * 'c list -> 'a * 'b * 'c * 'c list
val pptree : tree -> unit
val pptrees : tree list -> unit
