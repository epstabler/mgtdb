open Str  (* for parsing the input *)
open Tktree (* tree display *)
open Mgtdbp (* the parser *)
open Dt2st (* conversion from derivation tree to state tree *)
open Dt2bts (* conversion from derivation tree to bare tree *)
open Dt2xbs (* conversion from derivation tree to X' tree *)
open Dt2vt (* conversion from derivation tree to value tree *)
open Dt2mcfg (* conversion from derivation tree to mcfg derivation tree *)

open G0

let time f x =
    let t = Sys.time() in
    let fx = f x in
    ( Printf.printf "parse found in %fs\n" (Sys.time() -. t); flush stdout ; fx; )

let rec process lexArrays tree =
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
	  process lexArrays tree; )
    else if List.length input > 0 && List.hd input = "q"
    then 0

    else if List.length input > 0 && List.hd input = "pd"
    then (pptree (dt2t tree); print_string "\n"; process lexArrays tree; )
    else if List.length input > 0 && List.hd input = "ps"
    then (pptree (st2t (dt2st tree)); print_string "\n";  process lexArrays tree; )
    else if List.length input > 0 && List.hd input = "px"
    then (pptree (xb2t (dt2xb tree)); print_string "\n";  process lexArrays tree; )
    else if List.length input > 0 && List.hd input = "pb"
    then (pptree (bt2t (dt2bt tree)); print_string "\n";  process lexArrays tree; )
    else if List.length input > 0 && List.hd input = "pv"
    then (pptree (vt2t (dt2vt tree)); print_string "\n";  process lexArrays tree; )
    else if List.length input > 0 && List.hd input = "pm"
    then (pptree (mcfg2t (dt2mcfg tree)); print_string "\n";  process lexArrays tree; )

    else if List.length input > 0 && List.hd input = "l"
    then (tktree (T (".", lexArrays2stringTrees lexArrays)); process lexArrays tree; )
    else if List.length input > 0 && List.hd input = "d"
    then (tktree (dt2t tree); process lexArrays tree; )
    else if List.length input > 0 && List.hd input = "s"
    then (tktree (st2t (dt2st tree)); process lexArrays tree; )
    else if List.length input > 0 && List.hd input = "x"
    then (tktree (xb2t (dt2xb tree)); process lexArrays tree; )
    else if List.length input > 0 && List.hd input = "b"
    then (tktree (bt2t (dt2bt tree)); process lexArrays tree; )
    else if List.length input > 0 && List.hd input = "v"
    then (tktree (vt2t (dt2vt tree)); process lexArrays tree; )
    else if List.length input > 0 && List.hd input = "m"
    then (tktree (mcfg2t (dt2mcfg tree)); process lexArrays tree; )
    else if List.length input > 0 && List.hd input = "n"
    then 1
    else if List.length input > 0 && List.hd input = ";"
    then -1
    else 1

let rec loop1 (sA,lA,tA) h0 ifs0 dx0 min0 dq0 linestring = (* parse and process result *)
  try
    let (tree,dq) = time (parseDQ (sA,lA,tA) min0) dq0 in
    let n = process (sA,lA,tA) tree in
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
      if List.length input > 0 && List.hd input = "l"
      then (tktree (T (".", lexArrays2stringTrees (sA,lA,tA))); loop0 (sA,lA,tA) h0 ifs0 dx0 min0 )
      else if List.length input > 0 && List.hd input = "q"
      then ()
      else
	let dq = (DQ.add (input,iq,prob,Nd []::[]) DQ.empty) in
	  loop1 (sA,lA,tA) h0 ifs0 dx0 min0 dq linestring

let go = fun lex min0 ->
  let (start:string) = startCat in
  let (sA:sArray) = Array.of_list (stringValsOfG [] lex) in
  let (lA,tA) = gIntoLexArrayTypeArray sA lex in
  let lAs: lexArrays = (sA,lA,tA) in
  let startInts: ifeature = intsOfF sA (Cat start) in
  let h = lA.(snd startInts) in
  let ifs: ifeature list = startInts::[] in
  let dx: ix = [] in
    loop0 lAs h ifs dx min0

let _ = go g0 0.00000001

