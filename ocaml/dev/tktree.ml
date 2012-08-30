(*  file: tktree.ml
    date: September 2009
 creator: Denis Paperno
    updated from Paperno's treedraw7.ml by ES
    rewritten by Floris van Vugt, August 2010

    the drawing algorithm is inspired heavily by the beautiful article:
    Andrew J. Kennedy, Functional Pearls - Drawing Trees,
    J. Functional Programming 6 (3): 527–534, May 1996

    LOAD WITH labltk, or compile with something like this:
      ocamlc -I +labltk labltk.cma treedraw.ml -o treedraw
    or if you have an optimized version on your platform, something like this
      ocamlopt -ccopt -O3 -I +labltk labltk.cmx treedraw.ml -o treedraw
*)

type tree = T of string * (tree list);;

(* Some configuration constants *)
(*let label_font = "-*-times-bold-r-normal--32-120-*";; *) (* large font *)
let label_font = "-*-times-bold-r-normal--12-120-*";; (* small font *)
(* NB: if you want to change the label font, beware that this particular
   font whose name is mentioned here will be used for determining the width
   of labels in the tree. That will make sure the tree is drawn correctly 
   on the canvas. But then when we export the tree to PS or LaTeX, if the font
   is too small or too large, spacing issues will ensue (since we cannot 
   determine the width of a string in PS or LaTeX directly). *)


(* Space that gets added on both sides of a label, 
   so that it doesn't touch the adjacent label.
   (So the minimal horizontal distance between 
   two labels will be twice this)
*)
let horizontal_padding = 5.;; 

(* The vertical height of a "level" in the tree.
   That is, the difference in y-coordinate between one node
   and the next. So in this height is included the label height,
   the line height, and extra padding space. *)
let tree_line_interval = 22.;; 

(* the height of the labels on the nodes of the tree.
   We skip that much horizontal space from the top of the label
   until we start drawing the line connecting the node to her daughters. *)
let tree_node_label_height = 15.;;

(* the empty vertical space between the end of a connecting line and the label of the
   node that it connects to.
   A negative means that we actually make the line go through the top of the label
   for a bit (since the top of the label may have a lot of empty space already). *)
let tree_line_padding = 0.;;

(* How far the label coordinates are lowered in LaTeX to 
   compensate for the fact that LaTeX draws the label above this
   point rather than below *)
let latex_label_vertical_offset = 10.;;

(* Same as latex_label_vertical_offset but then for the PostScript output. *)
let postscript_label_vertical_offset = 11.;;

(* The contents of a tree: just the labels and structure ALREADY DEFINED
type tree = T of string * (tree list);;
*)

(*example of a minimal tree
T ("root",[]);;
T("S",[T("NP",[T("Mary",[])]); T("VP",[T("sings",[])])]);;
*)

(* 
   Now this is a tree that is ready to be drawn on the screen.
   Every node has a horizontal position associated with it, and a list
   of descendents.
   NB: This horizontal position is *relative to its parent*, so for example
   D("child",-50,[]) means that it will be 50px to the left relative to its 
   parent.
*)
type drawTree = D of string * float * (drawTree list);;

(* Concate a list of strings into a single string *)
(* Return a drawtree as a readable string *)
let drawTree_as_string tr =
  let implode strl = List.fold_left (^) "" strl in
  let rec as_string prefix tr =
    match tr with
	D (lab,x,descedents) -> 
	  (prefix^lab^" ("^(string_of_float x)^")\n"
	   ^(implode (List.map (as_string (prefix^"\t")) descedents)))
  in as_string "" tr;;

(* Returns the maximum depth of a tree d *)
let rec maxDepth d =
  let rec maxDepthOfList ds =
    match ds with 
	[] -> 0
      | t::ts -> max (maxDepth t) (maxDepthOfList ts)
  in
    match d with
	T (_,descendents) -> ((maxDepthOfList descendents)+1) 

(* We shift a drawtree by dx pixels to the right. *)
let rec shiftTree ((D (lab,x,desc)),dx) = D (lab,x+.dx,desc);;
(* That's easy! That's because we have defined the horizontal coordinates
   relative to the parent node. *)

(* Ok, so we will be computing something like the contour of the tree.
  That is, line per line the horizontal interval that the tree covers in that
  line. So for example, the tree:
  
            a
           / \
          b   c
              |
              d

  Will have a contour something like this 
  (each line corresponds to a line of labels
  in the tree):
  
           <=>
         <=====>
             <=>

  So in list form, it will look something like:
  [ (-1,1); (-2,2); (0,2) ]

*)

(* 
   This is a list that specifies at each level of depth of the tree
   the horizontal interval that it takes up. This is important to know
   so that we can make sure that adjacent trees don't intersect. 
   NB: Kennedy (1996) calls a contour "extent".
*)
type contour = (float * float) list;;

(* Write a contour (list of horizontal intervals) as a readable string,
   just for debugging. *)
let contour_as_string contour = 
  let rec contour_contents cont = 
    match cont with 
	[] -> ""
      | (l,r)::rest -> ("("^(string_of_float l)^", "^(string_of_float r)^"); "^(contour_contents rest))
  in ("["^(contour_contents contour)^"]");;

(* Shift all the intervals in the contour dx pixels to the right *)
let rec shiftContour (contour,dx) = 
  List.map (fun (l,r) -> (l+.dx,r+.dx)) contour;;

(* Merge two contours a and b, assuming
   that they do not overlap, and that a is always
   to the left of b. *)
let rec mergeContours a b =
  match (a,b) with
    | ([],rest) -> rest
    | (rest,[]) -> rest
    | ((aL,_)::aRest,
       (_,bR)::bRest) -> (aL,bR)::(mergeContours aRest bRest);;

(* Given a list of contours, none overlapping, 
   and given in their left-to-right order, we compute
   the "umbrella" contour that envelopes them all. *)
let rec umbrellaContour contours =
  List.fold_right mergeContours contours [];;


(* Produces the mirror image of the contour (mirrorred over the vertical axis)*)
let mirrorContour contour =
  List.map (fun (x,y) -> (-.y,-.x)) contour;;

(* 
   Given two contours, find the minimal distance between them
   that will make sure they do not overlap on any of their levels.

   Example: suppose we have two trees who have the following contours:
   (dots indicate empty space and 0 indicates what is called zero in their
   intervals)
   
   |0|            |0|
   <=>            <===>
   ..<==>           <==>
   ....<=>           <=>

   Then this is the minimal distance that will make them not overlap:

   |0| |0|
   <=> <===>
   ..<==><==>
   ....<=><=>

   We return this minimal distance as an int number referring to the 
   shift required in their null points.
   (4 in our example)
*)

let rec minimalDistance a b =
  (* a and b are contours (i.e. outlines of the horizontal
     space that trees take up). Returns how far b needs to be
     shifted (minimally) to the right so as to not overlap
     with a *)

  match (a,b) with
      (* If one of the two contours
	 has no entries on this level,
	 then their minimal distance is zero,
	 meaning no shift is required. *)
      (_ ,[]) -> 0. 
    | ([],_ ) -> 0.

    (* If they both take up space on this level,
       then we figure out how far they need to be apart. *)
    | ((_,aR)::aNext,(bL,_)::bNext) ->
	(* aL is how far contour "a" sticks out on the left on this level,
	   aR is how far contour "a" sticks out on the right on this level *)
	( max (aR-.bL) (minimalDistance aNext bNext) )
;;


(* A little test for this function: *)
(*
let aCont = [ (-10,10); (0,20); (0,10); (-100,500) ];;
let bCont = [ (-5,  5);(-5, 5); (-15,5) ];;
minimalDistance aCont bCont;;
*)

(* 
   Given a list of contours, we compute how to
   merge them together. That is, we return a list of ints that
   represent how far each contour is shifted to obtain a good fit.
   
   NB: this way of fitting proceeds from left to right, so all trees
   will be as much to the left as possible. In order to have "free" moving
   branches center, we have to compute the fits also from right to left,
   and then take the average of these positions (see Kennedy 1996).
   
   We return for each contour how far it is shifted to fit in with the rest
*)
let fitContoursL contours =

  (* This function handles the inner recursion.
     It passes on a list with all the shifts, to
     which we simply need to append the current
     shift we are making. 

     - accumulated = the big contour consisting of all the previously
     merged contours
     - cs = a list of contours we still want to merge to the
     accumulated ones.
  *)
  let rec doMerges accumulated cs =
      
      match cs with
	  []         -> [] (* no shifts required! *)
	| c::rest    ->
	    
	    (* We compute how far we need to shift d to not overlap with c on
	       any depth level of the tree *)
	    let sh = minimalDistance accumulated c in
	      
	    (* Compute the combined contour of c and d, with d shifted so that it
	       does not overlap *)
	    let newAccumulated  = mergeContours accumulated (shiftContour (c,sh)) in
	      
	      (* let _ = print_string ("Shifted by "^(string_of_int sh)^" and merged into "^(contour_as_string merged)^"\n") 
		 in *)
	      
	      (* And then continue merging that with the rest *)
	      sh::(doMerges newAccumulated rest)
  in

    doMerges [] contours 
;;

(*
  Ok, what is the problem with fitContoursL? 
  It is that it works left-to-right and will therefore place branches that are
  in reality free to shift (because they are enclosed between branches that must be
  very far apart) completely to the left. But we want them to be centered.
  
  The remedy is as follows. We do fitContoursL the other way around, hence called
  fitContoursR. This gives us positions which are negative, and we then take the mean
  with the positive ones. Kind of magically it works.
*)
let fitContoursR contours = 
  (* We mirror the contours (along their central axis) *)
  let mirrorconts = List.map mirrorContour contours in
    (* We invert their order, and then do fitContours *)
  let invShifts   = fitContoursL (List.rev mirrorconts) in
    (* Finally we reverse again and take the negative *)
    List.map (fun x -> (-.x)) (List.rev (invShifts))


let fitContours contours =

  (* Doing the algorithm L->R *)
  let left  = fitContoursL contours in
  (* Doing the algorithm R->L *)
  let right = fitContoursR contours in

    (* Now we take their average. Since the R->L positions are negative,
       this means that we have automatically centered the branches around
       zero, so no need to do that separately! *)
    List.map (fun (x,y)->(x+.y)/.2.0) (List.combine left right);;
 
(* 
   Convert a tree into a drawingTree, 
   that is, lay it out and mark each node with its
   position. This is the only task that is slightly complicated,
   because we are going to lay the branches out as tightly as possible. 

   Here "drawing" is the drawing object, which we need to measure the width
   of the node labels.
   
   We return a tuple (dtree,contour) where:
   - dtree   = the drawtree (i.e. containing the positions of the nodes), and
   - contour = the horizontal intervals that each of the 
   depth crossections of the tree takes up.
*)

let processTree tree =
  (* Calculate a drawtree (i.e. with positions at the node) from a tree *)

  let rec processTree' tree = 
    
    (* Find the horizontal size (in pixels) of the label *)
    let label_size lab = Font.measure label_font lab in
	
      match tree with
	  T (lab,descendents) -> 
	    
	    (* Compute the size of the label *)
	    let labSize = float(label_size lab)/.2. in
	    let labInterval = ( -.(labSize+.horizontal_padding), labSize+.horizontal_padding)
	      (* Tells us how far the label sticks out on the left and right *)
	    in
	      
	      match descendents with 
		  (* If this is a leaf, then the task is easy *)
		  [] -> ( D (lab,0.0,[]),     (* the drawtree *)
			  [labInterval] )   (* the contour is easy, it is just the label size. *)
		    
		(* If it has descedents, then we need to look a little closer *)
		| descendents -> 
		    
		    (* For each of the descendents, we compute their drawtree and
		       contour *)
		    let processedDesc = List.map processTree' descendents in
		      
		    (* We get the draw trees in a nice list *)
		    (* And we get the contours in another equally nice list *)
		    let (drawDesc,contDesc) = List.split processedDesc in
		      
		    (* Do the complicated stuff here: compute how to best fit the contours together
		       so that they don't overlap. *)
		    let shifts = fitContours contDesc in

		      ( (* the drawtree *)
			D (lab,
			   0.0,
			   List.map shiftTree (List.combine drawDesc shifts)
			  ),
			
			(* the big contour, where we add the interval that
			   the label of this node takes up: *)
			labInterval::(umbrellaContour (List.map shiftContour (List.combine contDesc shifts)))
		      )
			
  in

    (* First we get this drawtree *)
    processTree' tree;;

(*draw the tree, with all the necessary commands to initialize Tk*)

(* We are going to be drawing in different ways: 
   (1) onto a Tcl/Tk canvas, 
   (2) as LaTeX picture output, and
   (3) as a postscript file.
   Now the following class is a wrapper around these
   different ways of output so that we can in all cases
   call the same functions.
   
   This is the drawing interface:
*)
class type drawing_type =
object
  
  (* spacing issues *)
  
  (* drawing lines and drawing labels *)
  method draw_line:        (float * float) -> (float * float) -> unit 
  method draw_label:       (float * float) -> string -> unit
    
    
  (* these are just for debugging *)
  method draw_thick_point: (float * float) -> unit
  method draw_yellow_line: (float * float) -> (float * float) -> unit
    
end

(* Rounding a float (this is used because drawing coordinates
   are given in float to our functions but they need to then
   draw in ints *)
let round_float x = int_of_float (floor (x +. 0.5))

(* 
   Ok, so we are going to be drawing in different places at the same time.
   First of all we'll draw onto the Tcl canvas (for immediate screen display).
   But also, we'll write LaTeX output (for later saving to .tex file).
   Finally, we'll generate PostScript output (for a .ps file).
   This object keeps track of these and allows us to use a single call
   to draw onto both at the same time.
*)

(* canvas     = The Tcl canvas on which we are drawing *)
(* latex      = The latex (picture environment) output buffer *)
class drawing canvas latex postscript (width,height) = object 

  method init = 

    (* First clear the buffers *)
    Buffer.clear latex;
    Buffer.clear postscript;

    (* The beginning of the LaTeX buffer *)
    Buffer.add_string latex ("{\n\\begin{picture}("^
			       (string_of_int width)^
			       ","^(string_of_int height)^
			       ")(0,-"^(string_of_int height)^")\n");
    (* The beginning of the Postscript buffer *)
    Buffer.add_string postscript ("%!PS-Adobe-3.0 EPSF-3.0\n"^
				    "%%Creator: TreeDraw for OCaml\n"^
				    "%%Title: tree\n"^
				    "%%DocumentData: Clean7Bit\n"^
				    "%%LanguageLevel: 2\n"^
				    "%%Pages: 1\n"^
				    "%%Orientation: Portrait\n"^
				    "%%Page: 1 1\n"^
				    (*"%%DocumentNeededResources: font NimbusRomanNo9L-Bold\n"^*)
				    "%%BoundingBox: 0 0 "^(string_of_int width)^" "^(string_of_int height)^"\n"^
				    "%%EndComments\n\n"^
				    
				    "/Times-Roman findfont\n"^
				    "12 scalefont\n"^"setfont\n\n");
    
  method close =
    Buffer.add_string latex "\\end{picture}\n}\n";
    Buffer.add_string postscript "\n%%EOF\n";
    
  (* draws a thick point (only for debugging) *)
  method draw_thick_point (xF,yF) =
    let (x,y)=(round_float xF,round_float yF) in
    let size = 3 in
      (*let _ = print_string ("Dot on "^(string_of_int x)^","^(string_of_int y)^"\n") in*)
      ignore (Canvas.create_oval
		~x1: (x-size)     ~y1: (y-size)
		~x2: (x+size)     ~y2: (y+size)
		~outline: `Red    ~fill:`Red
		canvas
	     )

  (* Again, just for debugging *)
  method draw_yellow_line (x1F,y1F) (x2F,y2F) =
    let (x1,y1)=(round_float x1F,round_float y1F) in
    let (x2,y2)=(round_float x2F,round_float y2F) in
    
    ignore (Canvas.create_line 
	      ~xys:[(x1,y1); (x2,y2)] 
	      ~fill:`Yellow
	      ~width:3
	      canvas
	   );
    Buffer.add_string latex ""

  method draw_line (x1F,y1F) (x2F,y2F) =
    (* We calculate an intermediate point through which we draw our line
       so that it becomes slightly curved for aesthetic purposes. *)
    let (x1,y1)=(round_float x1F,round_float y1F) in
    let (x2,y2)=(round_float x2F,round_float y2F) in

    let (xi,yi) = 
      (round_float (x1F+.(0.42*.(x2F-.x1F))),
       round_float (y1F+.(0.58*.(y2F-.y1F)))) in
      ignore (Canvas.create_line 
		~xys:[(x1,y1); (xi,yi); (x2,y2)] 
		~width:1
		~smooth:true
		canvas
	     );
      
      (* 
	 This drawline function uses the latex packages epic and eepic,
	 so include this in your LaTeX file preamble:
	 \usepackage{epic}   \usepackage{eepic}
      *)
      Buffer.add_string latex ("\\drawline("^(string_of_int x1)^
				 ","^(string_of_int (-y1))^")"^
				 "("^(string_of_int (x2))^","^
				 (string_of_int (-y2))^")"^"\n");
      
      (* 
	 Literal Postscript commands
      *)
      Buffer.add_string postscript ("\nnewpath\n0 setlinejoin 1 setlinecap\n"^
				      (* from coordinates *)
				      (string_of_int x1)^" "^(string_of_int (height-y1))^" moveto\n"^
				      (* intermediate coordinates *)
				      (string_of_int xi)^" "^(string_of_int (height-yi))^" "^
				      (string_of_int xi)^" "^(string_of_int (height-yi))^" "^
				      (* to coordinates *)
				      (string_of_int x2)^" "^(string_of_int (height-y2))^" curveto\n"^
				      "stroke\n")
      
  (* Draw label lab at point (x,y). *)
  method draw_label (xF,yF) lab =
    let (x,y)=(round_float xF,round_float yF) in
    
    let label_width = float(Font.measure label_font lab)/.2. in
      
      ignore (Canvas.create_text
		~x:x
		~y:y 
		~text:lab (* (labelprint a) *)
		~anchor:`N
		~font:label_font 
		canvas);
      Buffer.add_string latex ("\\put("^
				 (string_of_int (round_float (xF-.label_width)))^
				 ","^
				 (string_of_int (-(round_float(yF+.latex_label_vertical_offset))))^
				 "){"^lab^"}\n");

      Buffer.add_string postscript ("newpath\n"^
				      (string_of_int (round_float (xF-.label_width)))^" "^
				      (string_of_int (height-(round_float (yF+.postscript_label_vertical_offset))))^" moveto\n"^
				      "("^lab^") show\n")
    
end

(* Used for printing to a file *)
open Printf;;

(* Write message to the file *)
let writeToFile file message =

  let oc = open_out file in (* create or truncate file, return channel *)
    fprintf oc "%s\n" message; (* write something *)
    close_out oc (* flush and close the channel *)

;;
	  
(*initialize Tk*)
open Tk ;;

(* Plot the contour of a tree *)
let plotContour (drawing:drawing) contour =

  let rec plotContour' y cont =
  match cont with
      [] -> ()
    | (l,r)::rest -> 
	begin
	  drawing#draw_yellow_line (l,y) (r,y);
	  plotContour' (y+.tree_line_interval) rest
	end
  in
    plotContour' 0.0 contour;;


(* Ok, we have a "drawing" object that allows us to draw
   lines and labels. Now we just draw the tree in drawtree *)
let doplot (drawing:drawing) drawtree =
  
  (* Draw a node of the tree with label lab, starting at (x0,y0).
     Also draw the descendents from there. *)
  let rec plotnode lab (x0,y0) descendents  = 
    begin

      (* Draw the node label *)
      drawing#draw_label (x0,y0) lab;
      (*drawing#draw_thick_point (x0,y0);*)
      
      (* Draw lines to each of the descedents from below our label *)
      let drawDescendent d =
	match d with
	    D (descLab,descX,descDesc) ->
	      
	      (* The actual screen coordinates of the descendent *)
	      let (x1,y1) = (x0+.descX,y0+.tree_line_interval) in
		
		begin
		  
		  (* Draw a line to the descendent *)
		  drawing#draw_line 
		    (* from *)
		    (x0, y0+.tree_node_label_height)
		    (* to   *)			
		    (x1, y1+.tree_line_padding  );
		  
		  (* Then draw the descendent itself *)
		  plotnode descLab (x1,y1) descDesc
		    
		end
      in
	
	List.iter drawDescendent descendents 

    end
      
  in
    match drawtree with
	D(lab,x,descendents) ->
	  plotnode lab (x,0.0) descendents
;;

(* Plot a tree. 
   That is:
   (1) process it so that it becomes a "ready-to-draw" drawtree,
   (2) create a nice little window
   and a Tcl/Tk canvas and plot the drawtree on that. 
*)
let tktree tree =

  (* Create the LaTeX and postscript output buffers.
     NB the size 1000 is just an initial size. It will expand if we add more stuff *)
  let buf_latex      = Buffer.create 1000 in
  let buf_postscript = Buffer.create 1000 in

  let top = openTk () in

    (* Now we make the actual drawTree which contains for each
       node its position, so that we only need to draw that. 
       Notice that Tk needs to be opened before this step, otherwise
       we cannot determine the sizes of the tree labels. *)
  let (drawtree,sizes) = 
    let (dt,contour) = processTree tree in
      
    (* We compute how far we need to shift it to the right so that nothing "sticks out" on
       the left; that is, that all of the labels are entirely on the canvas. *)
    let correction =   
      let (left,_)=List.split contour in
	(* We take the negative of this number, since if it's negative that means it sticks out on the left *)
	-.(List.fold_right min left 0.0)
	  
    in
      (* And we return these shifted versions *)
      (shiftTree     (dt,correction),
       shiftContour  (contour,correction))
  in

  (* Determine the height of the tree t *)
  let hght = int_of_float (ceil((float(maxDepth tree))*.(tree_line_interval))) in
    (* Determine the width of the tree (here we assume that nothing "sticks out" to the left) *)
  let wdth  = 
    let (_,right)=List.split sizes in
      int_of_float(ceil(List.fold_right max right 0.0))
  in

  let top_frame = Frame.create top in
  let c = Canvas.create
            ~width:wdth
            ~height:hght
            ~borderwidth:0
            ~scrollregion:(-10,-10,wdth+10,hght+10)
            top in

    (* Create a "drawing" object, which outputs at the same time
       to the screen and to the LaTeX buffer *)
  let drawing = 
    (new drawing c buf_latex buf_postscript (wdth,hght)) in

  let texf   = Frame.create top in
  let latex  = Text.create ~height:5 ~width:35 texf in
  let scr    = Scrollbar.create texf in
  let copy   = Button.create top 
    ~font: "6x9"
    ~text: "Copy LaTeX code to clipboard"
    ~command: (fun () ->     
		 Clipboard.clear ();
		 Clipboard.append (Buffer.contents buf_latex) (); 
	      ) in
  let bottom = Frame.create
    ~width:(max 400 wdth)
    top in
  let entry = Entry.create bottom
    ~width:12
    ~font: "6x9"
  in
  let mes = Message.create bottom
    ~text: ".ps"
  in
  let button = Button.create bottom  (* Tiffany Hsu *)
    ~font: "6x9"
    ~text: "Save"
    ~command: (fun () -> 
		 writeToFile (Entry.get entry ^".ps") (Buffer.contents buf_postscript)
		   ) in
  let scrx = Scrollbar.create ~orient:`Horizontal top in
  let scry = Scrollbar.create ~orient:`Vertical top in
    (* ES    Buffer.add_string buf ("{\\large \\bf \n\\begin{picture}("^ *)

    Wm.title_set top "tree";
    (* ES replaced Wm.geometry_set top ((string_of_int (wdth + 100))^"x"^(string_of_int (hght tr + 200))); *)
    (* ES Without max on width, button texts not easily readable.
       Without min on height, both buttons and resizing corner may be off screen *)
    Wm.geometry_set top ((string_of_int (max 400 (wdth + 100)))^"x"^(string_of_int (min (hght + 200) 600)));
    pack [copy] ~side: `Bottom ~expand:false;
    (*ES: Uncomment if you want to display the latex code. For most, it will be more
      convenient to edit the code in their latex document source.
          pack [texf] ~side: `Bottom ~expand:true;
	  pack [latex] ~side: `Left ~expand:false ~fill:`Both;
	  pack [scr] ~side: `Right ~fill:`Y;
    *)
    pack [top_frame]  ~side: `Top    ~expand:true ~fill:`Both;
    pack [bottom]     ~side: `Bottom ~expand:false;
    pack [entry]      ~side: `Left   ~expand:true;
    pack [mes]        ~side: `Left   ~expand:false;
    pack [button]     ~side: `Left   ~expand:false;
    pack [scrx]       ~side: `Bottom ~expand:true ~fill:`X;
    pack [scry]       ~side: `Right  ~fill:`Y;
    pack [c]          ~side: `Top    ~expand:true ~fill:`Both;
    Canvas.configure ~xscrollcommand:(Scrollbar.set scrx) ~yscrollcommand:(Scrollbar.set scry) c;
    Scrollbar.configure ~command:(Canvas.yview c) scry;
    Scrollbar.configure ~command:(Canvas.xview c) scrx;

    (* Initialise the output buffers *)
    drawing#init;

    (* Draw the actual tree on the canvas and on the buffer *)
    doplot drawing drawtree;

    (* Plot the contour of the final tree (for debugging) *)
    (*plotContour drawing sizes;*)

    drawing#close;

    Text.insert 
      ~index:(`End,[]) 
      ~text:(Buffer.contents buf_latex) 
      latex;
    Text.configure ~yscrollcommand:(Scrollbar.set scr) latex;
    Scrollbar.configure ~command:(Text.yview latex) scr;
    pack [latex];
    pack [c];
    Printexc.print mainLoop ();

    (drawtree,sizes)
;;
