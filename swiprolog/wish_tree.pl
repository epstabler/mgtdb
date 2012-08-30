/* wish_tree.pl
 *
 * Written by Mark Johnson, 18th Febuary 1995
 *
 * Jan 1996 modifications by E Stabler, of various sorts, quite a few
 * Feb 2000 modified by E Stabler for SWI -- file based, for now
 *
 * wish_tree/1 creates a Wish process if necessary, and 
 * sends it commands that make it draw the tree.
 *
 * Example:
 * 	wish_tree(s/[np/[maria/[]],vp/[sings/[]]]).
 *
 * Example with no arcs to '$m'-marked children:
 * 	wish_tree(example/['$m'(s)/[np/[maria/[]],vp/[sings/[]]]]).
 *
 * Example with no arcs and no gaps between parents and '$n'-marked children:
 * 	wish_tree(s/[np/['$n'(maria)/[]],vp/['$n'(sings)/[]]]).
 */

:- module(wish_tree, [wish_tree/1]).
:- use_module(draw_tree,[draw_tree/2]).
%:- use_module(library(system),[exec/3]).

/* use just one font */
%:- use_module(fonttbr18,	[label_size/3, tk_font_name/1, tk_geometry/1]).
:- use_module(fonttbr12,	[label_size/3, tk_font_name/1, tk_geometry/1]).
%:- use_module(fontcmr10,	[label_size/3, tk_font_name/1, tk_geometry/1]).
%:- use_module(fontcmtt10,	[label_size/3, tk_font_name/1, tk_geometry/1]).
%:- use_module(fonttbr18,	[label_size/3, tk_font_name/1, tk_geometry/1]).
%:- use_module(fonttmr12,	[label_size/3, tk_font_name/1, tk_geometry/1]).
%:- use_module(fonttt8,	[label_size/3, tk_font_name/1, tk_geometry/1]).

%:- use_module(fontcmtt10,	[label_size/3, tk_font_name/1, tk_geometry/1]).

:- dynamic wish_streams/2.

% :- op(100, fx, -).	% EPS: keep default setting

wish_tree(Tree) :- atomic(Tree), !, draw_tree(Tree/[], wish_tree), !, shell('wish wishtemp&').
wish_tree(Tree) :- draw_tree(Tree, wish_tree), !, shell('wish wishtemp&').

/* Wish interface to drawTree
 */

% tree/3  parses a term of the form Label/Subtrees as representing a tree
% with label Label and subtrees Subtrees
tree(Label/(-Word), Label, [Word/[]]) :- !.	% MJ's format
tree((-Word), Word, []) :- !.			% MJ's format
tree('$VAR'(W), '$VAR'(W), []) :- !.
% if the label does not begin with phon or int marker, 
%	then add newlines before phon and int features
tree([H|T]/[], Label, Subtrees) :-
	\+H=ph(_),
	\+H=ii(_), !,
	treeterminal([H|T], Label, Subtrees).
tree('$m'([H|T])/[], '$m'(Label), Subtrees) :-
	\+H=ph(_),
	\+H=ii(_), !,
	treeterminal([H|T], Label, Subtrees).
tree(Label/Subtrees, Label, Subtrees).

treeterminal([ii(II)|Rest], [], ['$n'([ii(II)|Rest])/[]]) :- !.
treeterminal([ph(PH)|Rest], [], ['$n'([ph(PH)|Rest])/[]]) :- !.
treeterminal([H|T], [H|Label], Subtrees) :-
	treeterminal(T, Label, Subtrees).
treeterminal([], [], []).

xgap(10).
ygap(10).

openstream(Xmax, Ymax, _Tree, WIS) :-
	( wish_streams(WIS, WOS)    % WIS = WishInputStream, WOS = WishOutputStream
	  % \+ at_end_of_stream(WOS)
	->   format(WIS, ".c delete all~n", [])
	;    retractall(wish_streams(_,_)),
%             plsys(exec('/usr/bin/wish -geometry 385x420+240+50', [pipe(WIS),pipe(WOS),null], _PID)),
%             exec('/usr/bin/wish -geometry 385x420+240+50', [pipe(WIS),pipe(WOS),null], _PID),
	     open(wishtemp,write,WIS), WOS=user,
	     assert(wish_streams(WIS,WOS)),
	     format(WIS, 
"wm minsize . 20 20
frame .f
canvas .c -xscrollcommand {.hscroll set} -yscrollcommand {.vscroll set}	-scrollregion {-10p -10p 1000p 1000p} -bg white
scrollbar .hscroll -orient horizontal
pack .hscroll -side bottom -fill x -in .f
",           []),
	     format(WIS,
"frame .szb
pack .szb -side bottom -fill x -in .f
button .szb.1 -text \"dump ps\" -font 6x10 -command {.c postscript -colormode gray -file \"ltree.ps\"}
pack .szb.1 -side right
label .szb.2 -text \" wish_tree.pl\" -font -*-times-bold-r-normal--12-120-*'
pack .szb.2 -side left
",           []),
	     format(WIS, 
"pack .c -side top -expand 1 -fill both -in .f
scrollbar .vscroll -orient vertical
pack .vscroll -side right -fill y
pack .f -side left -expand 1 -fill both
.vscroll config -command {.c yview}
.hscroll config -command {.c xview}
",           []) ),
	Xmax10 is Xmax+750,
	Ymax10 is Ymax+500,
	tk_geometry(Geometry),
	format(WIS, Geometry, []), % EPSxx
	format(WIS, "~n", []), % EPSxx
	format(WIS, ".c config -scrollregion {-10p -10p ~0fp ~0fp}~n",
                    [Xmax10, Ymax10]).

drawline(Stream, Stream, X0, Y0, X1, Y1) :-
	format(Stream, ".c create line ~0fp ~0fp ~0fp ~0fp~n", [X0,Y0,X1,Y1]).

draw0lines(Stream, Stream, _X0, _Y0, _X1, _Y1) :- !. % EPS special case

/* not currently used:
draw2lines(Stream, Stream, X0, Y0, X1, Y1) :- % EPS special case
	X01 is X0-1, X11 is X1-1, 
	format(Stream, ".c create line ~0fp ~0fp ~0fp ~0fp~n", [X01,Y0,X11,Y1]),
	X02 is X0+2, X12 is X1+2, 
	format(Stream, ".c create line ~0fp ~0fp ~0fp ~0fp~n", [X02,Y0,X12,Y1]).
*/

drawlabel(Stream, Stream, X, Y0, '$n'(Label)) :- !,
	tk_font_name(FontName),
  	ygap(Ygap),
	Y is Y0-Ygap,
	format(Stream, ".c create text ~0fp ~0fp -text {~p} -anchor n -font ""~p""~n",
	       [X,Y,Label,FontName]).

drawlabel(Stream, Stream, X, Y, Label) :-
	tk_font_name(FontName),
	format(Stream, ".c create text ~0fp ~0fp -text {~p} -anchor n -font ""~p""~n",
	       [X,Y,Label,FontName]).

closestream(Stream) :- flush_output(Stream).
