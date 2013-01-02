/* draw_treeSWI.pl:  A tree drawing package
 * Written by Mark Johnson, 18th Febuary 1995
 * modified by Edward Stabler for the SICStus Prolog-TCL interface, tk_tree.pl
 * modified by Edward Stabler for SWI-Prolog   
 *
 * Top level call: draw_tree(Tree, InterfaceModule).
 *
 * Tree is a term denoting the tree to be drawn.
 * The InterfaceModule (e.g. latex_tree, tk_tree) relates the tree drawn here
 *  to the particular format of the display
 *
 * The interface Module must provide the following:
 *
 * Module:tree(+Tree, -Label, -Subtrees) 
 *    decompose Tree into its Label and a list (possibly empty) of Subtrees.
 *
 * Module: label_size(+Label, -Width, -Height) 
 *    the Width and Height of Label.  
 *
 * Module: xgap(-XGap) the min space between nodes in the X direction
 * Module: ygap(-YGap) the min space between nodes in the Y direction
 *
 * Module: openstream(+Xmax, +Ymax, +Tree, -Stream)
 *	create a `drawing stream' Stream
 *	for an image of size Xmax,Ymax (Stream can be any Prolog object
 *	whatsoever).
 *
 * Module: drawline(+Stream0, -Stream, +X0, +Y0, +X1, +Y1)
 *	draw a line on Stream0 from X0,Y0 to X1, Y1, returning Stream1.
 *
 * Module: drawlabel(+Stream0, -Stream, +X, +Y, +Label)
 *	draw Label with top center at X,Y on Stream, returning Stream1.
 *
 * draw_tree threads a pair of variables Stream0-Stream through all of the
 * the interface drawing calls.  This makes it easy to collect all of the
 * drawing commands in a list or some other data structure.
 */

:- module(draw_tree, [draw_tree/2]).

%:- use_module(library(lists)).

% internal node data structures

node_offsets(n(X,Y,_,_,_), X, Y).
node_xchildshift(n(_,_,XCS,_,_), XCS).		% X shift for child
node(n(_,_,_,Label,Children), Label, Children).

draw_tree(Tree, Module) :-
	layout_tree(Tree, Node, LeftSide, RightSide, Module),
	node_offsets(Node, 0, 0),
	min_x_side(LeftSide, X0),
	max_x_side(RightSide, X1),
	last(RightSide, _-Ymax), %sicstus, new versions of swi
%	last(_-Ymax, RightSide), % old swi
	DX is -(X0),
	Xmax is X1-X0,
	Module:openstream(Xmax, Ymax, Tree, Stream0),
	draw_node(Node, Stream0, Stream, none, none, DX, 0, Module),
	Module:closestream(Stream).

layout_tree(Tree, Node, LeftSide, RightSide, Module) :-
	Module:tree(Tree, Label, Subtrees),
	node(Node, Label, Subnodes),
	Module:label_size(Label, Width, Height),
	Width2 is Width/2,
	MWidth2 is -(Width2),
	(	Subtrees = []
	->	Subnodes = [],
		LeftSide = [MWidth2-Height],
		RightSide = [Width2-Height]
	;	Module:ygap(YGap),
		DY is Height+YGap,
		layout_subtrees(Subtrees, DY, Subnodes, LeftSide0, RightSide0, Module),
		last(Subnodes, RightNode), % sicstus, new versions of swi
%    		last(RightNode, Subnodes), % old swi
		node_offsets(RightNode, X, _Y),
		DX is X/(-2),
		node_xchildshift(Node, DX),
		translate_sides(LeftSide0, DX, DY, LeftSide1),
		translate_sides(RightSide0, DX, DY, RightSide1),
		LeftSide = [MWidth2-Height|LeftSide1],
		RightSide = [Width2-Height|RightSide1] 
	).

layout_subtrees(Trees, DY, Nodes, LeftSide, RightSide, Module) :-
	layout_subtrees(Trees, DY, Nodes, LeftSide, [], RightSide, Module).

layout_subtrees([], _DY, [], [], RightSide, RightSide, _Module).
layout_subtrees([Tree|Trees], DY, [Node|Nodes], LeftSide, RightSide0, RightSide, Module) :-
	layout_tree(Tree, Node, LeftSide1, RightSide1, Module),
	(	RightSide0 = []
	->	DX = 0
	;	max_overlap(RightSide0, LeftSide1, DX0),
		Module:xgap(DX1),
		DX is DX0+DX1
	),
	node_offsets(Node, DX, DY),
	translate_sides(RightSide1, DX, 0, DXRightSide1),
	merge_sides(DXRightSide1, RightSide0, MergedRightSide),
	layout_subtrees(Trees, DY, Nodes, LeftSide0, MergedRightSide, RightSide, Module),
	translate_sides(LeftSide1, DX, 0, DXLeftSide1),
	merge_sides(DXLeftSide1, LeftSide0, LeftSide).

% merge_sides(Ps, Qs, Ms) appends Ps to those Qs whose Y value
%  is greater than the greatest Y value of Ps.

merge_sides(Ps, Qs, Ms) :-
	last(Ps, _Xp-Yp),	% sicstus, new versions of swi
%	last(_Xp-Yp,Ps),        % old swi
	(	suffix([_Xq0-Yq0,Xq1-Yq1|RestQs], Qs),  
		Yq0 =< Yp, Yq1 > Yp
	->	append(Ps, [Xq1-Yq1|RestQs], Ms)
	;	Ms = Ps
	).

% translate_sides(Ps, DX, DY, DPs) adds DX-DY to Ps, yielding DPs.

translate_sides([], _DX, _XY, []).
translate_sides([X0-Y0|XYs0], DX, DY, [X-Y|XYs]) :-
	X is X0+DX,
	Y is Y0+DY,
	translate_sides(XYs0, DX, DY, XYs).

% max_overlap(Rights, Lefts, XGap) returns the maximum XGap
%  for the same value of Y for Rights and Lefts.

max_overlap(Rights, Lefts, XGap) :-
	max_overlap(Rights, Lefts, 0, XGap).

max_overlap([], _Lefts, XGap, XGap).
max_overlap(Rights, [], XGap, XGap) :- Rights = [_|_].
max_overlap([Xr-Yr|Rs], [Xl-Yl|Ls], XGap0, XGap) :-
	XGap1 is max(Xr-Xl, XGap0),
	( Yr =:= Yl -> max_overlap(Rs, Ls, XGap1, XGap)
	; Yr < Yl   -> max_overlap(Rs, [Xl-Yl|Ls], XGap1, XGap)
	;		max_overlap([Xr-Yr|Rs], Ls, XGap1, XGap)
	).

draw_node(Node, Stream0, Stream, XP, YP, X0, Y0, Module) :-
	node(Node, Label0, Subnodes),
	node_offsets(Node, DX, DY),
	X1 is X0+DX,
	Y1 is Y0+DY,
	(	number(XP), Label0='$n'(_Label), Label=Label0	% EPS
	->	Module:draw0lines(Stream0, Stream1, XP, YP, X1, Y1)
	;	number(XP), Label0='$m'(Label)			% EPS 
	->	Module:draw0lines(Stream0, Stream1, XP, YP, X1, Y1)
	;	number(XP), Label=Label0
	->	Module:drawline(Stream0, Stream1, XP, YP, X1, Y1)
	;	Stream1 = Stream0, Label=Label0
	),
	Module:drawlabel(Stream1, Stream2, X1, Y1, Label),
	(	Subnodes = []
	->	Stream = Stream2
	;	Label='$n'(_),
		node_xchildshift(Node, DX1),
		Module:label_size(Label, _, DY1),
		X2 is X1 + DX1,
		Y2 = Y1,
		draw_nodes(Subnodes, Stream2, Stream, X1, Y2, X2, Y1, Module)
	;	node_xchildshift(Node, DX1),
		Module:label_size(Label, _, DY1),
		X2 is X1 + DX1,
		Y2 is Y1 + DY1,
		draw_nodes(Subnodes, Stream2, Stream, X1, Y2, X2, Y1, Module)
	).

draw_nodes([], Stream, Stream, _XP, _YP, _XOff, _YOff, _Module).
draw_nodes([Node|Nodes], Stream0, Stream, XP, YP, XOff, YOff, Module) :-
	draw_node(Node, Stream0, Stream1, XP, YP, XOff, YOff, Module),
	draw_nodes(Nodes, Stream1, Stream, XP, YP, XOff, YOff, Module).

min_x_side(Ps, MinX) :-
	min_x_side(Ps, 0, MinX).

min_x_side([], MinX, MinX).
min_x_side([X0-_|Ps], MinX0, MinX) :-
	MinX1 is min(X0, MinX0),
	min_x_side(Ps, MinX1, MinX).

max_x_side(Ps, MaxX) :-
	max_x_side(Ps, 0, MaxX).

max_x_side([], MaxX, MaxX).
max_x_side([X0-_|Ps], MaxX0, MaxX) :-
	MaxX1 is max(X0, MaxX0),
	max_x_side(Ps, MaxX1, MaxX).

suffix(Suffix, Suffix).
suffix(X, [_|Tail]) :-
        suffix(X, Tail).
