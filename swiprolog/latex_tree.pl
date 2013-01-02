/* latex_tree.pl
 *
 * Written by Mark Johnson, 18th Febuary 1995
 * Modified by Edward Stabler, January 1996
 *
 * A LaTeX/eepic interface for the drawTree tree-drawing package.
 *
 * latex_tree/1 writes a LaTeX file ltree.tex in the current
 * directory.  It is easy to extract the picture from this
 * to paste into a file.
 *
 * Example:
 * latex_tree(s/[np/[det/(-the),n1/[adj/(-intelligent),n/(-citizens)]],vp/[v/(-vote),np/(-anarchist)]]).
 *
 * Example with double lines:
 * latex_tree(a/['$m'(b)/[],'$m'(c)/[]]).
 */

:- module(latex_tree, [latex_tree/1]).
% latex_tree/1 is the top predicate
% but draw_tree.pl makes direct calls to:
%	tree/3, label_size/3, xgap/1, ygap/1, drawline/6, drawlabel/5

:- use_module(draw_tree,[draw_tree/2]).
%:- use_module(draw_treeSWI,[draw_tree/2]).
:- use_module(fontcmtt10, [label_size/3]).

latex_tree(Tree) :- draw_tree(Tree, latex_tree), !.

/* LaTeX/eepic interface to drawTree
 */

% tree/3  parses a term of the form Label/Subtrees as representing a tree
% with label Label and subtrees Subtrees
tree(Label/(-Word), Label, [Word/[]]) :- !.	% MJ's format
tree((-Word), Word, []) :- !.			% MJ's format
tree('$VAR'(W), '$VAR'(W), []) :- !.		% ES special
% if the label does not begin with phon or int marker, 
%	then add newlines before phon and int features
tree([H|T]/[], Label, Subtrees) :-		% ES special
	\+H=x(_),
	\+H=ph(_),
	\+H=ii(_), !,
	treeterminal([H|T], Label, Subtrees).
tree('$m'([H|T])/[], '$m'(Label), Subtrees) :-	% ES special
	\+H=x(_),
	\+H=ph(_),
	\+H=ii(_), !,
	treeterminal([H|T], Label, Subtrees).
tree([]/[], '$e', []) :- !.			% ES special
tree('>'/Subtrees, '$>', Subtrees) :- !.	% ES special
tree('<'/Subtrees, '$<', Subtrees) :- !.	% ES special
tree('$m'('>')/Subtrees, '$m'('$>'), Subtrees) :- !.	% ES special
tree('$m'('<')/Subtrees, '$m'('$<'), Subtrees) :- !.	% ES special
tree(Label/Subtrees, Label, Subtrees).		% default case

treeterminal([x(II)|Rest], [], ['$n'([II|Rest])/[]]) :- !.
treeterminal([ii(II)|Rest], [], ['$n'([ii(II)|Rest])/[]]) :- !.
treeterminal([ph(PH)|Rest], [], ['$n'([ph(PH)|Rest])/[]]) :- !.
treeterminal([H|T], [H|Label], Subtrees) :-
	treeterminal(T, Label, Subtrees).
treeterminal([], [], []).

xgap(10).		% min space between nodes in the X direction
ygap(10).		% min space between nodes in the Y direction

openstream(Xmax, Ymax, Tree, Stream) :-
	open('ltree.tex', write, Stream),
        format(Stream, "\\documentstyle[epic,eepic]{article}~n", []),
        format(Stream, "\\begin{document}~n", []),
        MYmax is -(Ymax),
        format(Stream, "\\begin{picture}(~0f,~0f)(~0f,~0f)~n", [Xmax,Ymax,0,MYmax]),
        format(Stream, "% drawTree(~w).~n%~n", [Tree]).

drawline(Stream, Stream, X0, Y0, X1, Y1) :-
        MY0 is -(Y0),
        MY1 is -(Y1),
        format(Stream, "\\drawline(~0f,~0f)(~0f,~0f)~n", [X0,MY0,X1,MY1]).

draw0lines(Stream, Stream, _X0, _Y0, _X1, _Y1) :- !. % EPS

/* not in use now:
draw2lines(Stream, Stream, X0, Y0, X1, Y1) :- % EPS
        MY0 is -(Y0),
        MY1 is -(Y1),
	X01 is X0-1, X11 is X1-1, 
        format(Stream, "\\drawline(~0f,~0f)(~0f,~0f)~n", [X01,MY0,X11,MY1]),
	X02 is X0+2, X12 is X1+2, 
        format(Stream, "\\drawline(~0f,~0f)(~0f,~0f)~n", [X02,MY0,X12,MY1]).
*/

drawlabel(Stream, Stream, X0, Y0, '$n'(Label)) :-
        label_size(Label, DX0, _Y),
        X is X0-(DX0/2),
        Y1 is -(Y0+8),
  	ygap(Ygap),
	Y is Y1+Ygap,
        format(Stream, "\\put(~0f,~0f){~w}~w", [X,Y,Label]).

drawlabel(Stream, Stream, _X0, _Y0, '$e') :- !.

/*
drawlabel(Stream, Stream, X0, Y0, '$e') :-
        label_size(e, DX0, _Y),
        X is X0-(DX0/2),
        Y is -(Y0+8),
        format(Stream, "\\put(~0f,~0f){$~w$}~n", [X,Y,'\\lambda']).
*/

drawlabel(Stream, Stream, X0, Y0, '$<') :-
        label_size(<, DX0, _Y),
        X is X0-(DX0/2),
        Y is -(Y0+8),
        format(Stream, "\\put(~0f,~0f){\\tt<}~n", [X,Y]).

drawlabel(Stream, Stream, X0, Y0, '$>') :-
        label_size(>, DX0, _Y),
        X is X0-(DX0/2),
        Y is -(Y0+8),
        format(Stream, "\\put(~0f,~0f){\\tt>}~n", [X,Y]).

drawlabel(Stream, Stream, X0, Y0, Label) :-
        label_size(Label, DX0, _Y),
        X is X0-(DX0/2),
        Y is -(Y0+8),
        format(Stream, "\\put(~0f,~0f){~w}~n", [X,Y,Label]).

closestream(Stream) :-
        format(Stream, "\\end{picture}~n\\end{document}~n", []),
	close(Stream).
