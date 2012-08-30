/* file: lexBuild.pl

After loading a lexicon in the original format, use this command to
build a tree representation of it.

  ?- lexBuild(T).

 */
:- op(500, xfy, ::). % lexical items
:- op(500, fx, =). % for selection features

lexBuild(T) :- bagof((S,Fs),(S::Fs),L), addLexicon(L,'.'/[],T).

addLexicon([],T,T).
addLexicon([(String,Fs)|Items],Rt/Ts0,T) :-
	rev(Fs,[String],ReverseFs),
	lexTranslate(ReverseFs,NewFs),
	addLexItem(NewFs,Ts0,Ts1), !,
	addLexicon(Items,Rt/Ts1,T).

% translate the old feature notation into the new one
lexTranslate([],[]).
lexTranslate([=F|Fs0],[sel(F)|Fs]) :- !, lexTranslate(Fs0,Fs).
lexTranslate([+F|Fs0],[pos(F)|Fs]) :- !, lexTranslate(Fs0,Fs).
lexTranslate([-F|Fs0],[neg(F)|Fs]) :- !, lexTranslate(Fs0,Fs).
lexTranslate([[]|Fs0],[[]|Fs]) :- !, lexTranslate(Fs0,Fs).
lexTranslate([[W|Ws]|Fs0],[[W|Ws]|Fs]) :- !, lexTranslate(Fs0,Fs).
lexTranslate([F|Fs0],[cat(F)|Fs]) :- lexTranslate(Fs0,Fs).

% addLexItem(Fs,Ts0,Ts) Ts is the result of making sure Fs is in Ts0,
%   adding nodes to the tree where necessary. We keep the nodes of the
%   tree in Prolog's standard alphanumeric order
addLexItem([],Ts,Ts).
addLexItem([F|Fs],[F/FTs0|Ts],[F/FTs|Ts]) :- !, addLexItem(Fs,FTs0,FTs).
addLexItem([F|Fs],[G/GTs|Ts],[F/FTs,G/GTs|Ts]) :- F @< G, !, addLexItem(Fs,[],FTs).
addLexItem([F|Fs],[T|Ts0],[T|Ts]) :- addLexItem([F|Fs],Ts0,Ts).
addLexItem([F|Fs],[],[F/FTs]) :-  addLexItem(Fs,[],FTs).

% the standard Prolog reverse
rev([],L,L).
rev([E|L0],L1,L) :- rev(L0,[E|L1],L).
