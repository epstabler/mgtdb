%file: beautify.pl this transducer "beautifies" derivation trees

btfy(xx/[A,B],'**'/[BA,BB]) :- btfy(A,BA), btfy(B,BB).
btfy(x/[A,B],'*'/[BA,BB]) :- btfy(A,BA), btfy(B,BB).
btfy(o/[A],o/[BA]) :- btfy(A,BA).
btfy((Ws:Fs)/[],String/[]) :- btfyLex(Ws,Fs,String).

btfyLex(Ws,Fs,String) :-
	atomic_list_concat(Ws,' ',WsString),
	btfyFs(Fs,BFs),
	atomic_list_concat([WsString,'::'|BFs],String).

btfyFs([],[]).
btfyFs([sel(F)],['=',F]).
btfyFs([cat(F)],[F]).
btfyFs([pos(F)],['+',F]).
btfyFs([neg(F)],['-',F]).
btfyFs([ra(F)],['~r',F]).
btfyFs([la(F)],['~l',F]).
btfyFs([epp(_)],[]).
btfyFs([sel(F),G|Fs],['=',F,' '|GFs]) :- btfyFs([G|Fs],GFs).
btfyFs([cat(F),G|Fs],[F,' '|GFs]) :- btfyFs([G|Fs],GFs).
btfyFs([pos(F),G|Fs],['+',F,' '|GFs]) :- btfyFs([G|Fs],GFs).
btfyFs([neg(F),G|Fs],['-',F,' '|GFs]) :- btfyFs([G|Fs],GFs).
btfyFs([epp(_),G|Fs],GFs) :- btfyFs([G|Fs],GFs).
btfyFs([ra(F),G|Fs],['~r',F,' '|GFs]) :- btfyFs([G|Fs],GFs).
btfyFs([la(F),G|Fs],['~l',F,' '|GFs]) :- btfyFs([G|Fs],GFs).

%:- [mg0,lexBuild,wish_tree,mgbeamp,mg2dt,beautify].
%:- lexBuild(LexTree), parse(LexTree,[which,wine,the,queen,prefers],T),
%   btfy(T,B), wish_tree(B).
