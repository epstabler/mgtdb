% file dt2bt.pl  transduce derivation to bare tree
% Adding 2 arguments to the transducer predicates, these 
% hold the head features and bare tree, respectively.

dt2bt(T,BT) :- dt2bt(T,([cat(Start)],[]),[cat(Start)],BT).

dt2bt(_/[A/[],B],(Al,Ms),Rem,'<'/[BTA,BTB]) :- %mrg1
	 dt2bt(B,([cat(F)],Ms),[],BTB), % B first, to determine if mrg1 is correct case
	 dt2bt(A/[],([sel(F)|Al],[]),Rem,BTA).
dt2bt(_/[A/[A1|L1],B],(Al,Ms12),Rem,'>'/[BTB,BTA]) :- %mrg2
	 dt2bt(B,([cat(F)],Ms2),[],BTB),
	 dt2bt(A/[A1|L1],([sel(F)|Al],Ms1),Rem,BTA),
	 append(Ms1,Ms2,Ms12), smc_bt(Ms12).
dt2bt(_/[A/[],B],(Al,Ms12),Rem,'<'/[BTA,''/[]]) :- %mrg3a
	 dt2bt(B,([cat(F),G|Be],Ms2),[],BTB),
	 dt2bt(A/[],([sel(F)|Al],Ms1),Rem,BTA),
	 append([ ([G|Be],BTB) |Ms1], Ms2 , Ms12), smc_bt(Ms12). % INSERT INTO MOVERS
dt2bt(_/[A/[A1|L1],B],(Al,Ms12),Rem,'>'/[''/[],BTA]) :- %mrg3b
	 dt2bt(B,([cat(F),G|Be],Ms2),[],BTB),
	 dt2bt(A/[A1|L1],([sel(F)|Al],Ms1),Rem,BTA),
	 append([ ([G|Be],BTB) | Ms1], Ms2, Ms12), smc_bt(Ms12).   % INSERT INTO MOVERS

dt2bt(_/[A,B],(Al,Ms),Rem,'<'/[BTB,BTA]) :- %right adjoin
	 dt2bt(A,([ra(_F)],[]),[],BTA),
	 dt2bt(B,(Al,Ms),Rem,BTB).
dt2bt(_/[A,B],(Al,Ms),Rem,'>'/[BTA,BTB]) :- %left adjoin
	 dt2bt(A,([la(_F)],[]),[],BTA),
	 dt2bt(B,(Al,Ms),Rem,BTB).

dt2bt(o/[A],(Al,Ms),Rem,'>'/[Spec,BTA]) :-
	 dt2bt(A,([pos(F)|Al],AMs),Rem,BTA),
	 select( ( [neg(F)|Rest], BTB ), AMs,Remainder),  % EXTRACT FROM MOVERS
	 (  Rest=[] , Ms=Remainder, Spec=BTB                     % move1
	 ;  Rest=[_|_], Ms=[(Rest,BTB)|Remainder], Spec=''/[]    % move2
	 ).
dt2bt(o/[A],(Al,Ms),Rem,'>'/[Spec,BTA]) :-
	 dt2bt(A,([pos(F)|Al],AMs),Rem,BTA),
	 select( ( [epp(F)|Rest], BTB ), AMs,Remainder),  % EXTRACT FROM MOVERS EPP case
 	 (  Rest=[] , Ms=Remainder, Spec=BTB                     % move1
	 ;  Rest=[_|_], Ms=[(Rest,BTB)|Remainder], Spec=''/[]    % move2
	 ).
dt2bt((W:Fs)/[],(Fs,[]),Rem,(W:Rem)/[]).

smc_bt([]). % the movers are now (Fs,Tree) pairs
smc_bt([([NegF|_],_Tree0)|L]) :- \+ member(([NegF|_],_Tree1),L), smc_bt(L).

:- [beautify].  % for btfyFs/2

btfy_bt(R/[A,B],R/[BA,BB]) :- !, btfy_bt(A,BA), btfy_bt(B,BB).
btfy_bt(R/[A],R/[BA]) :- !, btfy_bt(A,BA).
btfy_bt((Ws:Fs)/[],String/[]) :- !,
	atomic_list_concat(Ws,' ',WString),
	btfyFs(Fs,BFs),
	atomic_list_concat(BFs,' ',FString),
	atomic_list_concat([WString,'::',FString],String).
btfy_bt(F/[],F/[]).

btfyFss([],[]).
btfyFss([Fs|More],[SFs|SMore]) :-
	btfyFs(Fs,BFs),
	atomic_list_concat(BFs,SFs),
	btfyFss(More,SMore).

%:- [pp_tree,wish_tree,lexBuild,mg0,beautify,mgbeamp].
%:- lexBuild(LexTree), wish_tree(LexTree).
%:- lexBuild(LexTree), parse(LexTree,[which,wine,the,queen,prefers],T), dt2bt(T,B), btfy_bt(B,Y), wish_tree(Y).
