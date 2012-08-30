% file dt2xb.pl  transduce derivation to x-bar
% Adding 2 arguments to the transducer predicates, these 
% hold the head category and x-bar tree, respectively.

dt2xb(T,XP/XB) :- dt2xb(T,([cat(Cat)],[]),Cat,_/XB), catMax(Cat,XP), novars(XB).

dt2xb(_/[A/[],B],(Al,Ms),Cat,CatMid/[XBA,BP/XBB]) :- %mrg1
	 dt2xb(B,([cat(F)],Ms),F,_/XBB), % B first, to determine if mrg1 is correct case
	 dt2xb(A/[],([sel(F)|Al],[]),Cat,XBA),
	 catMid(Cat,CatMid), catMax(F,BP).
dt2xb(_/[A/[A1|L1],B],(Al,Ms12),Cat,CatMid/[BP/XBB,XBA]) :- %mrg2
	 dt2xb(B,([cat(F)],Ms2),F,_/XBB),
	 dt2xb(A/[A1|L1],([sel(F)|Al],Ms1),Cat,XBA),
	 append(Ms1,Ms2,Ms12), smc_xb(Ms12),  catMid(Cat,CatMid), catMax(F,BP).
dt2xb(_/[A/[],B],(Al,Ms12),Cat,CatMid/[XBA,Ti/[]]) :- %mrg3a
	 dt2xb(B,([cat(F),G|Be],Ms2),F,_/XBB),
	 dt2xb(A/[],([sel(F)|Al],Ms1),Cat,XBA),
	 append([ ([G|Be],BPi/XBB) |Ms1], Ms2 , Ms12), smc_xb(Ms12), % INSERT INTO MOVERS
	 catMid(Cat,CatMid), catMax(F,BP), BPi=..[BP,Trace], Ti=..[t,Trace].
dt2xb(_/[A/[A1|L1],B],(Al,Ms12),Cat,CatMid/[Ti/[],XBA]) :- %mrg3b
	 dt2xb(B,([cat(F),G|Be],Ms2),F,_/XBB),
	 dt2xb(A/[A1|L1],([sel(F)|Al],Ms1),Cat,XBA),
	 append([ ([G|Be],BPi/XBB) | Ms1], Ms2, Ms12), smc_xb(Ms12),   % INSERT INTO MOVERS
	 catMid(Cat,CatMid), catMax(F,BP), BPi=..[BP,Trace], Ti=..[t,Trace].

dt2xb(_/[A,B],(Al,Ms),Cat,BP/[BP/BTB,AP/BTA]) :- %right adjoin
	 dt2xb(A,([ra(Cat)],[]),Cat,_/BTA),
	 dt2xb(B,(Al,Ms),Cat,_/BTB),
	 atomic_concat('~r',Cat,ACat),  % Since the usual adjunct cats not given
	 catMax(ACat,AP), catMax(Cat,BP).
dt2xb(_/[A,B],(Al,Ms),Cat,BP/[AP/BTA,BP/BTB]) :- %left adjoin
	 dt2xb(A,([la(Cat)],[]),Cat,_/BTA),
	 dt2xb(B,(Al,Ms),Cat,_/BTB),
	 atomic_concat('~l',Cat,ACat), % Since the usual adjunct cats not given
	 catMax(ACat,AP), catMax(Cat,BP).

dt2xb(o/[A],(Al,Ms),Cat,CatMid/[Spec,XBA]) :-
	 dt2xb(A,([pos(F)|Al],AMs),Cat,XBA),
	 select( ( [neg(F)|Rest], XBB ), AMs,Remainder),  % EXTRACT FROM MOVERS
	 (  Rest=[] , Ms=Remainder, Spec=XBB                     % move1
	 ;  Rest=[_|_], Ms=[(Rest,XBB)|Remainder], Spec=''/[]    % move2
	 ),
	 catMid(Cat,CatMid).
dt2xb(o/[A],(Al,Ms),Cat,CatMid/[Spec,XBA]) :-
	 dt2xb(A,([pos(F)|Al],AMs),Cat,XBA),
	 select( ( [epp(F)|Rest], XBB ), AMs,Remainder),  % EXTRACT FROM MOVERS - EPP case
	 (  Rest=[] , Ms=Remainder, Spec=XBB                     % move1
	 ;  Rest=[_|_], Ms=[(Rest,XBB)|Remainder], Spec=''/[]    % move2
	 ),
	 catMid(Cat,CatMid).
dt2xb((W:Fs)/[],(Fs,[]),Cat,Cat/[W/[]]).

catMid(F,FBar) :- atomic_concat(F,'''',FBar).
catMax(F,FMax) :- atomic_concat(F,'P',FMax).

smc_xb([]). % movers are now (Fs,XBTree) pairs
smc_xb([([NegF|_],_Tree0)|L]) :- \+ member(([NegF|_],_Tree1),L), smc_xb(L).

btfy_xb(R/[A,B],R/[BA,BB]) :- !, btfy_xb(A,BA), btfy_xb(B,BB).
btfy_xb(R/[A],R/[BA]) :- !, btfy_xb(A,BA).
btfy_xb(t(N)/[],t(N)/[]) :- !.
btfy_xb(''/[],''/[]) :- !.
btfy_xb(Ws/[],String/[]) :- atomic_list_concat(Ws,' ',String).

novars(M) :- nvs(M,1,_). % instantiate the variables with numbers

nvs(M,M,N):- !, succ(M,N).
nvs(Term,M,N):- functor(Term,_,Arity), nvs(0,Arity,Term,M,N).

nvs(A,A,_,N,N):- !.
nvs(Am,Arity,Term,M,N) :- succ(Am,An), arg(An,Term,Arg), nvs(Arg,M,K), !, nvs(An,Arity,Term,K,N).

%:-[pp_tree,wish_tree,lexBuild,beautify].
%:-[mg0].
%:-lexBuild(LexTree),wish_tree(LexTree).
%:-lexBuild(LexTree),parse(LexTree,[which,wine,the,queen,prefers],T), btfy(T,B), wish_tree(B).
%:-lexBuild(LexTree),parse(LexTree,[which,wine,the,queen,prefers],T), dt2xb(T,S), btfy_xb(S,B), wish_tree(B).
