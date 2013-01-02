% file dt2st.pl  transduce derivation to state (HeadFeatures,ListOfMoverFeatures)

dt2st(x/[A/[],B],(Al,Ms)) :- %mrg1
	 dt2st(B,([cat(F)],Ms)),  % B first, to determine if mrg1 is correct case
	 dt2st(A/[],([sel(F)|Al],[])).
dt2st(x/[A/[A1|L1],B],(Al,Ms12)) :- %mrg2
	 dt2st(B,([cat(F)],Ms2)),
	 dt2st(A/[A1|L1],([sel(F)|Al],Ms1)),
	 append(Ms1,Ms2,Ms12),
	 smc(Ms12).
dt2st(x/[A,B],(Al,Ms12)) :- %mrg3
	 dt2st(B,([cat(F),G|Be],Ms2)),
	 dt2st(A,([sel(F)|Al],Ms1)),
	 append(Ms1,[[G|Be]|Ms2],Ms12),
	 smc(Ms12).
dt2st(xx/[A,B],(Al,Ms12)) :- %mrgEPP
	epp(F), 
	dt2st(B,([cat(F),epp(F)|Be],Ms2)),
	dt2st(A,([sel(F)|Al],Ms1)),
	append(Ms1,[[neg(F)|Be]|Ms2],Ms12),
	smc(Ms12).
dt2st(o/[A],(Al,Ms)) :-
	 dt2st(A,([pos(F)|Al],AMs)),
	 select([neg(F)|Rest],AMs,Remainder),
	 (  Rest=[] , Ms=Remainder             % move1
	 ;  Rest=[_|_], Ms=[Rest|Remainder]    % move2
	 ).
dt2st((_W:Fs)/[],(Fs,[])).

smc([]).
smc([[E|_]|L]) :- \+ member([E|_],L), smc(L).
