% file dt2stt.pl  transduce derivation to state tree

dt2stt(x/[A/[],B],(Al,Ms)/[([sel(F)|Al],[])/ATs,([cat(F)],Ms)/BTs]) :- %mrg1
	 dt2stt(B,([cat(F)],Ms)/BTs),  % B first, to determine if mrg1 is correct case
	 dt2stt(A/[],([sel(F)|Al],[])/ATs).
dt2stt(x/[A/[A1|L1],B],(Al,Ms12)/[([sel(F)|Al],Ms1)/ATs,([cat(F)],Ms2)/BTs]) :- %mrg2
	 dt2stt(B,([cat(F)],Ms2)/BTs),
	 dt2stt(A/[A1|L1],([sel(F)|Al],Ms1)/ATs),
	 append(Ms1,Ms2,Ms12),
	 smc(Ms12).
dt2stt(x/[A,B],(Al,Ms12)/[([sel(F)|Al],Ms1)/ATs,([cat(F),G|Be],Ms2)/BTs]) :- %mrg3
	 dt2stt(B,([cat(F),G|Be],Ms2)/BTs),
	 dt2stt(A,([sel(F)|Al],Ms1)/ATs),
	 append(Ms1,[[G|Be]|Ms2],Ms12),
	 smc(Ms12).
dt2stt(xx/[A,B],(Al,Ms12)/[([sel(F)|Al],Ms1)/ATs,([cat(F)|Be],Ms2)/BTs]) :- epp(F), %mrgEPP
	dt2stt(B,([cat(F),epp(F)|Be],Ms2)/BTs),
	dt2stt(A,([sel(F)|Al],Ms1)/ATs),
	append(Ms1,[[neg(F)|Be]|Ms2],Ms12),
	smc(Ms12).

dt2stt(x/[A,B],(Al,Ms)/[([ra(F)],[])/ATs,(Al,Ms)/BTs]) :- %adjoin
	 dt2stt(A,([ra(F)],[])/ATs),
	 dt2stt(B,(Al,Ms)/BTs).
dt2stt(x/[A,B],(Al,Ms)/[([la(F)],[])/ATs,(Al,Ms)/BTs]) :- %adjoin
	 dt2stt(A,([la(F)],[])/ATs),
	 dt2stt(B,(Al,Ms)/BTs).

dt2stt(o/[A],(Al,Ms)/[([pos(F)|Al],AMs)/ATs]) :-
	 dt2stt(A,([pos(F)|Al],AMs)/ATs),
	 select([neg(F)|Rest],AMs,Remainder),
	 (  Rest=[] , Ms=Remainder             % move1
	 ;  Rest=[_|_], Ms=[Rest|Remainder]    % move2
	 ).
dt2stt((W:Fs)/[],(Fs,[])/[W/[]]).


smc([]).
smc([[E|_]|L]) :- \+ member([E|_],L), smc(L).

:- [beautify].  % we use btfyFs/2 from this file

btfy_stt((Fs,Ms)/[A,B],String/[BA,BB]) :- !,
	btfyFss([Fs|Ms],BFMs),
	atomic_list_concat(BFMs,',',String),
	btfy_stt(A,BA),
	btfy_stt(B,BB).
btfy_stt((Fs,Ms)/[A],String/[BA]) :- !,
	btfyFss([Fs|Ms],BFMs),
	atomic_list_concat(BFMs,',',String),
	btfy_stt(A,BA).
btfy_stt(Ws/[],String/[]) :- atomic_list_concat(Ws,' ',String).

btfyFss([],[]).
btfyFss([Fs|More],[SFs|SMore]) :-
	btfyFs(Fs,BFs),
	atomic_list_concat(BFs,SFs),
	btfyFss(More,SMore).
