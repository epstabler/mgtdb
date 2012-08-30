% file: mgbeamp.pl, implementing Stabler (2012) Appendix B

%%%%% BEGIN: ES add these things for the old SWI Prolog
%:- [library(heaps)].
:- [heaps]. 
atomic_list_concat([],'').
atomic_list_concat([E],E) :- !.
atomic_list_concat([E,F|L],S) :- !, atomic_list_concat([F|L],S0), atom_concat(E,S0,S).

atomic_list_concat([],_,'').
atomic_list_concat([E],Sep,E) :- !.
atomic_list_concat([E,F|L],Sep,S) :- !,
    atomic_list_concat([F|L],Sep,S0),
    atom_concat(Sep,S0,S1),
    atom_concat(E,S1,S).
%%%%% END: ES additions for the old SWI Prolog

% This is essentially the same as recognizer mgbeam.pl, except:
% * Each Parse in Beam is (Input,Q,D) where D is a derivation whose leaves
%   may be variables Di in predicted cats (Tree,TreeI,Ms,MIs,Di)
% * For each tree, we add a list of its ancestors in predicted cats:
%     (Tree,Anc,TreeI,Ms,Ancs,MIs,Di), now a 7-tuple, where
%  Anc is the list of ancestors of Tree in Lex, and Ancs are the ancestors of Ms

% INITIALIZE AND BEGIN
parse(_/LexTs,Input,D) :- % last arg is derivation!
    startCategory(F),
    memberOnce(cat(F)/Ts,LexTs),
    singleton_heap(Queue,[],(cat(F)/Ts,[cat(F)],[],[],[],[],DF)), % last arg is derivation!
    singleton_heap(Beam,-1,(Input,Queue,DF)),  % last arg is derivation!
%    portray_beam(Beam), % for tracing only
    extendBeam(LexTs,Beam,D).

% EXTEND THE BEAM RECURSIVELY
extendBeam(LexTs,Beam0,D) :-
    get_from_heap(Beam0,P0,(In,Q0,A0),Beam1), % pop most probable parse
    ( success(In,Q0), D=A0
    ; get_from_heap(Q0,_,(_/Ts,Anc,TI,Movers,Ancs,MIs,A),Q), % pop leftmost cat
      findall(Parse,(member(T,Ts),infer(T,Anc,TI,Movers,Ancs,MIs,A,LexTs,(In,Q,A0),Parse)),New),
      length(New,NumberOfOptions), 
      ( NumberOfOptions>0,
	P is (1/NumberOfOptions)*P0, % uniform probability over next steps
	P < -0.001 -> % Simple pruning rule: improbability bound (cf Roark'01)
	insertAll(New,P,Beam1,Beam),
	%portray_beam(Beam),	% for tracing only
	extendBeam(LexTs,Beam,D)
      ; extendBeam(LexTs,Beam1,D)
      )
    ; empty_heap(Q0),  %portray_beam(Beam1), % for tracing only
      extendBeam(LexTs,Beam1,D)
    ).

% STEPS:  infer(T,I,Movers,MIs,Derivation,Lex,(Input0,Queue0),(Input,Queue))
infer(Words/[],Anc,_TI,Ms,Ancs,_MIs,(Words:Anc)/[],_Lex,(In0,Q,A),(In,Q,A)) :- % SCAN
    Ms=[], Ancs=[], append(Words,In,In0). %, format('~w~n',[scan:Words]).

infer(sel(F)/[FT|FTs],Anc,TI,Ms0,Ancs0,MIs0,x/[B,C],LexTs,(In,Q0,A),(In,Q,A)) :-  % UNMERGE
    terminal([FT|FTs],Terminals,NonTerminals),
    append01(TI,TI0,TI1), % extend tree index TI with 0 and 1
    (  Terminals=[_|_],  % unmerge1
       memberOnce(cat(F)/CTs,LexTs),
       add_to_heap(Q0,TI0,(sel(F)/Terminals,[sel(F)|Anc],TI0,[],[],[],B),Q1),
       least(TI1,MIs0,Least),
       add_to_heap(Q1,Least,(cat(F)/CTs,[cat(F)],TI1,Ms0,Ancs0,MIs0,C),Q)
    ;  NonTerminals=[_|_],  % unmerge2
       memberOnce(cat(F)/CTs,LexTs), 
       least(TI1,MIs0,Least),
       add_to_heap(Q0,Least,(sel(F)/NonTerminals,[sel(F)|Anc],TI1,Ms0,Ancs0,MIs0,B),Q1),
       add_to_heap(Q1,TI0,(cat(F)/CTs,[cat(F)],TI0,[],[],[],C),Q)
    ;  Terminals=[_|_],  % unmerge3
       selectMAI(cat(F)/CTs,OtherA,OtherI,Ms0,Ms,Ancs0,Ancs,MIs0,MIs),
       add_to_heap(Q0,TI,(sel(F)/Terminals,[sel(F)|Anc],TI,[],[],[],B),Q1),
       least(OtherI,MIs,Least),
       add_to_heap(Q1,Least,(cat(F)/CTs,[cat(F)|OtherA],OtherI,Ms,Ancs,MIs,C),Q)
    ;  NonTerminals=[_|_], % unmerge4
       selectMAI(cat(F)/CTs,OtherA,OtherI,Ms0,Ms,Ancs0,Ancs,MIs0,MIs),
       least(TI,MIs,Least),
       add_to_heap(Q0,Least,(sel(F)/NonTerminals,[sel(F)|Anc],TI,Ms,Ancs,MIs,B),Q1),
       add_to_heap(Q1,OtherI,(cat(F)/CTs,[cat(F)|OtherA],OtherI,[],[],[],C),Q)
    ).

infer(pos(F)/[FT|FTs],Anc,TI,Ms0,Ancs0,MIs0,o/[B],LexTs,(In,Q0,A),(In,Q,A)) :- % UNMOVE
    \+ member(neg(F)/_,Ms0),    % shortest move constraint
    ( memberOnce(neg(F)/NTs,LexTs), % unmove1
      append01(TI,TI0,TI1),
      least(TI1,[TI0|MIs0],Least),
      add_to_heap(Q0,Least,
         (pos(F)/[FT|FTs],[pos(F)|Anc],TI1,[neg(F)/NTs|Ms0],[[neg(F)]|Ancs0],[TI0|MIs0],B),
		  Q)
    ; selectMAI(neg(F)/NTs,OtherA,OtherI,Ms0,Ms,Ancs0,Ancs,MIs0,MIs), % unmove2
      least(TI,[OtherI|MIs],Least),
      add_to_heap(Q0,Least,
         (pos(F)/[FT|FTs],[pos(F)|Anc],TI,[neg(F)/NTs|Ms],[[neg(F)|OtherA]|Ancs],[OtherI|MIs],B),
		  Q)
    ).
  
% DEFINITION OF SUCCESS: THE INPUT IS EMPTY, THE PARSE QUEUE IS EMPTY
success([],Q) :- empty_heap(Q).

% terminal(Cats,Terminals,Nontermials) split Cats into Terminals/Nonterminals
terminal([],[],[]).  
terminal([Ws/[]|Ts],[Ws/[]|Trms],NonTrms) :- !, terminal(Ts,Trms,NonTrms).
terminal([T|Ts],Trms,[T|NonTrms]) :- terminal(Ts,Trms,NonTrms).

memberOnce(E,[E|_]) :- !.
memberOnce(E,[_|L]) :- memberOnce(E,L).

% insertAll(Es,P,B0,B) B is result of adding all Es to B0 with priority P
insertAll([],_,B,B). 
insertAll([E|Es],P,B0,B) :- add_to_heap(B0,P,E,B1), insertAll(Es,P,B1,B).

% append(L,L0,L1) L0 is L with 0 appended, L1 has 1 appended
append01([],[0],[1]).
append01([E|L],[E|L0],[E|L1]) :- append01(L,L0,L1).

% select mover, ancestors and index (note that mover is embedded!)
selectMAI(E,A,I,[_/Ts|Es],Es,[A|As],As,[I|Is],Is) :- member(E,Ts).
selectMAI(E,A,I,[F|Fs],[F|Gs],[B|Cs],[B|Ds],[J|Is],[J|Js]) :- selectMAI(E,A,I,Fs,Gs,Cs,Ds,Is,Js).

portray_beam(B) :- heap_to_list(B,L), heap_size(B,S),
    format('~n~w~w~n',[S,' parses in beam:']), portray_parseN(L,1).

showRootsOnly([],[]). % only roots to make trace more readable
showRootsOnly([-(P,(T/_,_Anc,TI,Ms0,_Ancs,MsI,A))|L],[-(P,(T,TI,Ms,MsI,A))|RL]) :-
	rootsOnly(Ms0,Ms),
	showRootsOnly(L,RL).

rootsOnly([],[]).   rootsOnly([R/_|Ts],[R|Rs]) :- rootsOnly(Ts,Rs).

portray_parseN([],_). % portray each parse, numbering them from 1
portray_parseN([-(P,(In,Q,A))|Items],N) :-
    heap_size(Q,S), heap_to_list(Q,QL0), showRootsOnly(QL0,QL),
    format('~w~w~w~w~w~w~w~w~w~w~w~w~w~n',[N,'(',S,'). ','(',P,',(',In,',',QL,',',A,'))']),
    N1 is N+1,
    portray_parseN(Items,N1).

least(I,[],I).  % least(I,Is,J) = J is the least index among I and Is
least(I,[J|Js],Least) :- J@<I -> least(J,Js,Least); least(I,Js,Least).

% Tree drawing tools, lexical tree builder
:- [pp_tree,wish_tree,lexBuild,beautify].
%:- [mg0].
%:- lexBuild(LexTree), wish_tree(LexTree).
%:- lexBuild(LexTree), parse(LexTree,[which,wine,the,queen,prefers],T), btfy(T,D), wish_tree(D).
%:- lexBuild(LexTree), parse(LexTree,[which,wine,the,queen,prefers],T), write(T).
