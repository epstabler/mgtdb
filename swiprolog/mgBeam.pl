% file: mgBeam.pl, implementing Stabler (2012) Appendix B
:- [library(heaps)].
% * Beam is a heap of parses, ranked by -probability, so max probability=min of heap
% * For now, probabilities of each possible expansion uniform
% * Each Parse in Beam is (Input,Q) where
% * Q is a heap of predicted items ranked by LeastIndex
% * Each predicted category in Q is (Tree,TreeI,Movers,MIndices)
% * NB pruning rule in extendBeam: improbability bound (cf Roark'01)

% INITIALIZE AND BEGIN
recognize(_/LexTs,Input) :-
    startCategory(StartF),
    memberOnce(cat(StartF)/Ts,LexTs),
    singleton_heap(Queue,[],(StartF/Ts,[],[],[])),
    singleton_heap(Beam,-1,(Input,Queue)), % -1 since Beam is a minheap
    portray_beam(Beam), % for tracing only
    extendBeam(LexTs,Beam).

% EXTEND THE BEAM RECURSIVELY
extendBeam(LexTs,Beam0) :-
    get_from_heap(Beam0,P0,(In,Q0),Beam1), % pop most probable parse
    ( success(In,Q0)
    ; get_from_heap(Q0,_,(_/Ts,TI,Movers,MIs),Q), % pop leftmost cat
      findall(Parse,(member(T,Ts),infer(T,TI,Movers,MIs,LexTs,(In,Q),Parse)),New),
      length(New,NumberOfOptions), 
      ( NumberOfOptions>0,
	P is (1/NumberOfOptions)*P0, % uniform probability over next steps
	P < -0.001 -> % Simple pruning rule: improbability bound (cf Roark'01)
	insertAll(New,P,Beam1,Beam),
	portray_beam(Beam),	% for tracing only
	extendBeam(LexTs,Beam)
      ; portray_beam(Beam1), % for tracing only
	extendBeam(LexTs,Beam1)
      )
    ; empty_heap(Q0),
      portray_beam(Beam1), % for tracing only
      extendBeam(LexTs,Beam1)
    ).

% STEPS:  infer(T,I,Movers,MIs,Lex,(Input0,Queue0),(Input,Queue))
infer(Words/[],_TI,Ms,_MIs,_Lex,(In0,Q),(In,Q)) :- % SCAN
    Ms=[], append(Words,In,In0), format('~w~n',[scan:Words]).

infer(sel(F)/[FT|FTs],TI,Ms0,MIs0,LexTs,(In,Q0),(In,Q)) :-  % UNMERGE
    terminal([FT|FTs],Terminals,NonTerminals),
    append01(TI,TI0,TI1), % extend tree index TI with 0 and 1
    (  Terminals=[_|_],        % unmerge1
       memberOnce(cat(F)/CTs,LexTs),
       add_to_heap(Q0,TI0,(sel(F)/Terminals,TI0,[],[]),Q1),
       least(TI1,MIs0,Least),
       add_to_heap(Q1,Least,(cat(F)/CTs,TI1,Ms0,MIs0),Q)
    ;  NonTerminals=[_|_], % unmerge2
       memberOnce(cat(F)/CTs,LexTs), 
       least(TI1,MIs0,Least),
       add_to_heap(Q0,Least,(sel(F)/NonTerminals,TI1,Ms0,MIs0),Q1),
       add_to_heap(Q1,TI0,(cat(F)/CTs,TI0,[],[]),Q)
    ;  Terminals=[_|_],   % unmerge3
       selectMI(cat(F)/CTs,OtherI,Ms0,Ms,MIs0,MIs),
       add_to_heap(Q0,TI,(sel(F)/Terminals,TI,[],[]),Q1),
       least(OtherI,MIs,Least),
       add_to_heap(Q1,Least,(cat(F)/CTs,OtherI,Ms,MIs),Q)
    ;  NonTerminals=[_|_], % unmerge4
       selectMI(cat(F)/CTs,OtherI,Ms0,Ms,MIs0,MIs),
       least(TI,MIs,Least),
       add_to_heap(Q0,Least,(sel(F)/NonTerminals,TI,Ms,MIs),Q1),
       add_to_heap(Q1,OtherI,(cat(F)/CTs,OtherI,[],[]),Q)
    ).

infer(pos(F)/[FT|FTs],TI,Ms0,MIs0,LexTs,(In,Q0),(In,Q)) :- % UNMOVE
    \+ member(neg(F)/_,Ms0),        % shortest move constraint
    ( memberOnce(neg(F)/NTs,LexTs), % unmove1
      append01(TI,TI0,TI1),
      least(TI1,[TI0|MIs0],Least),
      add_to_heap(Q0,Least,(pos(F)/[FT|FTs],TI1,[neg(F)/NTs|Ms0],[TI0|MIs0]),Q)
    ; selectMI(neg(F)/NTs,OtherI,Ms0,Ms,MIs0,MIs), % unmove2
      least(TI,[OtherI|MIs],Least),
      add_to_heap(Q0,Least,(pos(F)/[FT|FTs],TI,[neg(F)/NTs|Ms],[OtherI|MIs]),Q)
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

% select mover and index (note that mover is embedded!)
selectMI(E,I,[_/Ts|Es],Es,[I|Is],Is) :- member(E,Ts).
selectMI(E,I,[F|Fs],[F|Gs],[J|Is],[J|Js]) :- selectMI(E,I,Fs,Gs,Is,Js).

portray_beam(B) :- heap_to_list(B,L), heap_size(B,S),
    format('~n~w~w~n',[S,' parses in beam:']), portray_parseN(L,1).

showRootsOnly([],[]). % only roots to make trace more readable
showRootsOnly([-(P,(T/_,TI,Ms0,MsI))|L],[-(P,(T,TI,Ms,MsI))|RL]) :-
	rootsOnly(Ms0,Ms),
	showRootsOnly(L,RL).

rootsOnly([],[]).   rootsOnly([R/_|Ts],[R|Rs]) :- rootsOnly(Ts,Rs).

portray_parseN([],_). % portray each parse, numbering them from 1
portray_parseN([-(P,(In,Q))|Items],N) :-
    heap_size(Q,S), heap_to_list(Q,QL0), showRootsOnly(QL0,QL),
    format('~w~w~w~w~w~w~w~w~w~w~w~n',[N,'(',S,'). ','(',P,',(',In,',',QL,'))']),
    N1 is N+1,
    portray_parseN(Items,N1).

least(I,[],I).  % least(I,Is,J) = J is the least index among I and Is
least(I,[J|Js],Least) :- J@<I -> least(J,Js,Least); least(I,Js,Least).

% Examples
:- [pp_tree,wish_tree,lexBuild,mg0].
:- lexBuild(LexTree), wish_tree(LexTree).
:- lexBuild(LexTree), recognize(LexTree,[which,wine,the,queen,prefers]).
