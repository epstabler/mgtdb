/*
 * file: pp_tree.pl
 */

pp_tree(T) :- pp_tree(T, 0).

pp_tree(Cat/Ts, Column) :- !, tab(Column), write(Cat), write(' /['), pp_trees(Ts, Column).
pp_tree(X, Column) :- tab(Column), write(X).

pp_trees([], _) :- write(']').
pp_trees([T|Ts], Column) :- NextColumn is Column+4, nl, pp_tree(T, NextColumn), pp_rest_trees(Ts, NextColumn).

pp_rest_trees([], _) :- write(']').
pp_rest_trees([T|Ts], Column) :- write(','), nl, pp_tree(T, Column), pp_rest_trees(Ts, Column).
