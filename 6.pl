:- use_module(library(clpfd)).

input(I) :- read_file_to_codes("6.txt", I, []).

distinct(I, Pos, C) :- 
  length(Sub, C),
  all_distinct(Sub),
  append([L, Sub, _], I),
  length(L, LL),
  Pos #= LL + C.

p1 :- input(I), distinct(I, P, 4), writeln(P).
p2 :- input(I), distinct(I, P, 14), writeln(P).

