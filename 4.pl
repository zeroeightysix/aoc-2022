:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).

section(s(I,S)) --> integer(I), "-", integer(S).
line((A,B))     --> section(A), ",", section(B), eol.

assignments([])     --> [].
assignments([A|As]) --> line(A), assignments(As).

% `true` if S1 fully contains S2, or S2 fully contains S1.
subsumes(s(A,B), s(C,D)) :-
  ( A #=< C, B #>= D
  ; C #=< A, D #>= B) -> true.

overlaps(s(A,B), s(C,D)) :-
  A #=< D, C #=< B.

input(I) :- phrase_from_file(assignments(I), "4.txt").

part_1 :-
  input(I),
  aggregate_all(count, (member((S1, S2), I), subsumes(S1, S2)), C),
  writeln(C).

part_2 :-
  input(I),
  aggregate_all(count, (member((S1, S2), I), overlaps(S1, S2)), C),
  writeln(C).
