:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).

prio([])     --> [].
prio([P|Ps]) --> [C], { char_type(C, lower) -> P #= C - 96 }, prio(Ps).
prio([P|Ps]) --> [C], { char_type(C, upper) -> P #= C - 38 }, prio(Ps).

prios([])             --> eos.
prios([bag(A,B)|Pss]) --> prio(A), prio(B), { length(A, LA), length(B, LB), LA #= LB }, eol, prios(Pss).

triplet(t(A,B,C)) --> prio(A), eol, prio(B), eol, prio(C), eol.

triplets([])     --> [].
triplets([T|Ts]) --> triplet(T), triplets(Ts).

bags(B)  :- once(phrase_from_file(prios(B), "3.txt")).
trips(B) :- once(phrase_from_file(triplets(B), "3.txt")).

bag_intersect(bag(A,B), I) :- 
  intersection(A, B, Is), 
  member(I, Is).

trip_intersect(t(A,B,C), I) :-
  intersection(A, B, AB),
  intersection(AB, C, Is),
  member(I, Is).

part_1 :-
  bags(B),
  maplist(bag_intersect, B, I),
  sum_list(I, Sum),
  writeln(Sum).

part_2 :-
  trips(B),
  maplist(trip_intersect, B, I),
  sum_list(I, Sum),
  writeln(Sum).
