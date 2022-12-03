:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

% AST
elf([])     --> eol.
elf([C|Cs]) --> number(C), eol, elf(Cs).
elves([])     --> eos.
elves([E|Es]) --> elf(E), elves(Es).

% Can we count maxima using semicontext notation? (yes)
state(S), [S] --> [S].
state(S0, S), [S] --> [S0].

max([])     --> [].
max([N|Ns]) -->
  state(M0, M),
  { M #= max(M0, N) },
  max(Ns).

% hence, part 1's solution is a phrase that 'ends up' in the maximum of the input elves
input(I) :- phrase_from_file(elves(I), "1.txt").

part_1 :-
  input(I),
  maplist(sum_list, I, Sums),
  phrase(max(Sums), [0], M),
  format('Part 1: ~d', M).
