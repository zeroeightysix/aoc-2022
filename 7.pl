:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

state(S), [S] --> [S].
state(S0, S), [S] --> [S0].

str(S) --> string_without("\n", C), { C \= [], string_codes(S, C) }.

cmd(ls)     --> "$ ls", eol.
cmd(cd(D))  --> "$ cd ", str(D), eol.
up_or_eos   --> cmd(cd("..")) | eos.

listing([])              --> [].
listing(Ls)              --> "dir ", str(_), "\n", listing(Ls).
listing([(S, N)|Ls])     --> integer(S), " ", str(N), "\n", listing(Ls).

size((S, _), S).
dsize(dir(S, _), S).
ls(Size, Files) --> listing(Files), { maplist(size, Files, Sizes), sum_list(Sizes, Size) }.

directory(dir(Size, Subs)) -->
  cmd(cd(_)),
  cmd(ls),
  ls(S1, _Files),
  dirs(Subs),
  { maplist(dsize, Subs, Sizes), sum_list(Sizes, S2), Size #= S1 + S2 },
  up_or_eos.

dirs([])     --> [].
dirs([D|Ds]) -->
  directory(D),
  dirs(Ds).

sum_max(Max, dir(S1, Subs), Sum) :-
  maplist(sum_max(Max), Subs, Sums),
  sum_list(Sums, S2),
  ( S1 #=< Max ->
    Sum #= S1 + S2
  ; Sum #= S2).

size_exceeds(Value, dir(S, _)) :- S #>= Value.
min_above(Above, dir(S, Subs), Min) :-
  S #>= Above,
  Min #= min(S, MinSubs),
  
  findall(M, (member(X, Subs), min_above(Above, X, M)), Mins),
  ( Mins \= [] ->
    min_list(Mins, MinSubs),
    Min #= min(S, MinSubs)
  ; Min #= S).

p1 :- phrase_from_file(directory(D), "7.txt"), sum_max(100_000, D, S), writeln(S).
p2 :- phrase_from_file(directory(dir(S, Subs)), "7.txt"),
  Unused #= 70_000_000 - S,
  DeleteAtLeast #= 30_000_000 - Unused,
  min_above(DeleteAtLeast, dir(S, Subs), Min),
  writeln(Min).
