valid('X').
valid('Y').
valid('Z').

sh_score('X', 1).
sh_score('Y', 2).
sh_score('Z', 3).

st_score(strat(A, B), Score) :-
  sh_score(B, Sh),
  ( win(A, B) ->
    (Score is Sh + 6);
    ( draw(A, B) ->
      (Score is Sh + 3);
      % lose(A,B) ->
      (Score is Sh + 0)
    )
  ).

% Draw conditions
draw('A', 'X').
draw('B', 'Y').
draw('C', 'Z').
% Win conditions
win('A', 'Y').
win('B', 'Z').
win('C', 'X').
% Lose if not draw and not win
lose(A, B) :-
  valid(B),
  \+ (draw(A, B) ; win(A, B)).

strategy(String, strat(A, B)) :-
  split_string(String, " ", " ", [A0, B0]),
  atom_string(A, A0),
  atom_string(B, B0).

read_strats(S) :-
  read_file_to_string("2.txt", Input, []),
  split_string(Input, "\n", "\n", Lines),
  maplist(strategy, Lines, Strats),
  S = Strats.

ind_scores(Strats, Scores) :-
  maplist(st_score, Strats, Scores).

total_score_p1 :-
  read_strats(Sts),
  ind_scores(Sts, Scs),
  sum_list(Scs, Tot),
  write(Tot).

% Part 2

% For some strat(A, B), choose the strat(A,C) where
% lose if B = X
% draw if B = Y
% win if B = Z
decide(strat(A, 'X'), strat(A, C)) :- lose(A, C).
decide(strat(A, 'Y'), strat(A, C)) :- draw(A, C).
decide(strat(A, 'Z'), strat(A, C)) :- win(A, C).

total_score_p2 :-
  read_strats(Sts),
  maplist(decide, Sts, NewSts),
  ind_scores(NewSts, Scs),
  sum_list(Scs, Tot),
  write(Tot).
