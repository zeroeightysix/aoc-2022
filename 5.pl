:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

to_end --> eol.
to_end --> [C], { dif(C, 0'\n) }, to_end.

% Stacks parsing

crate([]) --> "   ".
crate(C)  --> "[",   [Char], "]",   { atom_codes(C, [Char]) }.

crate_line_tr([C | Cs])   --> crate(C), crate_line_tr_r(Cs).
crate_line_tr_r([C | Cs]) --> " ", crate(C), crate_line_tr_r(Cs).
crate_line_tr_r([])       --> eol.

crates_tr([]) --> " 1 ", to_end , eol.
crates_tr([Cs | Css]) --> crate_line_tr(Cs), crates_tr(Css).

crates(C) --> crates_tr(A), { transpose(A, C) }.

empty([]).
nonempty(L, F) :- exclude(empty, L, F).

crates_filt(F) --> crates(C), { maplist(nonempty, C, F) }.

% Move parsing

move(Q, F, T) --> "move ", integer(Q), " from ", integer(F), " to ", integer(T), eol.
moves([]) --> eos.
moves([(Q,F,T) | Ms]) --> move(Q,F,T), moves(Ms).

% Input

input(C, M) --> crates_filt(C), moves(M).

split(At, L, L1, L2) :-
  length(L1, At),
  append(L1, L2, L).

replace(Idx, L1, E2, L2) :-
  nth1(Idx, L1, _, R),
  nth1(Idx, L2, E2, R).
  
perform_rev((Q,F,T), C, Cout) :-
  % 'Remove' the blocks from the 'F'rom stack
  nth1(F, C, FStack),
  split(Q, FStack, BlocksR, FResidual),
  reverse(BlocksR, Blocks),
  replace(F, C, FResidual, CIntermediary),
  % 'Add' the blocks to the 'T'o stack
  nth1(T, C, TStack),
  append(Blocks, TStack, TCompound),
  replace(T, CIntermediary, TCompound, Cout).

perform((Q,F,T), C, Cout) :-
  % 'Remove' the blocks from the 'F'rom stack
  nth1(F, C, FStack),
  split(Q, FStack, Blocks, FResidual),
  replace(F, C, FResidual, CIntermediary),
  % 'Add' the blocks to the 'T'o stack
  nth1(T, C, TStack),
  append(Blocks, TStack, TCompound),
  replace(T, CIntermediary, TCompound, Cout).

proc(_, C, [], C).
proc(Method, C, [M|Ms], Out) :-
  call(Method, M, C, C2),
  proc(Method, C2, Ms, Out).

head([H|_], H).

solve(Method) :-
  phrase_from_file(input(C, M), "5.txt"),
  call(Method, C, M, O),
  maplist(head, O, Ats),
  atom_string(Ats, Msg),
  writeln(Msg).

p1 :- solve(proc(perform_rev)).
p2 :- solve(proc(perform)).
