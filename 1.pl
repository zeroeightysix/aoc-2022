use_module(library(list_util)).

empty([]).

numbers_strings([], []).
numbers_strings([N | NT], [S | ST]) :-
  number_string(N, S),
  numbers_strings(NT, ST).

read_cals(Calories) :-
  % read entire file
  read_file_to_string("1.txt", Input, []),
  % split by newline
  split_string(Input, "\n", "", InputSplit),
  % split by empty lines
  split(InputSplit, "", CalStrings),
  % strings -> ints conversion
  maplist(numbers_strings, CaloriesUnfilt, CalStrings),
  % remove the last empty list (originating from the newline at the end)
  exclude(empty, CaloriesUnfilt, Calories).

sum_cals(C, S) :-
  maplist(sum_list, C, S).
  
max_elf :-
  read_cals(C),
  sum_cals(C, S),
  max_list(S, M),
  write(M).
  
max_elves :-
  read_cals(C),
  sum_cals(C, S),
  sort(0, @>, S, SSort),
  take(3, SSort, Max),
  sum_list(Max, MaxSum),
  write(MaxSum).
