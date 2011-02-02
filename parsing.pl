%%%
% Marian Marek Kedzierski
% mk248269
% 2.02.2011
% Modul parsujacy argumenty i tekst programu
%%%

:- ensure_loaded([utils, verifying]).

%
% parse_args(+Argv, -Wejscie, -Wyjscie) - ustalenie strumieni I/O
%
parse_args([In], In, user) :- !.
parse_args([In, Out], In, Out) :- !.
parse_args(_, _, _) :-
  write('### Niepoprawne wywolanie, usage: metaprogram <PlikWe> <PlikWy>\n'), nl, halt.

%
% parse_prog(-Program).
%   Program jest lista kolejnych termow wczytanych na biezacym wejsciu
%   UWAGA: predykat pozostawia jako efekt uboczny 
%     przypisanie zmiennym programu ich nazw
%
parse_prog(Program) :-
  next_term(Term),
  parse_prog_pom(Term, Program).
parse_prog(_) :-
  current_line(Line),
  write('### Blad skladniowy w poblizu '), write(Line), write(' wiersza. Koncze dzialanie'), nl, 
  halt.

%
% parse_prog_pom(+BiezacyTerm, -ResztaProgramu) - pomocniczy dla parse_prog
%
parse_prog_pom(end_of_file, []) :- !.
parse_prog_pom((:- op(P, T, O)), [(:- op(P, T, O)) | Program]) :- !,
  op(P, T, O), 
  next_term(T2),
  parse_prog_pom(T2, Program).
parse_prog_pom(T, [T | Program]) :-
  T \= end_of_file,
  next_term(T2),
  parse_prog_pom(T2, Program).

%
% next_term(-Term) - wczytuje Term z biezacego wejscia unifikujac zmienne
%   z ich nazwami oraz sprawdzajac, czy program nie ma zadnych singletonow
%
next_term(Term) :-
  read_term(Term, [variable_names(VN), singletons(S), syntax_errors(quiet)]),
  replaceVN(VN),
  check_singletons(S).
