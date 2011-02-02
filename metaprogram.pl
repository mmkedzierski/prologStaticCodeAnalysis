%%%
% Marian Marek Kedzierski
% mk248269
% 2.02.2011
%
% Zadanie zaliczeniowe z Pracowni Programowania w Prologu:
%   Metaprogram
%
% Modul glowny programu
%%%

:- ensure_loaded([printing, parsing, graphs, analysing, verifying, layers, info]).

user:runtime_entry(start) :-
  current_prolog_flag(argv, Argv),
  parse_args(Argv, In, Out),
  set_prolog_flag(fileerrors, off),
  see(In), !, tell(Out),
  parse_prog(Program),
  present_program(Program),
  seen, told.

user:runtime_entry(start) :-
  write('### Could not open the input file (does it exist?)'), nl, halt.
