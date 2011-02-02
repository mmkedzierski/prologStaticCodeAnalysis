%%%
% Marian Marek Kedzierski
% mk248269
% 2.02.2011
% Modul wypisujacy ostrzezenia o bledach analizowanego programu
%%%

:- ensure_loaded([utils, analysing]).

%
% check_singletons(+Lista singletonow) - wypisanie ostrzezenia w razie potrzeby
%
check_singletons([]).
check_singletons([S | L]) :-
  current_line(Line),
  singleton_names([S | L], Names), 
  write('### WARNING: Singleton variables '),
  write(Names), write(' in clause ending at line '), 
  write(Line), nl.

%
% check_and_clear_undeclared_preds(RawMap, ClearedMap).
%   ClearedMap jest mapa RawMap po usunieciu niezadeklarowanych 
%   i niewbudowanych predykatow z grafu zaleznosci.
%
check_and_clear_undeclared_preds((All, Neg), (AllNew, NegNew)) :-
  undeclared_preds(All, Preds),
  Preds \= [], !,
  write('### WARNING: Undeclared predicates found: '), nl,
  print_name_list(Preds), nl,
  remove_nonvertices(All, Preds, AllNew),
  remove_nonvertices(Neg, Preds, NegNew).
check_and_clear_undeclared_preds(Map, Map)
