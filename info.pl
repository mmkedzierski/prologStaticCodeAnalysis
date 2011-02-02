%%%
% Marian Marek Kedzierski
% mk248269
% 2.02.2011
% Modul z predykatami wypisujacymi informacje o analizowanym programie
%%%

%
% present_program(+Program) - wyswietl informacje o programie
%
present_program([]) :-
  write('### Analizowany program jest pusty.'), nl.
present_program([Cl | Prog]) :-
  Program = [Cl | Prog],
  program_map(Program, Map),
  list_constants(Program),
  list_predicates(Map),
  list_layers(Map),
  program_listing(Program).

% 
% list_constants(+Program) - wyswietl informacje o stalych w programie
%
list_constants(Program) :-
  constants(Program, Consts),
  nl,
  write('*** Lista stalych: '), nl,
  print_name_list(Consts), nl,
  nl.

% 
% list_predicates(+Map) - wyswietl informacje o predykatach 
%   w programie o mapie Map
%
list_predicates((All, _)) :-
  vertices(All, Predicates),
  nl,
  write('*** Lista predykatow: '), nl,
  filter_out(Predicates, [top_level], RealPredicates),
  print_name_list(RealPredicates), nl,
  nl.

% 
% list_layers(+Map) - wyswietl informacje o warstwach programu o mapie Map
%   - sprawdz, czy program jest warstwowy (jesli nie - wskazanie przyczyny)
%    - przypisanie warstw predykatom
%
list_layers(Map) :-
  layers(Map, Layers),
  filter_out(Layers, [(top_level, _)], RealLayers),
  nl,
  write('*** Program jest warstwowy. Podzial programu na warstwy:'), nl,
  inverse_image(RealLayers, InvImg),
  sort(InvImg, LayersList),
  print_layers(LayersList), nl.
list_layers(_).

%
% program_listing(+Program) - ladnie wypisz program
%
program_listing(Program) :-
  nl,
  write('*** Tresc analizowanego programu:'),
  pretty_print(Program),
  nl.

%
% print_layers(+Layers) - wyswietl informacje o warstwach predykatow
%   Layers - przyporzadkowanie: (warstwa -> lista predykatow)
%
print_layers([]).
print_layers([(Layer, PredList) | Rest]) :-
  write('** Warstwa '), write(Layer), write(':'), nl,
  print_name_list(PredList), nl,
  print_layers(Rest).

%
% print_name_list(+List) - wypisz ladnie liste List z informacjami o programie
%
print_name_list([]) :- write('<brak>').
print_name_list([H]) :- write(H).
print_name_list([A, B | R]) :- 
  write(A), write(', '), print_name_list([B | R]).
