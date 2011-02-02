%%%
% Marian Marek Kedzierski
% mk248269
% 2.02.2011
% Modul analizujacy strukture programu:
% - budujacy mape zaleznosci programu: program_map/2
% - znajdujacy niezadeklarowane predykaty: undeclared_preds/2
% - znajdujacy stale: constants/1
% - wyznaczajacy sygnatury predykatow
%%%

:- op(500, xfx, --).

:- ensure_loaded([utils]).


%
% program_map(+Program, -Map).
%   Map jest mapa zaleznosci w programie Program.
%   Jest ona postaci (GraphAll, GraphNeg), gdzie:
%     GraphAll - graf wszystkich zaleznosci, 
%     GraphNeg - graf zaleznosci negatywnych,
%
program_map(Program, Map) :- 
  clause_signs(Program, ClauseSignList),
  vertices(GraphAll, ClauseSignList),
  vertices(GraphNeg, ClauseSignList),
  program_map_pom(Program, (GraphAll, GraphNeg)),
  close_graph(GraphAll),
  close_graph(GraphNeg),
  check_and_clear_undeclared_preds((GraphAll, GraphNeg), Map).

%
% clause_signs(+Program, -ClauseSigns).
%   ClauseSigns jest lista sygnatur wszystkich klauzul programu Program
%   (posortowana i bez powtorzen)
%
clause_signs(Program, ClauseSigns) :-
  clause_signs_pom(Program, RawClauseSigns),
  sort_unique(RawClauseSigns, ClauseSigns).

%
% clause_signs_pom(+Program, -ClauseSigns) - pomocniczy dla clause_signs
%   ClauseSigns jest lista sygnatur wszystkich klauzul programu Program
%
clause_signs_pom([], []).
clause_signs_pom([Clause | Prog], [Sign | RawClauseSigns]) :-
  pred_signature(Clause, Sign),
  clause_signs_pom(Prog, RawClauseSigns).

%
% program_map_pom(+Program, +Map).
%   tak samo jak program_map, ale Map jest para otwartych grafow,
%   zainicjowanych juz na wejsciu
%
program_map_pom([], _).
program_map_pom([Clause | Prog], (All, Neg)) :-
  pred_signature(Clause, Sign),
  pred_body_list(Clause, BodyElemList),
  member((Sign, _, Eall), All),
  member((Sign, _, Eneg), Neg),
  program_map_add(BodyElemList, Eall, Eneg, false),
  program_map_pom(Prog, (All, Neg)).

%
% program_map_add(+BodyElemList, ?Eall, ?Eneg, +IsNeg).
%   BodyElemList - lista literalow
%   Eall - lista otwarta koncow krawedzi
%   Eneg - lista otwarta koncow krawedzi negatywnych
%   dodaj sygnatury predykatow wewnatrz BodyElemList do:
%   - Eall - wszystkich
%   - Eneg - wtedy, gdy IsNeg = true
%
program_map_add([], _, _, _).
program_map_add([Lit|Body], Eall, Eneg, IsNeg) :- 
  analyse_literal(Lit, Eall, Eneg, IsNeg),
  program_map_add(Body, Eall, Eneg, IsNeg).

%
% analyse_literal(+Literal, ?Eall, ?Eneg, +IsNeg).
%   Literal - analizowany literal pewnej klauzuli programu
%   < znaczenie reszty argumentow, takie jak w undecl >
%   dodaj sygnature Literalu do:
%   - Eall - zawsze
%   - Eneg - tylko jesli IsNeg = true
%
analyse_literal((\+ NL), Eall, Eneg, _) :- !,
  body2list(NL, BodyList),
  program_map_add(BodyList, Eall, Eneg, true).
analyse_literal(Lit, Eall, Eneg, IsNeg) :-
  term_signature(Lit, Sign),
  add_appropriate(IsNeg, Sign, Eall, Eneg).

%
% add_appropriate(+IsNeg, +V, ?Eall, ?Eneg).
%   dodaj predykat V do odpowiednich list Eall, Eneg, 
%   jesli V nie jest wbudowanym predykatem
%
add_appropriate(_, V, _, _) :- 
  built_in_pred(V), !. % nie dodawac wbudowanych predykatow
add_appropriate(false, V, Eall, _) :- 
  % \+ built_in_pred(V),
  openset_add(V, Eall).
add_appropriate(true, V, Eall, Eneg) :- 
  % \+ built_in_pred(V),
  openset_add(V, Eall),
  openset_add(V, Eneg).

%
% undeclared_preds(+Graph, -Und).
%   Preds to sa wszystkie elementy, do ktorych wchodza krawedzie 
%   w grafie Graph, ale ktore nie sa wierzcholkami tego grafu
%   
undeclared_preds(Graph, Und) :-
  undecl(Graph, Graph, [], Und1),
  sort_unique(Und1, Und).
  
%
% undecl(+GRest, +Gwhole, +Und, -NewUnd) - pomocniczy dla undeclared_preds
%   GRest - pozostala czesc grafu do przetworzenia
%   Gwhole - caly graf
%   Und - akumulator na liste niezadeklarowanych predykatow
%   NewUnd - akumulator Und powiekszony o niezadeklarowane predykaty
%     w czesci Grest grafu Gwhole
%
undecl([], _, Und, Und).
undecl([(_, _, E) | G], Gwhole, Und, NewUnd) :-
  undecl_pom(E, Gwhole, Und, Und1), 
  undecl(G, Gwhole, Und1, NewUnd).

%
% undecl_pom(+Edges, +Gwhole, +Und, -NewUnd) - pomocniczy dla undecl
%   Edges - pozostale do przetworzenia krawedzie
%   < znaczenie reszty argumentow, takie jak w undecl >
%
undecl_pom([], _, Und, Und).
undecl_pom([V | Rest], Gwhole, Und, NewUnd) :-
  member((V, _, _), Gwhole), !,          % czerwone odciecie
  undecl_pom(Rest, Gwhole, Und, NewUnd).
undecl_pom([V | Rest], Gwhole, Und, NewUnd) :-
  undecl_pom(Rest, Gwhole, [V | Und], NewUnd).


%
% constants(+Program, -Const).
%   Const - lista stalych w programie Program
%
constants(Program, Const) :- 
  add_constants(Program, C1 -- []),
  sort_unique(C1, Const).
 
%
% add_constants(+Program, -LRConst) - predykat pomocniczy dla constants
%   LRConst jest lista roznicowa stalych w programie Program
%
add_constants([], C -- C).
add_constants([Atom | L], [Atom | CP] -- CK) :-
  nonvar(Atom), atom(Atom),
  represents_const(Atom), !,      % czerwone odciecie zwn. koszt
  add_constants(L, CP -- CK).
add_constants([Atom | L], Const) :-
  nonvar(Atom), atom(Atom), !,
  % \+ represents_const(Atom),
  add_constants(L, Const).
add_constants([Term | L], CP -- CK) :-
  nonvar(Term), \+ atom(Term), !,
  Term =.. [_ | Args],
  add_constants(Args, CP -- CS),
  add_constants(L, CS -- CK).
add_constants([Var | L], Const) :-   % Var - zmienna anonimowa programu
  var(Var),
  add_constants(L, Const).

%
% pred_body_list(+Clause, -BodyList).
%   BodyList jest lista literalow w klauzuli Clause
%
pred_body_list((:- Body), BodyList)   :- !, body2list(Body, BodyList).
pred_body_list((?- Body), BodyList)   :- !, body2list(Body, BodyList).
pred_body_list((_ :- Body), BodyList) :- !, body2list(Body, BodyList).
pred_body_list(_, []).    % Fakt

%
% body2list(+Body, -BodyList).
%   BodyList jest lista literalow w ciele Body klauzuli
%
body2list(Body, [Elem | BodyElemList]) :-
  Body =.. [',', Elem, BodyRest], !,
  body2list(BodyRest, BodyElemList).
body2list(LastElem, [LastElem]).

%
% pred_signature(+C, -S).
%   jesli C jest klauzula, to S jest sygnatura predykatu
%   przez nia definiowanego (postac sygnatury: nazwa/arnosc)
%   wpp. S = top_level
%
pred_signature((:- _), top_level) :- !.
pred_signature((?- _), top_level) :- !.
pred_signature((Head :- _), Sign) :- !,   % czerwone odciecie
  term_signature(Head, Sign).
pred_signature(Fact, Sign) :-
  term_signature(Fact, Sign).
  
%
% term_signature(+T, -S).
%   S jest sygnatura glownego predykatu termu T
%   postac sygnatury: nazwa/arnosc
%
term_signature(Module:Pred, Module:Sign) :- !,
  term_signature(Pred, Sign).
term_signature(Term, Name/Arity) :-
  functor(Term, Name, Arity).
