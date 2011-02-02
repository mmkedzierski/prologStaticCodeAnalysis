%%%
% Marian Marek Kedzierski
% mk248269
% 2.02.2011
% Modul analizujacy warstwowosc programu
%%%

:- op(500, xfx, --).

:- ensure_loaded([utils, graphs, analysing, printing]).

%
% Algorym dzielenia programu na warstwy jest nastepujacy:
% 1. Zbuduj mape programu (graf zaleznosci)
% 2. Wyznacz silnie spojne skladowe (SCC) calego grafu zaleznosci
% 3. Przetworz graf wejsciowy na DAG silnie spojnych skladowych Gscc
% 4. Posortuj Gscc topologicznie
% 5. W kolejnosci odwrotnej do kolejnosci topologicznej przypisuj
%   wszystkim wierzcholkom V grafu Gscc numer warstwy, wybierajac 
%   najmniejszy numer:
%   a) wiekszy od wszystkich numerow warstw, do ktorych biegna z V 
%      krawedzie negatywne
%   b) wiekszy badz rowny numerom warstw, do ktorych biegna z V 
%      krawedzie pozytywne
%   jesli powyzsze nie da sie zrealizowac, to graf nie jest warstwowy
% 6. Przypisz kazdemu predykatowi numer warstwy, do ktorej nalezy
%   jego silnie spojna skladowa.
%


%
% funkcje pomocnicze sluzace do poinformowania uzytkownika o przyczynie
% przerwania algorytmu podzialu na warstwy
%
pos_loop_alert(_, _).
neg_loop_alert(Pred1, Pred2) :-
  write('*** Program nie jest warstwowy.'), nl,
  write('Powod braku warstwowosci: krawedz negatywna ('), write(Pred1), write(' -> '), write(Pred2),
  write(') lezy na cyklu w grafie zaleznosci.'), nl,
  nl,
  fail.

%
% layers(+Map, -Layers).
%   Layers to przyporzadkowanie (predykat -> numer warstwy) 
%   dla programu o mapie Map
%
layers((All, Neg), Layers) :-
  scc(All, SCCs),
  build_scc_graph(All, SCCs, pos_loop_alert, GsccAll),
  build_scc_graph(Neg, SCCs, neg_loop_alert, GsccNeg),
  assign_layers(GsccAll, GsccNeg, SccLayers),
  vertices(All, Vertices),
  lookup_layers(Vertices, SCCs, SccLayers, Layers).

%
% assign_layers(+GsccAll, +GsccNeg, -SccLayers).
%   SccLayers jest przyporzadkowaniem (Scc -> warstwa) w programie
%   GsccAll - graf SCC wszystkich zaleznosci 
%   GsccNeg - graf SCC zaleznosci negatywnych
%
assign_layers(GsccAll, GsccNeg, SccLayers) :- 
  topological_sort(GsccAll, SortedLG),
  rev_list(SortedLG, Order),
  assign_layers_pom(Order, GsccAll, GsccNeg, A -- A, SccLayers -- []).
  
%
% assign_layers_pom(+Sccs, +GsccAll, +GsccNeg, +LRLayersIn, -LRLayersOut).
%   pomocniczy dla assign_layers
%   Sccs - wierzcholki grafu SCC pozostale do przypisania warstw
%   LRLayersIn - lista roznicowa ustalonego juz przyporzadkowania warstw
%   LRLayersOut - koncowa lista roznicowa z przyporzadkowaniem warstw
%
assign_layers_pom([], _, _, Out, Out).
assign_layers_pom([U | R], PosG, NegG, LayP -- [(U, Id) | LayK], Out) :-
  member((U, _, NegE), NegG),
  find_max(NegE, LayP -- [(U, Id) | LayK], MaxNegId),
  NegId is MaxNegId + 1,
  member((U, _, PosE), PosG),
  find_max(PosE, LayP -- [(U, Id) | LayK], PosId),
  Id is max(NegId, PosId),
  assign_layers_pom(R, PosG, NegG, LayP -- LayK, Out).

%
% find_max(+E, +LRLayers, -MaxId).
%   MaxId jest maksymalnym numerem warstwy wsrod Scc bedacych elementami E
%   LRLayers - lista roznicowa aktualnego przyporzadkowania warstw
%
find_max([], _, 0).
find_max([V | Neg], KnownLayers, MaxId) :-
  find_max(Neg, KnownLayers, MaxId1),
  lr_member((V, IdV), KnownLayers),
  nonvar(IdV),   % w przeciwnym razie program nie jest warstwowy
  MaxId is max(MaxId1, IdV).

%
% lookup_layers(+Vertices, +SCCs, +SccLayers, -Layers).
%   Vertices - lista wierzcholkow
%   Layers - przyporzadkowanie wierzcholkom z Vertices ich warstw
%   SccLayers - przyporzadkowanie SCC ich warstw
%
lookup_layers([], _, _, []).
lookup_layers([V | Vs], SCCs, SccLayers, [(V, LayerId) | Ls]) :-
  member((V, Scc), SCCs),
  member((Scc, LayerId), SccLayers),
  lookup_layers(Vs, SCCs, SccLayers, Ls).
