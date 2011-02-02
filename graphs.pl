%%%
% Marian Marek Kedzierski
% mk248269
% 2.02.2011
% Modul generujacy grafy zaleznosci
%%%

:- ensure_loaded([utils]).

%
% Reprezentacja grafow:
%   Kazdy graf jest lista elementow (V, C, E), gdzie:
%   V - wierzcholek
%   C - klasa abstrakcji wierzcholka (sluzaca m.in. do przypisania mu SCC)
%   E - lista takich U, ze V -> U jest krawedzia w grafie
%

%
% Zdaje sobie sprawe, ze budowanie grafow przy uzyciu list otwartych
% w sposob, w jaki to dokonuje ponizej
% mimo swojej prostoty pozostawia luke w postaci potencjalnej wiekszej
% zlozonosci (koniecznosc przejrzenia calej listy sasiedztwa w celu jej 
% rozszerzenia). Gdybym mial jeszcze chwilke, to mysle, ze przepisalbym
% predykaty budujace grafy tak, aby najpierw tworzyly liste krawedzi,
% wykonywaly na niej sort_unique a dopiero potem budowaly faktyczny graf
% w lepszy sposob wykorzystujac listy otwarte.
% Niestety zbliza sie juz polnoc, a ja nie chce ryzykowac, ze wprowadze
% do programu nowe bledy, wiec zostawiam to tak, jak jest
% (ewentualnie moge to zmienic pozniej, jesli uzna to Pani za sluszne).
%

%
% remove_nonvertices(+Graph, +NonVertices, -NewGraph).
%   NewGraph jest grafem Graph pozbawionym elementow NonVertices,
%   o ile sa one elementami wystepujacymi jako konce krawedzi,
%   ale nie bedacymi wierzcholkami grafu Graph
%
remove_nonvertices([], _, []).
remove_nonvertices([(V, C, E) | G], ToRemove, [(V, C, Enew) | Gnew]) :-
  filter_out(E, ToRemove, Enew),
  remove_nonvertices(G, ToRemove, Gnew).

%
% build_scc_graph(+G, +SCCs, +LoopAlert, -Gscc).
%   Gscc jest DAG-iem silnie spojnych skladowych SCC grafu G,
%   LoopAlert jest predykatem o arnosci 2 uruchamianym, gdy
%   G zawiera krawedz pomiedzy pewna silnie spojna skladowa a nia sama
%
build_scc_graph(G, SCC, LoopAlert, Gscc) :-
  build_scc_graph_p(G, SCC, LoopAlert, Gscc),
  close_graph(Gscc).

%
% build_scc_graph_p(+G, +SCCs, +LoopAlert, -Gscc).
%   tak samo jak build_scc_graph, ale Gscc jest grafem otwartym
%
build_scc_graph_p([], _, _, _).
build_scc_graph_p([(V, _, E) | G], SCCs, LoopAlert, Gscc) :-
  member((V, SccV), SCCs),
  openset_add((SccV, _, SccE), Gscc),
  build_scc_graph_pom(E, SCCs, V, SccV, LoopAlert, SccE),
  build_scc_graph_p(G, SCCs, LoopAlert, Gscc).

%
% build_scc_graph_pom(+E, +SCCs, +V, +SccV, +LoopAlert, -SccE).
%   E - lista pozostalych sasiadow V
%   SCC - przyporzadkowanie wierzcholek -> silnie spojna skladowa
%   SccV - silnie spojna skladowa wierzcholka V
%   LoopAlert - do powiadamiania o porazce
%   SccE - krawedzie w grafie Gscc wychodzace z SccV
%
build_scc_graph_pom([], _, _, _, _, _).
build_scc_graph_pom([U | E], SCCs, V, SccV, LoopAlert, SccE) :-
  member((U, SccV), SCCs), !,
  CallableAlert =.. [LoopAlert, V, U],
  call(CallableAlert),
  build_scc_graph_pom(E, SCCs, V, SccV, LoopAlert, SccE).
build_scc_graph_pom([U | E], SCCs, V, SccV, LoopAlert, SccE) :-
  member((U, SccU), SCCs),
  SccU \= SccV,
  openset_add(SccU, SccE),
  build_scc_graph_pom(E, SCCs, V, SccV, LoopAlert, SccE).

%
% scc(+Graph, -SCC).
%   SCC jest przyporzadkowaniem (wierzcholek -> silnie spojna skladowa)
%   w grafie Graph
%
scc(G, SCC) :-
  topological_sort(G, Przetw),
  reverse_graph(G, GR),
  dfs_init(Przetw, GR, ([], []), (_, _), 0),
  extract_scc(GR, SCC).

%
% extract_scc(+Graph, -SCC).
%   SCC jest przyporzadkowaniem silnie spojnych skladowych w grafie Graph,
%   w ktorym te skladowe juz zostaly wyznaczone
%
extract_scc([], []).  
extract_scc([(V, C, _) | G], [(V, C)|SCC]) :-
  extract_scc(G, SCC).
  
%
% reverse_graph(+G, -GR).
%   krawedz (U, V) nalezy do grafu GR wtw. gdy (V, U) nalezy do grafu G
%
reverse_graph(G, GR) :- 
  vertices(G, Vs),
  vertices(GR, Vs),
  rev_graph(G, GR),
  close_graph(GR).

%
% rev_graph(+G, -GR) - pomocniczy dla reverse_graph
%
rev_graph([], _).
rev_graph([(V, _, E) | G], GR) :-
  rev_iter(E, V, GR),
  rev_graph(G, GR).

%
% rev_graph(+E, +V, -GR) - pomocniczy dla rev_graph
%   E - lista wierzcholkow, do ktorych biegna krawedzie z V
%   GR - docelowy graf odwrocony
%
rev_iter([], _, _).
rev_iter([U|E], V, GR) :-
  member((U, _, EU), GR),    % GR jest lista zamknieta - jednokrotny sukces
  openset_add(V, EU),
  rev_iter(E, V, GR).

%
% vertices(+Graph, -Vertices).
%   Vertices jest lista wierzcholkow grafu G
%
vertices([(V, _, _) | G], [V | Vs]) :- !,
  vertices(G, Vs).
vertices([], []).

%
% close_graph(?G) - graf G jest domkniety:
%   - ma domknieta liste wierzcholkow
%   - kazda lista sasiedztwa jest domknieta
%
close_graph([]) :- !.
close_graph([(_, _, E) | G]) :- 
  close_list(E),
  close_graph(G).
  
%
% topological_sort(+G, -SortedV).
%   SortedV jest kolejnoscia topologiczna wierzcholkow w grafie G
%
topological_sort(G, SortedV) :-
  vertices(G, W),
  dfs_init(W, G, ([], []), (_, SortedV), 0).
  
%
% dfs_init(+Wierzch, +G, +Times_in, ?Times_out, Lp).
%   Wierzch - wierzcholki do przeszukania (w odpowiedniej kolejnosci)
%   G - graf
%   Times_in = (OdwIn, PrzetwIn) - aktualne listy odwiedzonych 
%     i przetworzonych wierzcholkow
%   Times_out = (OdwOut, PrzetwOut) - jw. ale listy sa ostateczne
%   Lp - liczba porzadkowa (numer aktualnej glownej iteracji algorytmu DFS)
%
dfs_init([], _, Times, Times, _).
dfs_init([W|L], G, (Odw, Prz), Post, Iter) :-
  member(W, Odw), !,                        % czerwone odciecie
  dfs_init(L, G, (Odw, Prz), Post, Iter).
dfs_init([W|L], G, Pre, Post, Iter) :-
  % Pre = (Odw, _), nonmember(W, Odw),   
  Iter1 is Iter + 1,
  dfs(W, G, Pre, Middle, Iter1),
  dfs_init(L, G, Middle, Post, Iter1).

%
% dfs(+Wierzch, +G, +Times_in, ?Times_out, +Lp).
%   realizacja jednej iteracji dfs
%   argumenty jak w dfs_init
%
dfs(U, G, (Odw, Prz), (OdwNew, [U|Prz1]), Iter) :-
  member((U, Iter, E), G),    % G jest grafem zamknietym - jednokrotny sukces
  dfs_iter(E, G, ([U|Odw], Prz), (OdwNew, Prz1), Iter).

%
% dfs_iter(+Edges, +G, +TimesIn, ?TimesOut, +Lp).
%   iteracja po krawedziach Edges jednego wierzcholka
%   reszta argumentow jak w dfs_init
%
dfs_iter([], _, Times, Times, _).
dfs_iter([V|R], G, Pre, Post, Iter) :-
  try_visit(V, G, Pre, Middle, Iter),
  dfs_iter(R, G, Middle, Post, Iter).

%
% try_visit(+V, +G, +Times_in, ?Times_out).
%   proba odwiedzenia wierzcholka V (jesli nie byl wczesniej odwiedzony)
%   reszta argumentow jak w dfs_init
%
try_visit(V, _, (Odw, Prz), (Odw, Prz), _) :-
  member(V, Odw), !.                         % czerwone odciecie
try_visit(V, G, Pre, Post, Iter) :-
  % Pre = (Odw, _), member(V, Odw), !.   
  dfs(V, G, Pre, Post, Iter).
