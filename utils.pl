%%%
% Marian Marek Kedzierski
% mk248269
% 2.02.2011
% Modul z predykatami sluzacymi jako narzedzia
%%%

:- op(500, xfx, --).

% represents_const(+Atom) - Atom reprezentuje stala w analizowanym programie
represents_const(Atom) :-
  atom_codes(Atom, [FstLetter|_]),
  small_letter(FstLetter).

small_letter(Letter) :-
  Letter >= 97,
  Letter =< 122.

% replaceVN(+VariableNames) - przypisuje zmiennym ich nazwy
replaceVN([]).
replaceVN([N = X | L]) :-
  X = '$VAR'(N),
  replaceVN(L).

% singleton_names(+Sings, -Names) - Names sa nazwami singletonow w Sings
singleton_names([], []).
singleton_names([Name = _|S], [Name|N]) :- 
  singleton_names(S, N).

% current_line(?Line) - Line jest numerem aktualnego wiersza na wejsciu
current_line(Line) :-
  current_input(Stream),
  line_count(Stream, Line_1),
  Line is 1 + Line_1.

% operator_type_position(?Typ operatora, ?Pozycja operatora).
operator_type_position(xfx, infix).
operator_type_position(xfy, infix).
operator_type_position(yfx, infix).
operator_type_position(xf, postfix).
operator_type_position(yf, postfix).
operator_type_position(fx, prefix).
operator_type_position(fy, prefix).

% close_list(?L) - domkniecie listy L
close_list([]) :- !.
close_list([_|L]) :- close_list(L).

% close_mapping(?L) - domkniecie odwzorowania
close_mapping([]) :- !.
close_mapping([(_, L) | LL]) :- 
  close_list(L),
  close_mapping(LL).

%
% filter_out(+List, +ElemsToRemove, -ClearedList).
%   ClearedList jest lista List po usunieciu elementow z listy ElemsToRemove
%
filter_out([], _, []).
filter_out([X | L], ToRemove, CL) :-
  member(X, ToRemove), !,
  filter_out(L, ToRemove, CL).
filter_out([X | L], ToRemove, [X | CL]) :-
  % nonmember(X, ToRemove),
  filter_out(L, ToRemove, CL).

%
% openset_add(+E, ?L) - dodaj E do otwartej listy L, 
%   jesli jeszcze go w niej nie ma
%
openset_add(E, L) :-
  once(member(E, L)).

% sort_unique(+L, -SL) - SL jest posortowana lista L bez powtorzen
sort_unique(List, SetList) :-
  sort(List, Sorted),
  unique(Sorted, SetList).

% unique(+L, -UL) - UL jest lista L bez powtorzen elementow obok siebie
unique([], []).
unique([X | List], [X | UniqueList]) :- 
  unique(List, X, UniqueList).

%
% unique(+L, +Last, -UL) - [Last|UL] jest lista [Last|L] bez 
%   powtorzen obok siebie
%
unique([], _, []).
unique([X|L], X, UL) :- !,
  unique(L, X, UL).
unique([X|L], Y, [X|UL]) :-
  X \= Y,
  unique(L, X, UL).

%  
% built_in_pred(+Pred) - Pred jest wbudowanym 
%   predykatem / operatorem /odcieciem
%
built_in_pred(!/0).
built_in_pred(Operator/_) :-
  atom(Operator),
  current_op(_, _, Operator).
built_in_pred(Pred/Arity) :-
  functor(Term, Pred, Arity),
  predicate_property(Term, built_in).

%
% lr_member(+E, ?L) - E jest pierwszym swoim wystapieniem 
%   na liscie roznicowej L
%
lr_member(E, P -- _) :- 
  nonvar(P), P = [E|_], !.
lr_member(E, P -- K) :- 
  nonvar(P), P = [X|R], X \= E, lr_member(E, R -- K).

% rev_list(+L, -R) - R jest odwrocona lista L
rev_list(L, R) :- rev_list(L, [], R).
rev_list([], A, A).
rev_list([X|L], A, R) :- rev_list(L, [X|A], R).

%
% inverse_image(+Fun, -IM) - IM jest przeciwobrazem funkcji F, 
%   gdzie funkcje reprezentujemy jako listy elementow (argument, wartosc)
%
inverse_image(Fun, IM) :- 
  inverse_image_pom(Fun, IM), 
  close_mapping(IM).

% tak, jak inverse_image, ale IM drugi argument jest przyporzadowaniem otwarta
inverse_image_pom([], _).
inverse_image_pom([(Value, Key) | Rest], IM) :-
  openset_add((Key, Vs), IM),
  openset_add(Value, Vs),
  inverse_image_pom(Rest, IM).
