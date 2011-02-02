%%%
% Marian Marek Kedzierski
% mk248269
% 2.02.2011
% Modul obslugujacy ladne wypisywanie kodu
%%%

:- ensure_loaded([utils]).

%
% pretty_print(+Program) - wypisz listing programu Program
%
pretty_print(Prog) :-
  pretty_print(Prog, nil).

%
% pretty_print(+Program, +LastPredicate) - j.w.
%   LastPredicate - sygnatura ostatnio napotkanego predykatu
%
pretty_print([], _).
pretty_print([Clause|Prog], LastPred) :-
  pred_signature(Clause, Pred),
  try_start_pred(Pred, LastPred),
  print_clause(Clause),
  pretty_print(Prog, Pred).

%
% try_start_pred(+CurrentPred, +PrevPred) - wypisz znak nowego wiersza,
%   jesli argumenty sa rozne
%
try_start_pred(Pred, Pred) :- !.
try_start_pred(P1, P2) :- P1 \= P2, nl.

%
% print_clause(+Term) - wypisz kolejny term programu (element listy Program)
%
print_clause((:- D)) :- !,
  write(':- '), writeq(D),
  finish_clause.
print_clause((?- D)) :- !,
  write('?- '), writeq(D),
  finish_clause.
print_clause((Head :- Body)) :-
  print_term(Head), write(' :- '),
  print_body(Body, 1),
  finish_clause.
print_clause(Fact) :-
  print_term(Fact),
  finish_clause.

finish_clause :-
  write('.'), nl.

%
% print_term(+Term) - nazwa mowi za siebie
%
print_term(Var) :-
  var(Var), !,
  write('_').
print_term('$VAR'(Var)) :- !,
  writeq('$VAR'(Var)).
print_term(Tuple) :-
  Tuple =.. [',' | Elems], !,
  print_args(Elems).
print_term([]) :- !,
  write([]).
print_term(Lista) :-
  Lista =.. ['.', H, T], !,
   print_list(H, T).
print_term(Module:Pred) :-
  write(Module), write(':'),
  print_term(Pred).
print_term(Term) :-
  Term =.. [PredName | Args],
  atom(PredName),
  current_op(_, Type, PredName),
  operator_type_position(Type, Pos),
  print_operator(Pos, PredName, Args).
print_term(Term) :-
  Term =.. [PredName | Args],
  writeq(PredName),
  print_args(Args).

%
% print_operator(+Pozycja, +Nazwa, +Argumenty) - wypisz operator z operandami
%   Pozycja - jedna ze stalych: prefix, infix, postfix
%
print_operator(_, _, []) :- !.
print_operator(prefix, Symbol, [A1 | Rest]) :-
  write('('), 
  writeq(Symbol), write(' '), 
  print_terms([A1 | Rest]),
  write(')').
print_operator(infix, _, [X]) :-
  print_term(X).
print_operator(infix, Symbol, [A1, A2 | Rest]) :-
  print_term(A1), write(' '),
  writeq(Symbol), write(' '),
  print_operator(infix, Symbol, [A2 | Rest]).
print_operator(postfix, Symbol, [A1 | Rest]) :-
  write('('), 
  print_terms([A1 | Rest]),
  write(') '),
  writeq(Symbol).
  
%
% print_terms(+ListOfTerms) - wypisz liste termow bez nawiasow i przecinkow
%
print_terms([]).
print_terms([X | L]) :-
  print_term(X),
  print_terms(L).

%
% print_args(+ListOfTerms) - wypisz liste termow z nawiasami i przecinkami
%
print_args([]).
print_args([A|Args]) :-
  write('('), 
  print_args_pom([A|Args]),
  write(')').

%
% print_args_pom(+Lista) - wypisz liste termow z przecinkami miedzy nimi
%
print_args_pom([]).
print_args_pom([A]) :-
  print_term(A).
print_args_pom([A, B | ArgList]) :-
  print_term(A), write(', '),
  print_args_pom([B | ArgList]).

%
% print_list(+Head, +Tail) - wypisz liste [Head | Tail]
%
print_list(Head, Tail) :-
  write('['),
  print_list_pom(Head, Tail),
  write(']').

%
% print_list_pom(Head, Tail)  - wypisanie 'wnetrza' listy
%
print_list_pom(Head, []) :- !,
  print_term(Head).
print_list_pom(Head, Tail) :-
  Tail =.. ['.', H2, T2], !,
  print_term(Head), write(', '),
  print_list_pom(H2, T2).
print_list_pom(Head, Tail) :-
  print_term(Head),
  write(' | '),
  print_term(Tail).

%
% print_body(+Body, +Depth) - wypisz cialo klauzuli na glebokosci wciec Depth
%
print_body(Body, Depth) :-
  body2list(Body, BodyElemList),
  print_body_pom(BodyElemList, Depth).
  
%
% print_body_pom(+BodyList, +Depth) - wypisz kolejne literaly z ciala klauzuli
%   na glebokosci wciec Depth
%
print_body_pom([X], Depth) :-
  print_body_elem(X, Depth).
print_body_pom([T, T2 | Rest], Depth) :-  
  print_body_elem(T, Depth), write(', '),
  print_body_pom([T2 | Rest], Depth).

%
% print_body_elem(+Literal, +Depth) - wypisz literal na glebokosci wciec Depth
%
print_body_elem(!, _) :- !, write(!).
print_body_elem((\+ Negated), Depth) :- !,
  nl, indent(Depth), write('\\+ ( '),
  Depth1 is Depth + 1,
  print_body(Negated, Depth1),
  nl, indent(Depth), write(')').
print_body_elem((X ; Y), Depth) :-
  nl, indent(Depth), write('( '),
  Depth1 is Depth + 1,
  print_alternative((X ; Y), Depth1),
  nl, indent(Depth), write(')').
print_body_elem(Term, Depth) :-
  (nonvar(Term) -> Term \= !),
  nl,  indent(Depth), 
  print_term(Term).
 
%
% print_alternative(+Alt, +Depth)
%   wypisanie termu Alt bedacego alternatywa pewnych termow
%
print_alternative((X ; Rest), Depth) :- !,
  print_body_elem(X, Depth), write('; '),
  print_alternative(Rest, Depth).
print_alternative(X, Depth) :-
  print_body_elem(X, Depth).

%
% indent(+Depth) - wypisz wciecie o glebokosci Depth
%
indent(0).
indent(Depth) :-
  Depth > 0, write('  '),    % pojedyncze wciecie to dwie spacje
  Depth_1 is Depth - 1,
  indent(Depth_1).
