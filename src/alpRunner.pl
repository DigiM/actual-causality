:- module(alpRunner, [loadALP/1, query/3]).

:- use_module(library(lists), [member/2, append/3]).
:- use_module(operators).

loadALP(Filename) :- [Filename].

query([], I, I).
query([not Query | QueryList], I, O) :-
    !,
    atom_concat('not_', Query, NotQuery),
    validate(NotQuery, I, E),
    query(QueryList, E, O).
query([Query | QueryList], I, O) :-
    validate(Query, I, E),
    query(QueryList, E, O).

%% Abduction Utils %%
produce_context(I, I, []).
produce_context(O, I, [not Abd | AbdList]) :-
    !,
    \+ member(Abd, I),
    addAbducible(not Abd, I, INext),
    produce_context(O, INext, AbdList).
produce_context(O, I, [Abd | AbdList]) :-
    \+ member(not Abd, I),
    addAbducible(Abd, I, INext),
    produce_context(O, INext, AbdList).
    
insert_abducible(not T, I, O) :-
    !,
    \+ member(T, I),
    addAbducible(not T, I, O).
insert_abducible(T, I, O) :-
    \+ member(not T, I),
    addAbducible(T, I, O).

%% Misc Utils %%
addAbducible(Abd, I, I) :-
    member(Abd, I), !.
addAbducible(Abd, I, [Abd | I]).

validate(Query, I, O) :-
    Query =.. [Head | Body],
    append(Body, [I, O], Body_),
    Query_ =.. [Head | Body_],
    Query_.

