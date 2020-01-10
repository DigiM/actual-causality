use_module(library(lists), member/2, append/3).
:- op(900, fy, not).

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


query([], I, I).
query([not Query | QueryList], I, O) :-
    !,
    atom_concat('not_', Query),
    validate(Query, I, E),
    query(QueryList, E, O).
query([Query | QueryList], I, O) :-
    validate(Query, I, E),
    query(QueryList, E, O).

validate(Query, I, O) :-
    Query =.. [Head | Body],
    append(Body, [I, O], Body_),
    Query_ =.. [Head | Body_],
    Query_.

