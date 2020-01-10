:- module(alpModel,
    [
        markRule/2,
        markHeadPredicate/1,
        markPredicates/1,
        markPredicate/1,
        markAbducibles/1,
        isRule/2,
        isHeadPredicate/1,
        isPredicate/1,
        isAbducible/1
    ]).


:- use_module(operators).

:- dynamic isRule/2.
:- dynamic isHeadPredicate/1.
:- dynamic isPredicate/1.
:- dynamic isAbducible/1.

markRule(H, B) :-
    \+ isRule(H, B),
    assert(isRule(H, B)).
markRule(_, _).

markHeadPredicate(T) :-
    T =.. [Pred | TermList],
    length(TermList, Arity),
    \+ isHeadPredicate(Pred/Arity),
    (
        \+ isAbducible(Pred/_), !;
        retractall(isAbducible(Pred/_))
    ),
    assert(isHeadPredicate(Pred/Arity)).
markHeadPredicate(_).

markPredicates([]).
markPredicates([Pred | PredList]) :-
    markPredicate(Pred),
    markPredicates(PredList).

markPredicate(not T) :- markPredicate(T).
markPredicate(T) :-
    T =.. [Pred | TermList],
    length(TermList, Arity),
    \+ isPredicate(Pred/Arity),
    \+ isAbducible(Pred/_),
    assert(isPredicate(Pred/Arity)).
markPredicate(_).

markAbducibles([]).
markAbducibles([Abd | AbdList]) :-
    markAbducible(Abd),
    markAbducibles(AbdList).

markAbducible(Abd) :- 
    \+ isAbducible(Abd),
    assert(isAbducible(Abd)).
markAbducible(_).
