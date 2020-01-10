:- module(actualCauseRunner, [setUp/1, findActualCause/2]).

:- use_module(alpRunner).
:- use_module(library(lists), [subset/2]).

:- dynamic isActualCauseSet/1.
:- dynamic isActualCause/1.

setUp(Filename) :-
    loadALP(Filename),
    alpRunner:markActualWorld.

findActualCause(L, ActualCauseList) :- 
    retractall(isActualCauseSet(_)),
    retractall(isActualCause(_)),
    findActualCause_(L),
    markFromSet_,
    findall(L_i, isActualCause(L_i), ActualCauseList).

findActualCause_(L) :-
    query([L], [], O),
    checkConsistent_([L | O]),
    markActualCauseSet_(O),
    fail.
findActualCause_(_).

markActualCauseSet_(O) :-
    \+ checkAndRemove_(O),
    assert(isActualCauseSet(O)).
markActualCauseSet_(_).

markFromSet_ :-
    isActualCauseSet(Set),
    markActualCause_(Set),
    fail.
markFromSet_.

markActualCause_([]).
markActualCause_([L | LList]) :-
    (isActualCause(L); assert(isActualCause(L))),
    markActualCause_(LList).

checkConsistent_([]).
checkConsistent_([not L | LList]) :-
    !, \+ alpRunner:isActualWorld(L),
    checkConsistent_(LList).
checkConsistent_([L | LList]) :-
    \+ alpRunner:isActualWorld(not L),
    checkConsistent_(LList).

checkAndRemove_(O) :-
    isActualCauseSet(Set),
    (
        subset(Set, O), !; 
        subset(O, Set),
        retract(isActualCauseSet(Set)),
        fail
    ).
