:- module(
    actualCauseModel,
    [
        markActualWorld/1,
        markExogenousVariables/1,
        markClausalRule/2,
        isActualWorld/1,
        isExogenousVariable/1,
        isClausalRule/2
    ]).

:- use_module(operators).

:- dynamic isActualWorld/1.
:- dynamic isExogenousVariable/1.
:- dynamic isClausalRule/2.

markActualWorld([]).

markActualWorld([not L | LitList]) :-
    !, \+ isActualWorld(L),
    markActualWorld_(not L),
    markActualWorld(LitList).

markActualWorld([L | LitList]) :-
    \+ isActualWorld(not L),
    markActualWorld_(L),
    markActualWorld(LitList).

markActualWorld_(L) :-
    \+ isActualWorld(L),
    assert(isActualWorld(L)).
markActualWorld_(_).

markExogenousVariables([]).

markExogenousVariables([Var | VarList]) :-
    markExogenousVariable(Var),
    markExogenousVariables(VarList).

markExogenousVariable(Var) :-
    \+ isExogenousVariable(Var),
    assert(isExogenousVariable(Var)).
markExogenousVariable(_).

markClausalRule(H, B) :-
    \+ isClausalRule(H, B),
    assert(isClausalRule(H, B)).
markClausalRule(_, _).
