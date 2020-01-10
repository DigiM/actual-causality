:- module(utils,
    [
        tupleToList/2
    ]).

tupleToList((T, TTuple), [T | TList]) :- tupleToList(TTuple, TList), !.
tupleToList(T, [T]).
