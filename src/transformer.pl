:- module(transformer, [transformAll/0, markForTransformation/1, op(900, fy, not)]).

:- dynamic isRule/2.
:- dynamic isHeadPredicate/1.
:- dynamic isPredicate/1.
:- dynamic isAbducible/1.

transformAll :-
    apostropheTransformation,
    plusTransformation,
    minusTransformation,
    oTransformation.

markForTransformation(abds(AbdList)) :-
    markAbducibles(AbdList).

markForTransformation(H :- B) :-
    tupleToList(B, BList),
    markRule(H, BList),
    markHeadPredicate(H),
    markPredicate(H),
    markPredicates(BList).

markForTransformation(H) :-
    markRule(H, []),
    markHeadPredicate(H),
    markPredicate(H).

markRule(H, B) :-
    \+ isRule(H, B),
    assert(isRule(H, B)).
markRule(_).

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

%% Transformations %%

apostropheTransformation :-
    isRule(H, B),
    writeApostrophe(H, B),
    fail.
apostropheTransformation.

plusTransformation :-
    isHeadPredicate(Pred),
    writePlus(Pred),
    fail.
plusTransformation.

minusTransformation :-
    isHeadPredicate(Pred/Arity),
    findall(Rule, getRule(Pred, Rule), RuleList),
    length(RuleList, M),
    writeMinus(Pred/Arity, M),
    starTransformation(RuleList),
    fail.
minusTransformation :-
    isPredicate(Pred/Arity),
    \+ isHeadPredicate(Pred/Arity),
    writeMinus(Pred/Arity, 0),
    fail.
minusTransformation.

starTransformation(RuleList) :-
    writeStar(RuleList).

oTransformation :-
    isAbducible(Abd),
    writeO(Abd),
    writeNotO(Abd),
    fail.
oTransformation.

%% Write Helpers %%

writeEi(Pred, Num) :-
    Pred =.. [Functor | TermList],
    NumPlusOne is Num + 1,
    write(Functor), write('('), writeTerms(TermList),
    (Num = 0, !, write([]); write('E_'), write(Num)), write(', '), 
    write('E_'), write(NumPlusOne), write(')').


writeTerms([]).
writeTerms([Term | TermList]) :-
    write(Term), write(', '),
    writeTerms(TermList).

writeX(0) :- !.
writeX(Num) :- writeX_(Num).
writeX(0, nocomma) :- !.
writeX(Num, nocomma) :- write('('), writeX_(Num, nocomma).

writeX_(0) :- !.
writeX_(Num) :-
    write('X_'), write(Num), write(', '),
    NumMinusOne is Num - 1,
    writeX_(NumMinusOne).
writeX_(1, nocomma) :- 
    !, write('X_1'), write(')').
writeX_(Num, nocomma) :-
    write('X_'), write(Num), write(', '),
    NumMinusOne is Num - 1,
    writeX_(NumMinusOne, nocomma).

writeApostrophe(H, []) :-
    !, H =.. [Functor | TermList],
    write(Functor), write('_ab('), writeTerms(TermList), write('[]).'), nl.
writeApostrophe(H, BList) :-
    length(BList, M),
    H =.. [Functor | TermList],
    write(Functor), write('_ab('), writeTerms(TermList), write('E_'), write(M), write(') :- '),
    writeApostropheBody(BList, 0), nl.

writeApostropheBody([], _) :- write('.').
writeApostropheBody([Pred | PredList], Num) :-
    NumPlusOne is Num + 1,
    (Num = 0, !; write(', ')),
    (
        (
            Pred = (not Pred_), !,
            Pred_ =.. [Functor | TermList],
            atom_concat('not_', Functor, NotFunctor),
            NotPred =.. [NotFunctor | TermList],
            writeEi(NotPred, Num)
        );(
            writeEi(Pred, Num)
        )
    ),
    writeApostropheBody(PredList, NumPlusOne).

writePlus(Pred/Arity) :- 
    write(Pred), write('('), writeX(Arity), write('I, O) :- '),
    write(Pred), write('_ab('), writeX(Arity), write('E), produce_context(O, I, E).'), nl.

writeMinus(Pred/Arity, 0) :-
    !, write('not_'), write(Pred), write('('), writeX(Arity), write('I, I).'), nl.

writeMinus(Pred/Arity, M) :-
    write('not_'), write(Pred), write('('), writeX(Arity), write('E_0, E_'), write(M), write(') :- '),
    writeMinusBody(Pred/Arity, M, 0), nl.

writeMinusBody(_, M, M) :- !, write('.').
writeMinusBody(Pred/Arity, M, Num) :- 
    NumPlusOne is Num + 1,
    (Num = 0, !; write(', ')),
    write(Pred), write('_star'), write(NumPlusOne), write('('), writeX(Arity),
    write('E_'), write(Num), write(', E_'), write(NumPlusOne), write(')'),
    writeMinusBody(Pred/Arity, M, NumPlusOne).

writeStar(RuleList) :- writeStar(RuleList, 0).
writeStar([], _).
writeStar([[H | B] | RuleList], Num) :-
    NumPlusOne is Num + 1,
    writeStarRules(H, NumPlusOne, B),
    writeStar(RuleList, NumPlusOne).

writeStarRules(_, _, []).
writeStarRules(H, Num, [not T | TermList]) :-
    !,
    H =.. [HFunctor | HTermList],
    write(HFunctor), write('_star'), write(Num), write('('), writeTerms(HTermList), write('I, O) :- '),
    T =.. [TFunctor | TTermList],
    write(TFunctor), write('('), writeTerms(TTermList), write('I, O).'), nl,
    writeStarRules(H, Num, TermList).
writeStarRules(H, Num, [T | TermList]) :-
    H =.. [HFunctor | HTermList],
    write(HFunctor), write('_star'), write(Num), write('('), writeTerms(HTermList), write('I, O) :- '),
    T =.. [TFunctor | TTermList],
    write('not_'), write(TFunctor), write('('), writeTerms(TTermList), write('I, O).'), nl,
    writeStarRules(H, Num, TermList).

writeO(Pred/Arity) :- 
    write(Pred), write('('), writeX(Arity), write('I, O) :- '),
    write('insert_abducible('), write(Pred), writeX(Arity, nocomma), write(', I, O).'), nl.
writeNotO(Pred/Arity) :-
    write('not_'), write(Pred), write('('), writeX(Arity), write('I, O) :- '),
    write('insert_abducible('), write('not '), write(Pred), writeX(Arity, nocomma), write(', I, O).'), nl.

%% Misc Helpers %%

tupleToList((T, TTuple), [T | TList]) :- tupleToList(TTuple, TList), !.
tupleToList(T, [T]).

getRule(Pred, [H | B]) :-
    isRule(H, B),
    H =.. [Pred | _].
