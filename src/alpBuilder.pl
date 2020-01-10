:- module(alpBuilder, [processInputALP/2]).

:- use_module(operators).
:- use_module(utils).
:- use_module(transformer).
:- use_module(alpModel).

:- dynamic isNonAbductive/1.
:- dynamic flagAbductive/0.

processInputALP(Fin, Fout) :-
    processInput(Fin, Fout).

processInput(Fin, Fout) :-
    see(Fin),
    tell(Fout),
    processLines,
    seen,
    told.

processLines :-
    read(Line),
    (
        (Line = end_of_file, !, postProcess);
        (processLine(Line), processLines)
    ).

processLine(beginProlog) :-
    assert(flagAbductive).

processLine(endProlog) :-
    retract(flagAbductive).

processLine(abds(AbdList)) :-
    markAbducibles(AbdList).

processLine(Line) :-
    \+ flagAbductive, !,
    (isNonAbductive(Line); assert(isNonAbductive(Line))).

processLine(H <- B) :-
    tupleToList(B, BList),
    markRule(H, BList),
    markHeadPredicate(H),
    markPredicate(H),
    markPredicates(BList).

processLine(H) :-
    markRule(H, []),
    markHeadPredicate(H),
    markPredicate(H).

postProcess :-
    writeNonAbductives,
    apostropheTransformation,
    plusTransformation,
    minusAndStarTransformation,
    oTransformation.

%----------------------------------------
% Write Helpers
%----------------------------------------

writeNonAbductives :-
    writeNonAbductives_.
    
writeNonAbductives_ :-
    isNonAbductive(Line),
    write(Line), write('.'), nl,
    fail.
writeNonAbductives_.
