:- [transformer].

:- dynamic isNonAbductive/1.
:- dynamic isTransforming/0.

build(Filename) :-
    cleardatabase,
    atom_concat(Filename, '.in', Fin),
    atom_concat(Filename, '.pl', Fout),
    processInput(Fin, Fout).

cleardatabase :-
    retractall(isRule(_, _)),
    retractall(isHeadPredicate(_)),
    retractall(isPredicate(_)),
    retractall(isAbducible(_)).

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
    assert(isTransforming).
processLine(endProlog) :-
    retract(isTransforming).

processLine(abds(AbdList)) :-
    markForTransformation(abds(AbdList)).

processLine(Line) :-
    isTransforming,
    markForTransformation(Line).

processLine(Line) :-
    assert(isNonAbductive(Line)).

postProcess :-
    write(':- [utils].'), nl,
    writeNonAbductive,
    transformAll.

writeNonAbductive :-
    isNonAbductive(Rule),
    write(Rule), write('.'), nl,
    fail.
writeNonAbductive.
