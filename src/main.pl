:- [transformer].

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
        (markForTransformation(Line), processLines)
    ).

postProcess :-
    write(':- [utils].'), nl,
    transformAll.
