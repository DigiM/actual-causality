:- module(actualCauseBuilder, [processInputAC/2]).

:- use_module(operators).
:- use_module(actualCauseModel).

:- dynamic flagClausalTheory/0.

processInputAC(Fin, Fout) :-
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

processLine(beginClausalTheory) :-
    assert(flagClausalTheory).

processLine(endClausalTheory) :-
    retract(flagClausalTheory).

processLine(world(LitList)) :-
    markActualWorld(LitList).

processLine(exovars(VarList)) :-
    markExogenousVariables(VarList).

processLine(_) :-
    \+ flagClausalTheory.

processLine(_ => not _).

processLine(B => H) :-
    markClausalRule(H, B).

postProcess :-
    writeAbducibles,
    writeActualWorld,
    writeLogicProgram.

%-------------------------------------%
% Write Helpers
%-------------------------------------%

writeAbducibles :-
    findall(Var, isExogenousVariable(Var), VarList),
    write('abds(['),
    writeAbducibles_(VarList),
    write(']).'), nl, nl.

writeAbducibles_([]).
writeAbducibles_([Var]) :-
    write(Var), write('/0').

writeAbducibles_([Var | VarList]) :-
    write(Var), write('/0, '),
    writeAbducibles_(VarList).

writeActualWorld :-
    write(':- dynamic isActualWorld/1.'), nl,
    writeActualWorld_, nl,
    write('markActualWorld.'), nl, nl.

writeActualWorld_ :-
    isActualWorld(L),
    write('markActualWorld :- '),
    write('\\+ isActualWorld('), write(L), write('), '),
    write('assert(isActualWorld('), write(L), write(')), fail.'), nl,
    fail.
writeActualWorld_.

writeLogicProgram :-
    write('beginProlog.'), nl,
    writeRules,
    write('endProlog.'), nl, nl.

writeRules :-
    isClausalRule(H, B),
    write('    '), write(H), write(' <- '), write(B), write('.'), nl,
    fail.
writeRules.
