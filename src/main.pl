:- use_module(operators).

:- use_module(actualCauseBuilder).
:- use_module(alpBuilder).

:- use_module(actualCauseRunner).

build(Filename) :-
    atom_concat(Filename, '.in', Fin),
    atom_concat(Filename, '.in.alp', FinALP),
    atom_concat(Filename, '.pl', Fout),
    processInputAC(Fin, FinALP),
    processInputALP(FinALP, Fout).

findActualCause(Filename, L, ActualCauseList) :-
    setUp(Filename),
    findActualCause(L, ActualCauseList).

?- build(test).
