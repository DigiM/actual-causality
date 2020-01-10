:-dynamic isActualWorld/1.
markActualWorld:- \+isActualWorld(p),assert(isActualWorld(p)),fail.
markActualWorld:- \+isActualWorld(b),assert(isActualWorld(b)),fail.
markActualWorld:- \+isActualWorld(t),assert(isActualWorld(t)),fail.
markActualWorld.
h_ab(E_2) :- p([], E_1), t(E_1, E_2).
h_ab(E_2) :- not_p([], E_1), b(E_1, E_2).
d_ab(E_1) :- h([], E_1).
h(I, O) :- h_ab(E), produce_context(O, I, E).
d(I, O) :- d_ab(E), produce_context(O, I, E).
not_h(E_0, E_2) :- h_star1(E_0, E_1), h_star2(E_1, E_2).
h_star1(I, O) :- not_p(I, O).
h_star1(I, O) :- not_t(I, O).
h_star2(I, O) :- p(I, O).
h_star2(I, O) :- not_b(I, O).
not_d(E_0, E_1) :- d_star1(E_0, E_1).
d_star1(I, O) :- not_h(I, O).
p(I, O) :- insert_abducible(p, I, O).
not_p(I, O) :- insert_abducible(not p, I, O).
b(I, O) :- insert_abducible(b, I, O).
not_b(I, O) :- insert_abducible(not b, I, O).
t(I, O) :- insert_abducible(t, I, O).
not_t(I, O) :- insert_abducible(not t, I, O).
