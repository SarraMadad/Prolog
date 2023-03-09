:- begin_tests(balance).
consult('try.pl').
test(valid_input) :-
    balance([A-['C'-4,'H'-10],B-['O'-2]], [C-['C'-1,'O'-2],D-['H'-2,'O'-1]]).
    labeling([], [A,B,C]),
    assertion(A == 2),
    assertion(B == 13),
    assertion(C == 8),
    assertion(D == 10).

:- end_tests(balance).