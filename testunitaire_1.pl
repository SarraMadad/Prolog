:- begin_tests(balance).
consult('try.pl').
test(valid_input) :-
    balance([A-['Al'-1], B-['O'-2]], [C-['Al'-2], C-[ 'O'-3]]),
    labeling([], [A,B,C]),
    assertion(A == 4),
    assertion(B == 3),
    assertion(C == 2).

:- end_tests(balance).