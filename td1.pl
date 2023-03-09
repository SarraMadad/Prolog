%nous nous sommes aidés de ce lien : https://stackoverflow.com/questions/39689283/calculating-cousin-relationship-in-prolog

:- use_module(library(clpfd)).
:- use_module(library(dif)).
:- use_module(library(solution_sequences)).

mother(edwina, marie). 
father(joseph, marie).
mother(edwina, charles).
father(joseph, charles).
mother(marie, david). 
father(charles, emma).
father(david, frank).
mother(emma, george).
father(frank, harry).
father(george, ingeborg).

parent(M, C1) :- mother(M, C1).  
parent(F, C2) :- father(F, C2).
sibling(S1,S2) :- parent(P1, S1), parent(P1, S2), parent(P2, S1), parent(P2, S2) , not(S1=S2), P1 @< P2.
anc(A1, C3) :- parent(A1, C3).
anc(A2, C4) :- parent(P3, C4), anc(A2, P3).

cousin(V1, V2, 1, 0) :-  %1er cousin direct (R=0): ils ont des parents qui ont les mm parents
    parent(W1, V1), parent(W2, V2), sibling(W1, W2).

cousin(V1, V2, D, 0) :- %2eme et 3eme cousin direct (R=0) : ils ont des parents qui sont 1er ou 2eme cousins
    D#>= 2,
    diff_gen_old_com_anc(V1, V2, R, R),
    parent(W1, V1), parent(W2, V2), 
    M #= D-1, cousin(W1, W2, M, 0).

cousin(V1, V2, D, R) :-
    D#>=1, D#=<3,
    R#>=1, R#=<2, R\=0,
    dif(V1, V2),
    diff_gen_old_com_anc(V1, V2, R1, R2), %si on ne le met pas, on se retrouve avec D et R différents pour un mm couple 
    R#=abs(R2-R1), S#=R-1,
    (
        R1#=R2, cousin(V1, V2, D, 0);
        R1#>R2, parent(W1, V1), cousin(W1, V2, D, S);
        R2#>R1, parent(W2, V2), cousin(V1, W2, D, S)
    ).

%on récupère la diff de génération entre 2 personnes et leur plus vieil ancêtre commun
diff_gen_old_com_anc(V1, V2, R1, R2) :-
    diff_gen_old_anc(V1, R1, A), 
    diff_gen_old_anc(V2, R2, A).

%on récupère la diff de génération entre 1 personne et son plus vieil ancêtre
diff_gen_old_anc(E, 0, E) :-
    \+ parent(_, E).
diff_gen_old_anc(E, N, A) :-
    N #> 0,
    parent(P, E),
    M #= N - 1,
    diff_gen_old_anc(P, M, A).
