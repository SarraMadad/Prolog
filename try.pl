:- use_module(library(clpfd)).
:- use_module(library(pairs)).
:- use_module(library(lists)).

balance(Left, Right) :-
    %On extrait toutes les données importantes de notre équation
    extract_multiplicateur(Left, LeftMultiplicateur),%[1, 2]
    extract_multiplicateur(Right, RightMultiplicateur),%[2, 3]
    extract_molecule_name(Left, LeftMoleculeName),%['Al', 'O']
    extract_molecule_name(Right, RightMoleculeName),%['Al', 'O']
    extract_variables(Left, LeftVariables),%[A, B]
    extract_variables(Right, RightVariables),%[C, C]

    %On crée une liste de toutes les molécules présentes, dans l'ordre et une unique fois
    append(LeftMoleculeName, RightMoleculeName, Tmp_2), sort(Tmp_2, AllMolecule),
    %On crée notre liste de tous les coefficients dont on veut déterminer la valeur, on précise qe chaque coefficient est supérieur strictement à 1
    append(LeftVariables, RightVariables, Tmp_1), sort(Tmp_1, Variables),
    Variables ins 1..sup,

    %On crée nos listes de variables et de multiplicateurs pour chaque coté de l'équation
    %Ces listes prennent en compte les molécules auxquels ils sont affectés
    filtre(LeftVariables, LeftMoleculeName, AllMolecule, LeftVariablePerMolecule),
    filtre(RightVariables, RightMoleculeName, AllMolecule, RightVariablePerMolecule),
    filtre(LeftMultiplicateur, LeftMoleculeName, AllMolecule, LeftMultiplicateurPerMolecule),
    filtre(RightMultiplicateur, RightMoleculeName, AllMolecule, RightMultiplicateurPerMolecule),

    %On multiplie les listes précedemment crées afin de créer nos équations pour chaque molécule
    maplist(mult_list, LeftVariablePerMolecule, LeftMultiplicateurPerMolecule, LeftEquationPerMolecule),
    maplist(mult_list, RightVariablePerMolecule, RightMultiplicateurPerMolecule, RightEquationPerMolecule),

    %On somme nos listes pour créer nos 2 équations que l'on veut égaliser
    somme_for_equation(LeftEquationPerMolecule, EquationGaucheOK),
    somme_for_equation(RightEquationPerMolecule, EquationDroiteOK),

    %On égalise nos 2 équations terme à terme (l'ordre des molécules est toujours identique grâce à notre liste AllMolecule)
    %On egalise chaque terme car on veut que chaque molécule apparaissent le même nombre de fois des 2 côtés
    egalize_equation(EquationGaucheOK, EquationDroiteOK),

    %On a voulu sommer nos variables afin d'utiliser la fonction labeling, mais nous n'avons pas réussi cette partie
    somme_liste(Variables, Sum).
    %labeling([min(Sum)], Variables).



%Extrait les variables (coefficients que l'on doit déterminer) d'un cote ou l'autre de l'equation dans une liste
%Si la variable est utilisé pour N molécules (exemple : [A-['Al'-1, 'O'-2]]), la variable apparait N foiis dans la liste
extract_variables([], []).
extract_variables([Variable-AtomePerVariable|RestVariableList], VariableNameList) :-
    extract_variables(RestVariableList, RestVariable),
    longueur(AtomePerVariable, NbAtome),
    repeteElement(Variable, NbAtome, ListTmp),
    append(ListTmp, RestVariable, VariableNameList).

%Extrait les noms de molecule d'un cote ou l'autre de l'equation dans une liste
extract_molecule_name([], []).
extract_molecule_name([_-Molecule|RestMoleculeList], MoleculeNameList) :-
    extract_molecule_name(RestMoleculeList, RestMolecule),
    pairs_keys(Molecule, MoleculeName),
    append(MoleculeName, RestMolecule, MoleculeNameList).

%Extrait les multiplicateurs des molecules d'un cote ou l'autre de l'equation dans une liste
extract_multiplicateur([], []).
extract_multiplicateur([_-Multiplicateur|RestMultiplicateurList], MultiplicateurList) :-
    extract_multiplicateur(RestMultiplicateurList, RestMultiplicateur),
    pairs_values(Multiplicateur, MultiplicateurValue),
    append(MultiplicateurValue, RestMultiplicateur, MultiplicateurList).

%Calcule la longueur d'une liste donnée
longueur([], 0).
longueur([_|Queue], N):-
    longueur(Queue, M), N#=M+1.

%Permet de repeter un element N fois dans une liste (utilisé lorsqu'une variable est utilisé pour plusieurs molécules)
repeteElement(_Elem, 0, []).
repeteElement(Element, N, [Element | List]):-
    N #> 0,
    M #= N - 1,
    repeteElement(Element, M, List).


%Filtre une liste par rapport aux molécules et selon les molécules de l'équation.
%Par exemple pour A Al + B O2, on a [A, B] en variables, [1, 2] en coefficients, ['Al', 'O'] en liste de molecule
%On veur separer nos listes [A, B] et [1, 2] selon molécules.
%Cette fonction permet de faire fitre_List([A, B], ['Al', 'O'], ['Al', 'O'], List) et on obtient Liste = [[A, 0], [0, 2]]
%Elle est utilisée pour séparer les variables et les coefficients en N listes, N étant le nombre de molécules en tout
filtre_List([], [], Mol, []).
filtre_List([Value1|OtherValue], [String1|OtherString], Mol, [Output|ListPrec]):-
    member(Molecule, Mol),
    (
        Molecule==String1->Output#=Value1; Output#=0
    ), 
    filtre_List(OtherValue, OtherString, Mol, ListPrec).
filtre(L1, L2, [],[]).
filtre(L1, L2, [Mol|T],Res):-
    filtre(L1, L2, T, Res1),
    filtre_List(L1, L2, [Mol], Eq),
    append([Eq], Res1, Res).


mult(X, Y, R) :-
    R #= X * Y.
%Permet de multiplier 2 listes entre elles
mult_list([], [], []).
mult_list([L1|Rest1], [L2|Rest2], Answer):-
    mult_list(Rest1, Rest2,Answer1),
    mult(L1, L2, Ans),
    append([Ans], Answer1, Answer).

%Permet de faire apparaître les conditions d'équilibre de l'équation chimique, 
%il faut que chaque molécule apparaisse autant de chaque coôté de l'équation
egalize_equation([], []).
egalize_equation([L1|Rest1], [L2|Rest2]):-
    egalize_equation(Rest1, Rest2),
    L1#=L2.

%Permet de faire la somme de 2 listes
somme_liste([], 0).
somme_liste([T|Q], S) :- 
    somme_liste(Q, S1), S #= T + S1.

%Permet de faire la somme de 2 "listes de listes"
%Par exemple somme_for_equation([[1, 0, 2], [0, 3, 1]]) renvoie [3, 4]
%On utilise cette fonction afin de calculer combien de fois apparaît une molécule d'un cote de l'equation
somme_for_equation([], []).
somme_for_equation([List|Rest], Ans) :-
    somme_for_equation(Rest, SumEqu),
    somme_liste(List, SumList),
    append([SumList], SumEqu, Ans).