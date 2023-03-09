/*
:- module(env, [
    initEnv(Width,Height,PitNum,WumpusNum,ArrowNum,State),
    doable(State,Action), %determines whether Action is directly executable in State
    do(State,Action,NextState),  %env deduces new State by applying effects of Action
    sense(State,Percepts) %env deduces agent's perccepts from current State
    ]). 
*/
    
:- use_module(library(clpfd)).
:- use_module(library(random)).
:- use_module(library(dicts)).
:- use_module(reif).

%Initialise l'environnement de départ du Wumpus World
%Il faut que PitNum + WumpusNum + 1 (le gold) + 1 (l'entrée) <= Width * Height
initEnv(Width, Height, PitNum, WumpusNum, ArrowNum, State):-
    genWalls(Width, Height, Walls),
    genPits(Width, Height, PitNum, Pits),
    genWumpuses(Width, Height, Pits, WumpusNum, Wumpuses),
    
    append([sq{x:1, y:1}], Wumpuses, Tmp),
    append(Tmp, Pits, PointsDejaPris),
    createNewPoint(PointsDejaPris, Width, Height, PointGold),
    Xg #= PointGold.x, Yg #= PointGold.y,

    State = state{
        certainEternals: ce{
            exit: sq{x:1,y:1}, pits: Pits, width: Width, height: Height, walls: Walls, wumpuses: Wumpuses
        },
        certainFluents:cf{
            step: 0, agentAt: sq{x:1, y:1}, agentDir: east, goldAt: sq{x:Xg, y:Yg},
            alive: a{
                wumpuses: WumpusNum, agent: 1
            },
            agentHas: h{
                arrow: ArrowNum, gold: 0
            },
            score: 0
        }
    }.

%Génére la liste des murs (verticaux + horizontaux) sous la forme d'une liste
genWalls(Width, Height, Walls):-
    GoodHeight #= Height+1, genWallsHorizontaux(Width, GoodHeight, MursHaut),
    GoodWidth #= Width + 1, genWallsVerticaux(GoodWidth, Height, MursDroite), append(MursHaut, MursDroite, Tmp1),
    genWallsHorizontaux(Width, 0, MursBas), append(Tmp1, MursBas, Tmp2),
    genWallsVerticaux(0, Height, MursGauche), append(Tmp2, MursGauche, Walls).  

%Génére la liste des pits sous la forme d'une liste de longueur PitNum
genPits(_, _, 0, []).
genPits(Width, Height, PitNum, Pits):-
    PitNum #>=1,
    NewPitNum #= PitNum -1, genPits(Width, Height, NewPitNum, PitsPrec),
    append(PitsPrec, [sq{x:1, y:1}], ListPointToAvoid),
    createNewPoint(ListPointToAvoid, Width, Height, NewPit),
    append(PitsPrec, [NewPit], Pits).

%Génére la liste des Wumpus sous la forme d'une liste de longueur WumpusNum
%Les Wumpus ne peuvent pas être créer dans des Pits (choix de notre part)
genWumpuses(_, _, _, 0, []).
genWumpuses(Width, Height, Pits, WumpusNum, Wumpuses):-
    WumpusNum#>=1,
    NewWumpusNum #= WumpusNum -1, genWumpuses(Width, Height, Pits, NewWumpusNum, WumpusesPrec),
    append(Pits, [sq{x:1, y:1}], ListPointToAvoid1),
    append(ListPointToAvoid1, WumpusesPrec, ListPointToAvoid),
    createNewPoint(ListPointToAvoid, Width, Height, NewWumpus),
    append(WumpusesPrec, [NewWumpus], Wumpuses). 

%Renvoit les perceptions par rapport à un état
sense(State,Percepts) :- 
    Percepts = ps{breeze: Br, bump: Bu, glitter: G, scream: Sc, stench: St},
    [Br,Bu,G,Sc,St] ins 0..1,
    showStatePercepts(State,Percepts).

%Fait l'action demandé et renvoie le nouvel état lié à l'action
%Liste des actions : [forward, left, right, climb, grab, shoot]
do(State, Action, NextState) :- 
    CertainFluents = State.certainFluents, CertainEternals = State.certainEternals,
    Width = CertainEternals.width, Height = CertainEternals.height,
    [AgentAtX, NextAgentAtX] ins 0..sup, [AgentAtY, NextAgentAtY] ins 0..sup,
    Step in 0..sup, HasGold in 0..1, HasArrow in 0..sup, WumpusesAlive in 0..sup,
    Step #= CertainFluents.step + 1, HasGold #= CertainFluents.agentHas.gold, HasArrow #= CertainFluents.agentHas.arrow - 1, WumpusesAlive #= CertainFluents.alive.wumpuses -1,
    AgentDir = CertainFluents.agentDir, 
    AgentAtX = CertainFluents.agentAt.x, AgentAtY = CertainFluents.agentAt.y, AgentAt = CertainFluents.agentAt, 
    GoldAt = CertainFluents.goldAt, ExitAt = CertainEternals.exit, WumpusesAt = CertainEternals.wumpuses,

    (
        Action = forward -> (
            AgentDir = east -> (
                NextAgentAtX #= AgentAtX + 1, NextAgentAtY #= AgentAtY
            );
            AgentDir = west -> (
                NextAgentAtX #= AgentAtX - 1, NextAgentAtY #= AgentAtY
            );
            AgentDir = north -> (
                NextAgentAtX #= AgentAtX, NextAgentAtY #= AgentAtY + 1
            );
            AgentDir = south -> (
                NextAgentAtX #= AgentAtX, NextAgentAtY #= AgentAtY - 1
            )
        ),
        NextState = State.put(certainFluents/step, Step)
        .put(certainFluents/agentAt/x, NextAgentAtX)
        .put(certainFluents/agentAt/y, NextAgentAtY);

        Action = left -> (
            AgentDir = east -> (
                NextAgentDir = north
            );
            AgentDir = west -> (
                NextAgentDir = south
            );
            AgentDir = north -> (
                NextAgentDir = west
            );
            AgentDir = south -> (
                NextAgentDir = east
            )
        ),
        NextState = State.put(certainFluents/step, Step)
        .put(certainFluents/agentDir, NextAgentDir);

        Action = right -> (
            AgentDir = east -> (
                NextAgentDir = south
            );
            AgentDir = west -> (
                NextAgentDir = north
            );
            AgentDir = north -> (
                NextAgentDir = east
            );
            AgentDir = south -> (
                NextAgentDir = west
            )
        ),
        NextState = State.put(certainFluents/step, Step)
        .put(certainFluents/agentDir, NextAgentDir);

        Action = climb -> (
            ExitAt = AgentAt -> writeln("L'agent est sorti de la caverne"); 
            writeln("L'agent ne peut pas sortir de la caverne")            
        ),
        NextState = State.put(certainFluents/step, Step);

        Action = grab -> (
            GoldAt = AgentAt -> NextGrabGold #= 1, writeln("L'agent a recupere l'or"); 
            NextGrabGold #= 0, writeln("L'agent ne peut pas recuperer l'or")            
        ),
        NextState = State.put(certainFluents/step, Step)
        .put(HasGold, NextGrabGold);

        Action = shoot -> (
            shootArrow(Width, Height, AgentDir, AgentAtX, AgentAtY, Target),
            arrowHit(Target, WumpusesAt, Hit),
            (
                Hit = [] -> (writeln("La fleche n'a rien touche"), NextState = State.put(certainFluents/step, Step).put(certainFluents/agentHas/arrow, HasArrow)); 
                (writeln("La fleche a atteint un Wumpus"), 
                NextState = State.put(certainFluents/step, Step).put(certainFluents/alive/wumpuses, WumpusesAlive).put(certainFluents/agentHas/arrow, HasArrow))   
            )
        )
    ).

%Vérifie si l'agent peut faire l'action demander selon l'état ou il est
doable(State, Action) :-
    CertainFluents = State.certainFluents, CertainEternals = State.certainEternals,
    [AgentAtX, NextAgentAtX] ins 0..sup, [AgentAtY, NextAgentAtY] ins 0..sup,
    AgentDir = CertainFluents.agentDir, 
    AgentAtX = CertainFluents.agentAt.x, AgentAtY = CertainFluents.agentAt.y, AgentAt = CertainFluents.agentAt, 
    GoldAt = CertainFluents.goldAt, ExitAt = CertainEternals.exit, WallsAt = CertainEternals.walls,
    HasArrow in 0..sup, HasArrow #= CertainFluents.agentHas.arrow,
    (
        Action = forward -> (
            AgentDir = east -> (
                NextAgentAtX #= AgentAtX + 1, NextAgentAtY #= AgentAtY, NewPoint = sq{x : NextAgentAtX, y : NextAgentAtY},
                memberList(NewPoint, WallsAt)->writeln("L'agent ne peut pas avance dans cette direction a cause d'un mur"), false; true
            );
            AgentDir = west -> (
                NextAgentAtX #= AgentAtX - 1, NextAgentAtY #= AgentAtY, NewPoint = sq{x : NextAgentAtX, y : NextAgentAtY},
                memberList(NewPoint, WallsAt)->writeln("L'agent ne peut pas avance dans cette direction a cause d'un mur"), false; true
            );
            AgentDir = north -> (
                NextAgentAtX #= AgentAtX, NextAgentAtY #= AgentAtY + 1, NewPoint = sq{x : NextAgentAtX, y : NextAgentAtY},
                memberList(NewPoint, WallsAt)->writeln("L'agent ne peut pas avance dans cette direction a cause d'un mur"), false; true
            );
            AgentDir = south -> (
                NextAgentAtX #= AgentAtX, NextAgentAtY #= AgentAtY - 1, NewPoint = sq{x : NextAgentAtX, y : NextAgentAtY},
                memberList(NewPoint, WallsAt)->writeln("L'agent ne peut pas avance dans cette direction a cause d'un mur"), false; true
            )
        );
        %Se tourner n'implique aucune vérification car on ne change pas de case
        Action = left; 
        Action = right;

        Action = climb -> (
            ExitAt = AgentAt -> true; writeln("L'agent n'est pas à la sortie"), false            
        );

        Action = grab -> (
            GoldAt = AgentAt -> true; writeln("L'agent n'est pas la ou l'or est"), false            
        );

        Action = shoot -> (
            HasArrow #> 0 -> true; writeln("L'agent n'a plu de fleche"), false            
        )
    ).

%Génére la liste des murs horizontaux sous la forme d'une liste
genWallsHorizontaux(1, Height, [sq{x:1, y:Height}]).
genWallsHorizontaux(Width, Height, Walls):-
    Width#>=2,
    A = sq{x:Width, y:Height},
    NewWidth#=Width-1,
    genWallsHorizontaux(NewWidth, Height, WallsPrec),
    append(WallsPrec, [A], Walls).

%Génére la liste des murs verticaux sous la forme d'une liste
genWallsVerticaux(Width, 1, [sq{x:Width, y:1}]).
genWallsVerticaux(Width, Height, Walls):-
    Height#>=2,
    A = sq{x:Width, y:Height},
    NewHeight#=Height-1,
    genWallsVerticaux(Width, NewHeight, WallsPrec),
    append(WallsPrec, [A], Walls).
    
%Renvoie False si un élément est dans une liste, True sinon
nonMember(_,[]).
nonMember(Arg,[Arg|_]) :-
        !, fail.
nonMember(Arg,[_|Tail]) :-
        !, nonMember(Arg,Tail).

%Génére un nouveau point qui n'est pas présent dans ListePointAEviter, avec x<Width, y<Height
createNewPoint(ListePointaEviter, Width, Height, NouveauPoint):-
    GoodWidth #= Width +1, GoodHeight #= Height +1,
    random(1, GoodWidth, RandomWidth), random(1, GoodHeight, RandomHeight),
    PointATester = sq{x:RandomWidth, y:RandomHeight},
    (
        nonMember(PointATester, ListePointaEviter) -> NouveauPoint = PointATester;
        createNewPoint(ListePointaEviter, Width, Height, NouveauPoint)
    ).

%Génére les 4 voisins de Point dans une liste
pointVoisins(Point, Voisins):-
    CoordX #= Point.x, CoordY #= Point.y,
    CoordVoisinHaut #= CoordY +1, CoordVoisinDroite #= CoordX +1, CoordVoisinBas #= CoordY -1, CoordVoisinGauche #= CoordY -1,
    Voisins = [sq{x:CoordX, y:CoordVoisinHaut}, sq{x:CoordVoisinDroite, y:CoordY},  sq{x:CoordX, y:CoordVoisinBas}, sq{x:CoordVoisinGauche, y:CoordY}].

%Génére le voisin du bas de Point dans une liste d'un seul élément
pointVoisinBas(Point, Voisins):-
    CoordX #= Point.x, CoordY #= Point.y,
    CoordVoisinBas #= CoordY -1, 
    Voisins = [sq{x:CoordX, y:CoordVoisinBas}].

%Renvoie True si un élément est dans une liste, False sinon
memberList(X, [X|_]).
memberList(X, [_|T]) :- 
    memberList(X, T).

%Renvoie True si les 2 listes ont un élément en commun, False sinon
listIntersection([], _) :- false.
listIntersection([H|T], L) :- 
    memberList(H, L) ; listIntersection(T, L).

%Renvoie les perceptions de l'agent selon sa localisation et l'état
showStatePercepts(State, Percepts):-
    LocationX = State.certainFluents.agentAt.x, LocationY = State.certainFluents.agentAt.y,
    LocationPoint = sq{x:LocationX, y:LocationY},
    pointVoisins(LocationPoint, LocationVoisin), pointVoisinBas(LocationPoint, LocationVoisinBas),
    (
        listIntersection(LocationVoisin, State.certainEternals.pits)->Percepts.breeze #= 1, writeln("Il y a un trou sur une case voisine"); 
        Percepts.breeze #= 0
        ),
    (
        listIntersection(LocationVoisin, State.certainEternals.walls)->Percepts.bump #= 1, writeln("Il y a un mur sur une case voisine"); 
        Percepts.bump #= 0
        ),
    (
        listIntersection(LocationVoisinBas, State.certainFluents.goldAt)->Percepts.glitter #= 1, writeln("L'or se trouve sur la case en dessous"); 
        Percepts.glitter #= 0
        ),
    (
        State.certainFluents.alive.wumpuses = 0 ->Percepts.scream #= 1, writeln("Tous les wumpus sont morts"); 
        Percepts.scream #= 0
        ),
    (
        listIntersection(LocationVoisin, State.certainEternals.wumpuses)->Percepts.stench #= 1, writeln("Il y a un wumpus sur une case voisine"); 
        Percepts.stench #= 0
        ).


%Renvoie la listes des cases touchés par la flèche selon l'emplacement et la direction de l'agent, sous form de liste
shootArrow(_, Height, north, AgentX, Height, Target):-
    GoodHeight #= Height + 1,
    append([], [sq{x:AgentX, y:GoodHeight}], Target).
shootArrow(_, _, south, AgentX, 1, Target):-
    append([], [sq{x:AgentX, y:0}], Target).
shootArrow(Width, _, east, Width, AgentY, Target):-
    GoodWidth #= Width + 1,
    append([], [sq{x:GoodWidth, y:AgentY}], Target).
shootArrow(_, _, west, 1, AgentY, Target):-
    append([], [sq{x:0, y:AgentY}], Target).
shootArrow(Width, Height, Direction, AgentX, AgentY, Target):-
    AgentX #=< Width, AgentX #>= 1, AgentY #=< Height, AgentY #>= 1,
    (
        Direction = north ->(
            NewAgentY #= AgentY + 1, NewTarget = sq{x:AgentX, y:NewAgentY},
            shootArrow(Width, Height, north, AgentX, NewAgentY, TargetPrec),
            append([NewTarget], TargetPrec, Target)
        );

        Direction = south ->(
            NewAgentY #= AgentY - 1, NewTarget = sq{x:AgentX, y:NewAgentY},
            shootArrow(Width, Height, south, AgentX, NewAgentY, TargetPrec),
            append([NewTarget], TargetPrec, Target)
        );

        Direction = east ->(
            NewAgentX #= AgentX + 1, NewTarget = sq{x:NewAgentX, y:AgentY},
            shootArrow(Width, Height, east, NewAgentX, AgentY, TargetPrec),
            append([NewTarget], TargetPrec, Target)
        );

        Direction = west ->(
            NewAgentX #= AgentX - 1, NewTarget = sq{x:NewAgentX, y:AgentY},
            shootArrow(Width, Height, west, NewAgentX, AgentY, TargetPrec),
            append([NewTarget], TargetPrec, Target)
        )
    ).

%Renvoie une liste vide si la flèche ne touche pas de Wumpus, ou renvoie l'emplacement du 1er Wumpus touché
arrowHit([], _, []).
arrowHit([Target|OtherTarget], Wumpuses, Hit):-
    (
        memberList(Target, Wumpuses) ->(
            Hit = Target
        );
    arrowHit(OtherTarget, Wumpuses, Hit)
    ).

%Supprime un élément d'une liste et renvoie cette liste, ou renvoie false si l'élément n'est pas présent dans la liste
delete(X, [X], []).
delete(X, [X|L], L).
delete(X, [Y|L], [Y|L1]):-
    delete(X, L, L1).