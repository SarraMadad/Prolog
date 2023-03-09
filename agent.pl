/*
:- module(agent, [
    initAgent(Width,Height,PitNum,WumpusNum,ArrowNum,Beliefs,Prefs),
    reason(
        Percepts,Plan,
        Beliefs,NextBeliefs,
        Prefs,NextPrefs,
        ActionList,NextActionList
        ),
    doable(Beliefs,Actions),
    predict(Beliefs,Action,Beliefs) 
    ]).
*/

:- use_module(library(clpfd)).
:- use_module(library(dicts)).
:- use_module(library(lists)).
:- use_module(reif).

%Initialise l'agent de départ du Wumpus World
%Il faut que PitNum + WumpusNum + 1 (le gold) + 1 (l'entrée) <= Width * Height
initAgent(Width, Height, PitNum, WumpusNum, ArrowNum, Beliefs, Prefs) :-
    genWalls(Width, Height, Walls),
    
    Beliefs = bel{
        certainEternals: ce{
            exit: sq{x:1,y:1},
            width: Width, height: Height,
            walls: Walls
        },
        uncertainEternals: ues{
            0: ue{
                orTrue: ot{
                    pits: [], % List of squares where there is suspected pit given the history of breeze percepts
                    wumpuses: [] % List of squares where there is suspected wumpus given the history of strench percepts
                },
                knownTrue: kt{
                    pits: [], % List of squares with confirmed pits
                    wumpuses: [] % List of squares with confirmed wumpus
                },
                knownFalse: kf{
                    pits: [sq{x:1,y:1}], % List of squares with confirmed absence of pits
                    wumpuses: [sq{x:1,y:1}] % List of squares with confirmed absence of a wumpus
                }
            }
        },
        certainFluents: cfs{
            0: cf{
                agentAt: sq{x:1,y:1},
                agentDir: east,
                visited: [0-sq{x:1,y:1}],
                goldAt: sq{x:0,y:0},
                alive: a{
                    wumpus: WumpusNum,
                    agent: 1
                },
                agentHas: h{
                    arrow: ArrowNum,
                    gold: 0
                },
                score: 0
            }
        },
        uncertainFluents: ufs{
            0: uf{
                orTrue: ot{unsafe: []}, % List of suspected unsafe squares where there could be either a pit or a live wumpus given the history of breeze, scream and stench percepts
                knownTrue: kt{safe: [sq{x:1,y:1}]}, %list of confirmed safe squares
                knownFalse: kt{safe: [sq{x:1,y:1}]}% List of known safe squares
            }
        }
    },

    Prefs = pref{
        foundGold: 0,
        hasGold: 0,
        backToExit: 0,
        hasExited: 0
    }.

%Percepts = ps{breeze: Br, bump: Bu, glitter: G, scream: Sc, stench: St}
%perceive permet de mettre à jour belief selon les perceptions ressentis par l'agent
perceive(Beliefs, Percepts, NextBeliefs) :-
    CertainFluents = Beliefs.certainFluents, UncertainEternals = Beliefs.uncertainEternals,
    LocationX = CertainFluents.0.agentAt.x, LocationY = CertainFluents.0.agentAt.y, LocationPoint = sq{x:LocationX, y:LocationY},
    PitsMaybe = UncertainEternals.0.orTrue.pits,  NotPits = UncertainEternals.0.knownFalse.pits,
    WumpusesMaybe = UncertainEternals.0.orTrue.wumpuses,  NotWumpuses = UncertainEternals.0.knownFalse.wumpuses,
    pointVoisins(LocationPoint, Voisins), pointVoisinBas(LocationPoint, VoisinBas),

    (
        Percepts.breeze = 1 -> append(PitsMaybe, Voisins, NewPitsMaybe), BeliefTMP1 = Beliefs.put(uncertainEternals/0/orTrue/pits, NewPitsMaybe);
        append(NotPits, Voisins, NewNotPits), BeliefTMP1 = Beliefs.put(uncertainEternals/0/knownFalse/pits, NewNotPits)
    ),
    (
        Percepts.stench = 1 -> append(WumpusesMaybe, Voisins, NewWumpusesMaybe), BeliefTMP2 = BeliefTMP1.put(uncertainEternals/0/orTrue/wumpuses, NewWumpusesMaybe);
        append(NotWumpuses, Voisins, NewNotWumpuses), BeliefTMP2 = BeliefTMP1.put(uncertainEternals/0/knownFalse/wumpuses, NewNotWumpuses)
    ),
    (
        Percepts.glitter = 1 -> BeliefTMP3 = BeliefTMP2.put(certainFluents/0/goldAt, VoisinBas); BeliefTMP3 = BeliefTMP2
    ),
    (
        Percepts.scream = 1 -> BeliefTMP4 = BeliefTMP3.put(certainFluents/0/alive/wumpus, 0);
        BeliefTMP4 = BeliefTMP3      
    ),
    (
        Percepts.bump = 1 -> NextBeliefs = BeliefTMP4; %On ne sait pas ce que la perception d'un mur change car on suppose que l'agent connait la taille de la caverne et donc ou sont les murs
        NextBeliefs = BeliefTMP4
    ).

%Prefs = pref{foundGold: 1, hasGold: 0, backToExit: 0, hasExited: 0}
%prefsUpdate permet de mettre à jour Prefs en fonction de Beliefs
prefsUpdate(NextBeliefs,Prefs,NewPrefs) :-
    CertainFluents = NextBeliefs.certainFluents,
    LocationX = CertainFluents.0.agentAt.x, LocationY = CertainFluents.0.agentAt.y,
    GoldAtX = CertainFluents.0.goldAt.x, GoldAtY = CertainFluents.0.goldAt.y, HasGold = CertainFluents.0.agentHas.gold,
    (
        GoldAtX = 0, GoldAtY = 0 -> PrefsTMP1 = Prefs.put(foundGold, 0); PrefsTMP1 = Prefs.put(foundGold, 1)
    ),
    (
        HasGold = 0 -> PrefsTMP2 = PrefsTMP1.put(hasGold, 0); PrefsTMP2 = PrefsTMP1.put(hasGold, 1)
    ),
    (
        LocationX = 1, LocationY = 1 -> PrefsTMP3 = PrefsTMP2.put(backToExit, 1); PrefsTMP3 = PrefsTMP2.put(backToExit, 0)
    ),
    (
        LocationX = 1, LocationY = 1, HasGold = 1 -> NewPrefs = PrefsTMP3.put(hasExited, 1); NewPrefs = PrefsTMP3.put(hasExited, 0)
    ).

%doable verifie si l'action demandée peut être faite selon l'etat de Beliefs
doable(Beliefs,Action) :- 
    CertainFluents = Beliefs.certainFluents, CertainEternals = Beliefs.certainEternals,
    UncertainFluents = Beliefs.uncertainFluents,
    [AgentAtX, NextAgentAtX] ins 0..sup, [AgentAtY, NextAgentAtY] ins 0..sup,
    AgentDir = CertainFluents.0.agentDir, 
    AgentAtX = CertainFluents.0.agentAt.x, AgentAtY = CertainFluents.0.agentAt.y, AgentAt = CertainFluents.0.agentAt, 
    GoldAt = CertainFluents.0.goldAt, ExitAt = CertainEternals.exit, 
    SafeSquares = UncertainFluents.0.knownTrue.safe,
    HasArrow in 0..sup, HasArrow #= CertainFluents.0.agentHas.arrow,
    (
        Action = forward -> (
            AgentDir = east -> (
                NextAgentAtX #= AgentAtX + 1, NextAgentAtY #= AgentAtY, NewPoint = sq{x:NextAgentAtX, y:NextAgentAtY}, 
                memberList(NewPoint, SafeSquares)->true; writeln("L'agent ne peut pas avance dans cette direction car la case n'est pas safe"), false
            );
            AgentDir = west -> (
                NextAgentAtX #= AgentAtX - 1, NextAgentAtY #= AgentAtY, NewPoint = sq{x : NextAgentAtX, y : NextAgentAtY},
                memberList(NewPoint, SafeSquares)->true; writeln("L'agent ne peut pas avance dans cette direction car la case n'est pas safe"), false
            );
            AgentDir = north -> (
                NextAgentAtX #= AgentAtX, NextAgentAtY #= AgentAtY + 1, NewPoint = sq{x : NextAgentAtX, y : NextAgentAtY},
                memberList(NewPoint, SafeSquares)->true; writeln("L'agent ne peut pas avance dans cette direction car la case n'est pas safe"), false
                );
            AgentDir = south -> (
                NextAgentAtX #= AgentAtX, NextAgentAtY #= AgentAtY - 1, NewPoint = sq{x : NextAgentAtX, y : NextAgentAtY},
                memberList(NewPoint, SafeSquares)->true; writeln("L'agent ne peut pas avance dans cette direction car la case n'est pas safe"), false
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

%predict met à jour Beliefs selon l'action demandée
predict(Beliefs, Action, NextBeliefs) :-
    CertainFluents = Beliefs.certainFluents, CertainEternals = Beliefs.certainEternals,
    UncertainFluents = Beliefs.uncertainFluents,
    [AgentAtX, NextAgentAtX] ins 0..sup, [AgentAtY, NextAgentAtY] ins 0..sup,
    AgentDir = CertainFluents.0.agentDir, 
    AgentAtX = CertainFluents.0.agentAt.x, AgentAtY = CertainFluents.0.agentAt.y, AgentAt = CertainFluents.0.agentAt, 
    GoldAt = CertainFluents.0.goldAt, ExitAt = CertainEternals.exit, 
    HasArrow in 0..sup, HasArrow #= CertainFluents.0.agentHas.arrow,

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
        NextBeliefs = Beliefs.put(certainFluents/0/agentAt/x, NextAgentAtX).put(certainFluents/0/agentAt/y, NextAgentAtY)
        .put(certainFluents/0/visited/x, NextAgentAtX).put(certainFluents/0/visited/y, NextAgentAtY);

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
        NextBeliefs = Beliefs.put(certainFluents/0/agentDir, NextAgentDir);

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
        NextBeliefs = Beliefs.put(certainFluents/0/agentDir, NextAgentDir);

        Action = climb -> (
            ExitAt = AgentAt -> writeln("L'agent est sorti de la caverne"); 
            writeln("L'agent ne peut pas sortir de la caverne")            
        ),
        NextBeliefs = Beliefs;

        Action = grab -> (
            GoldAt = AgentAt -> NextGrabGold #= 1, writeln("L'agent a recupere l'or"); 
            NextGrabGold #= 0, writeln("L'agent ne peut pas recuperer l'or")            
        ),
        NextBeliefs = Beliefs.put(certainFluents/0/agentHas/gold, 1);

        Action = shoot -> (
            shootArrow(Width, Height, AgentDir, AgentAtX, AgentAtY, Target),
            arrowHit(Target, WumpusesAt, Hit),
            (
                Hit = [] -> (writeln("La fleche n'a rien touche"), NextBeliefs = Beliefs.put(certainFluents/0/agentHas/arrow, HasArrow)); 
                (writeln("La fleche a atteint un Wumpus"), 
                NextBeliefs = Beliefs.put(certainFluents/0/alive/wumpuses, WumpusesAlive))  
            )
        )
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

%Génére la liste des murs (verticaux + horizontaux) sous la forme d'une liste
genWalls(Width, Height, Walls):-
    GoodHeight #= Height+1, genWallsHorizontaux(Width, GoodHeight, MursHaut),
    GoodWidth #= Width + 1, genWallsVerticaux(GoodWidth, Height, MursDroite), append(MursHaut, MursDroite, Tmp1),
    genWallsHorizontaux(Width, 0, MursBas), append(Tmp1, MursBas, Tmp2),
    genWallsVerticaux(0, Height, MursGauche), append(Tmp2, MursGauche, Walls).

%Renvoie True si un élément est dans une liste, False sinon
memberList(X, [X|_]).
memberList(X, [_|T]) :- 
    memberList(X, T).

%Renvoie la liste des cases touchées par la flèche selon l'emplacement et la direction de l'agent, sous forme de liste
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