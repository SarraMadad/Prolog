# Prolog

TD1 - Cousin Relation :

=> file : td1.pl

Define a predicate cousin(Person1, Person2, Degree, Removal) where: 
  - Person1 and Person2 are person names,
  - Degree and Removal are integers

Following the definition from English Wikipedia: https://en.wikipedia.org/wiki/Cousin 

Test it on the example family from Wikipedia

Do not use imperative arithmetic predicates (is, >, <, =:= etc.) nor imperative KB update predicates (assert, retract, etc.)

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

TD2 - Balance Chemical Equations :

=> files : try.pl + test_unitaire_1.pl + test_unitaire_2.pl

Our goal is to determine the coefficients when given an equation. 

Your main objective:

Define a predicate balance/2 that balances a given equation. 
The first argument defines the left side of the equation. The second argument defines the right side of the equation.
You must use library(clpfd), library(pairs) and library(lists) are highly recommended as are predicates like keysort/2.


-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

TD3 - Wumpus Word :

=> files : agent.pl + env.pl

3 modules :
  - Sim: top-level module of whole WW game
  - Env: WW cavern simulator
  - Agent: WW gold hunter agent
