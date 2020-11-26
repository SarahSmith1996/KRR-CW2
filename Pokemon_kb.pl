%% brandons_kb.pl
%% A Prolog knowledge base and inference system.
%% Brandon Bennett 
%% First version:    20.11.2011   
%% Current version:  20.11.2018
%% Minor correction: 17.11.2019

%% Preliminary Declarations
:- op( 950, xfx, ==>  ).  % Define a special operator to make the rules more readable.
:- dynamic fact/1.        % fact/1 predicate can be altered while program is running.

:- retractall( fact(_) ). % Clear previously stored facts.

%% DEFINE SOME FACTS
%% First some subclasses of pokemon.
fact( [normal, subclass_of, pokemon] ).
fact( [fighting, subclass_of, pokemon] ).
fact( [flying, subclass_of, pokemon] ). 
fact( [grass, subclass_of, pokemon]).  
fact( [fire, subclass_of, pokemon]). 
fact( [water, subclass_of, pokemon]). 
fact( [ground, subclass_of, pokemon]). 
fact( [electric, subclass_of, pokemon]). 
fact( [psychic, subclass_of, pokemon]).
fact( [poison, subclass_of, pokemon]).

%% Some types of pokemon which inherit from the pokemon subclasses above
fact( [pikachu, subclass_of, electric]).
fact( [bulbosaur, subclass_of, grass]).
fact( [mew, subclass_of, psychic]).
fact( [charmander, subclass_of, fire]).
fact( [pidgey, subclass_of, flying]).
fact( [butterfree, subclass_of, flying]).
fact( [jigglypuff, subclass_of, normal]).
fact( [snorlax, subclass_of, normal] ).
fact( [eevee, subclass_of, normal]).
fact( [oddish, subclass_of, grass]).
fact( [machop, subclass_of, fighting]).
fact( [squirtle, subclass_of, water]).
fact( [onix, subclass_of, ground]).
fact( [drapion, subclass_of, poison]).


%% Now some specific facts.
%% Here are some specific pokemon characters which have different types 
fact( [sian, is, bulbosaur] ).
fact( [emma, is, mew] ).
fact( [sarah, is, eevee] ).
fact( [alex, is, charmander] ).
fact( [jack, is, pidgey] ).
fact( [mary, is, butterfree] ).
fact( [wilson, is, jigglypuff] ).
fact( [sarahm, is, oddish] ).
fact( [zoe, is, machop] ).
fact( [jonny, is, squirtle] ).
fact( [shaz, is, pikachu] ).
fact( [david, is, drapion] ).
fact( [vania, is, onix] ).
fact( [owen, is, snorlax] ).

%%Ages for the pokemon characters
fact( [sian, is_age, 3] ).
fact( [emma, is_age, 60] ).
fact( [sarah, is_age, 18] ).
fact( [alex, is_age, 22] ).
fact( [jack, is_age, 41] ).
fact( [mary, is_age, 2] ).
fact( [wilson, is_age, 11] ).
fact( [sarahm, is_age, 8] ).
fact( [zoe, is_age, 37] ).
fact( [jonny, is_age, 14] ).
fact( [shaz, is_age, 52] ).
fact( [david, is_age, 5] ).
fact( [vania, is_age, 16] ).
fact( [owen, is_age, 17] ).

%% Some parents of specific pokemon
fact( [sarah, is_parent_of, sian] ).
fact( [zoe, is_parent_of, mary] ).
fact( [emma, is_parent_of, sarah] ).
fact( [alex, is_parent_of, wilson] ).

%%Who is attacking who
fact( [jack, attacks, jonny] ).
fact( [mary, attacks, sian]).
fact( [david, attacks, emma] ).
fact( [zoe, attacks, sarah] ).

%%Some pokemon characters have defence 
fact( [jonny, armed]).
fact( [wilson, armed]).


%% SPECIFY INFERENCE RULES
ruleset([_]). 

%% General logical and set-theoretic rules
rule( logic, [[C1, subclass_of, C2], [C2, subclass_of, C3]] ==> [C1, subclass_of, C3] ).
rule( logic, [[X, is, C1], [C1, subclass_of, C2]]  ==>  [X, is, C2] ).

%% Taxonomic relationships regarding the concept vocabulary
rule( taxonomy, [[Z, is_parent_of, Y], [Y, is_parent_of, X]] ==> [Z, is_grandparent_of, X]). 
rule( taxonomy, [[X, is_parent_of, Y]] ==> [Y, is_child_of, X]). 
rule( taxonomy, [[X, is, C1], [Y, is, C1], [C1, subclass_of, pokemon], [X, attacks, Y]] ==> [X, neutral_to, Y]). 

%% Other semantic relations holding among concepts. 
rule( semantic, [[X, stronger_than, Y], [X, attacks, Y]] ==> [Y, vulnerable_to, X]).
rule( semantic, [[X, weaker_than, Y], [X, attacks, Y]] ==> [Y, resistant_to, X]).
rule( semantic, [[X, resistant_to, Y]] ==> -[Y, beats, X]).
rule( semantic, [[X, neutral_to, Y]] ==> -[X, beats, Y]). 
rule( semantic, [[X, beats, Y]] ==> -[Y, beats, X]).
rule( semantic, [[X, attacks, Y]] ==> [Y, attacked_by, X]).

     
%%rule( semantic, [[X, is, tall]] ==> -[X, is, short] ).
%%rule( semantic,  [[X,likes,S], [Y,likes,S], test(different(X,Y))] 
%%      ==> [X, and, Y, have_common_interest] ).
     
%% Some domain specific inference rules:
        
%%Strength rules denote who is stronger than who when attacking
rule( strength, [[X, is, fighting], [Y, is, normal]] ==> [X, stronger_than, Y]).
rule( strength, [[X, is, normal], [Y, is, flying]] ==> [X, stronger_than, Y]).
rule( strength, [[X, is, flying], [Y, is, grass]] ==> [X, stronger_than, Y]).
rule( strength, [[X, is, poison], [Y, is, grass]] ==> [X, stronger_than, Y]).
rule( strength, [[X, is, ground], [Y, is, poison]] ==> [X, stronger_than, Y]).
rule( strength, [[X, is, ground], [Y, is, fire]] ==> [X, stronger_than, Y]).
rule( strength, [[X, is, ground], [Y, is, electric]] ==> [X, stronger_than, Y]).
rule( strength, [[X, is, fire], [Y, is, grass]] ==> [X, stronger_than, Y]).
rule( strength, [[X, is, electric], [Y, is, flying]] ==> [X, stronger_than, Y]).
rule( strength, [[X, is, electric], [Y, is, water]] ==> [X, stronger_than, Y]).
rule( strength, [[X, is, psychic], [Y, is, fighting]] ==> [X, stronger_than, Y]).
rule( strength, [[X, is, psychic], [Y, is, poison]] ==> [X, stronger_than, Y]).
rule( strength, [[X, is, grass], [Y, is, ground]] ==> [X, stronger_than, Y]).
rule( strength, [[X, is, grass], [Y, is, water]] ==> [X, stronger_than, Y]).
rule( strength, [[X, is, water], [Y, is, fire]] ==> [X, stronger_than, Y]).
rule( strength, [[X, is, water], [Y, is, ground]] ==> [X, stronger_than, Y]).

%% Weakness rules denote who is weaker than who when attacking - 
%% weakness is different and is not simply the inverse of the above set of rules
rule( weakness, [[X, is, fighting], [Y, is, flying]] ==> [X, weaker_than, Y]).
rule( weakness, [[X, is, fighting], [Y, is, poison]] ==> [X, weaker_than, Y]).
rule( weakness, [[X, is, fighting], [Y, is, psychic]] ==> [X, weaker_than, Y]).
rule( weakness, [[X, is, flying], [Y, is, electric]] ==> [X, weaker_than, Y]).
rule( weakness, [[X, is, poison], [Y, is, ground]] ==> [X, weaker_than, Y]).
rule( weakness, [[X, is, ground], [Y, is, flying]] ==> [X, weaker_than, Y]).
rule( weakness, [[X, is, ground], [Y, is, grass]] ==> [X, weaker_than, Y]).
rule( weakness, [[X, is, grass], [Y, is, flying]] ==> [X, weaker_than, Y]).
rule( weakness, [[X, is, grass], [Y, is, poison]] ==> [X, weaker_than, Y]).
rule( weakness, [[X, is, grass], [Y, is, fire]] ==> [X, weaker_than, Y]).
rule( weakness, [[X, is, water], [Y, is, grass]] ==> [X, weaker_than, Y]).

%%Rules for protective parents who are angry at pokemon attacking their children
%%If a pokemon is being attacked and is weak, their parent gets angry
rule( angry, [[X, vulnerable_to, Y], [Z, is_parent_of, X]] ==> [Z, angry_at, Y]).
%%If a pokemon is angry at another and stronger than them, they want to attack
rule( angry, [[X, angry_at, Y], [X, stronger_than, Y]] ==> [X, wants_attack, Y]).
%%If a pokemon's child is young (<8), they want to defend them
rule( angry, [[Z, is_child_of, X], [Z, is_age, A], test(A<8)] ==> [X, wants_defend, Z]).

%%Rules for defence mechanisms      
%%If a pokemon wants to defend someone being attacked and wants to attack, they will attack.
rule( defend, [[X, wants_defend, Z], [Y, attacks, Z], [X, wants_attack, Y]] ==> [X, attacks, Y]).
%%If a pokemon attacks to defend someone being attacked, that someone is saved from the attacker.
rule( defend, [[X, wants_defend, Z], [X, wants_attack, Y], [Y, attacks, Z]] ==> [Z, being_saved_from, Y]).
%%If a pokemon is being saved, their attacker does not beat them.
rule( defend, [[Y, attacks, Z], [Z, being_saved_from, Y]] ==> -[Y, beats, Z]).
%%rule for being armed with defence equipment
rule( defend, [[X, attacks, Y], [Y, is_armed]] ==> -[X, beats, Y]). 
                  
%%rule( puppies, [[X, is, dog], [X, is_age, A], test(A<2)] ==> [X, is, puppy] ).
%%rule( happiness, [[X, is_parent_of, Y], [Y, is, happy]] ==> [X, is, happy] ).
%%rule( happiness, [[X, is, puppy]]  ==> [X, is, happy] ).
%%rule( games, [-[X, is, short]] ==> [X, can_play, basketball] ).
%%rule( games, [[X, plays, Y]] ==> [X,likes,Y] ).

%% Default rules make inferences on condition that something is not provable.
%% Such rules should go after the positive inference rules.
%%If a pokemon is in a vulnerable situation with no defence and no-one saving them, they will be beaten.
rule( default, [[X, vulnerable_to, Y], \+[X, being_saved_from, Y], \+[X, armed]] ==> [Y, beats, X]).
%%If a pokemon is neither stronger or weaker than another, they are neutral to them in an attack.
rule( default, [\+[X, stronger_than, Y], \+[X, weaker_than, Y], [X, attacks, Y]] ==> [X, neutral_to, Y]). 

%%rule( default,  [[X, is, bird], \+[flightless,X] ]  ==>  [X, flies] ).
%%rule( default,  [[X, can_play, Y], \+(-[X, plays, Y]) ]  ==>  [X,plays,Y] ).
%%rule( default,  [[F, is_father_of, C], [M, is_mother_of, C], \+(-[F, is_married_to, M])]
%%                  ==> [F, is_married_to, M]).
%%rule( default,  [[X, and, Y, have_common_interest], \+(-[X, likes, Y]) ]  ==>  [X,likes,Y] ).


%% Define how to interpret 'test' conditions in rule preconditions:
test( X < Y ) :- X < Y.
test( different(X,Y) ) :- \+(X=Y).


%% THE INFERENCE MECHANISM

%% applyrule: check if premisses are satisfiable. If so assert the conclusion.

applyrule( T, [] ==> Conc ) :- % If no premisses, assert conclusion (unless already known).
    \+( fact( Conc ) ),
    assert( fact( Conc ) ),
    (show_inferences -> show_inference(Conc, T) ; true).

applyrule( T, [-(P) | Rest] ==> Conc ) :-   % Check strong negated premiss is a negative fact
    fact( -(P) ),
    applyrule( T, Rest ==> Conc ).

applyrule( T,  [\+(P) | Rest] ==> Conc ) :-   % Check weak negated premiss is not a fact
    \+(fact( P )),
    applyrule( T, Rest ==> Conc ).

applyrule( T, [test(Test) | Rest] ==> Conc ) :-  % Evaluate a test condition.
     ground(Test), test( Test ),
     applyrule( T, Rest ==> Conc ).

applyrule( T,[Prem | Rest] ==> Conc ) :-     % Look for fact matching first premiss
     fact( Prem ),
     applyrule( T, Rest ==> Conc ).

%% infer applies all rules to all currently stored facts.
infer :- ruleset(Rules),
         bagof( i, R^Type^( rule(Type, R), member(Type, Rules), applyrule(Type, R)), Infs ),
         length( Infs, Len ),
         write('*** Number of inferences carried out: '), write( Len ), nl, nl,
         Len > 0.  % fail if no inferences found.

%% infer/1 repeatedly calls infer up to a given inference depth limit.
infer( Limit ) :- infer( 1, Limit ).
infer( Depth, Limit ) :- Depth>Limit, !, write( '*** Max inference depth reached' ), nl, nl.
infer( Depth, Limit ) :- write( '*** Inference depth: ' ), write( Depth ), 
                              write( ' ... ' ), nl,
                              infer, !, 
                              Next is Depth + 1, infer( Next, Limit ).
infer( _, _ ) :- write( '*** No more inferences found' ), nl, nl.          


%% Useful Display Predicates
%% Show all facts 
allfacts :-  write('=== ALL KNOWN FACTS ==='), nl, (fact(F), write(F), nl, fail) ; true.
%% Show all facts involving terms in list L
describe(L) :- L=[] ; (L = [H|T], describe(H), describe(T)).
describe(X) :- write('=== Facts involving: '), write( X ), write(' ==='), nl, 
              ( (fact(F), (member(X,F);(F= -(G), member(X,G))), write(F), nl, fail) ; true ).

show_inference( Conc, Type ) :-
    format( 'Infer: ~p ~50+  (~p)~n', [Conc, Type] ).

show_inferences :- true. %% Change this to false to hide inference output.

/** <examples>
 %% You can add your own queries here.
 
 %% Display known facts about a thing or list of things:
?- describe( champ ).
?- describe( [pete,rex,tweety] ).

?- allfacts. %% Display all known facts.

%% Compute inferences up to a given depth:
?- infer(1).
?- infer(2). 
?- infer(5). 

%% Compute inferences to a given level; then display known facts (including those inferred):
?- infer(3), describe([rex,lassie,fido,champ]).
?- infer(3), describe([tweety]).    
?- infer(5), allfacts.
*/


