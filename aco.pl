% https://www.aransena.com/blog/2015/12/22/python-ant-colony-optimization

:- use_module(library(random)).
%:- use_module(library(plrand)).

% Represents the city, (x,y) coordinates
% city(name, x, y, pheromone_level)
city('Vancouver', 3, 5). % 0
city('Montreal', 7, 9). % 1
city('Toronto', 2, 8). % 2
city('Calgary', 8, 11). % 3
city('Ottawa', 9, 13). % 4

%[[0, 3, 4]
%[3, 0, 1]
%[4, 1, 0]]

%select_city(start_city, end_city)
%start_city == 0
%end_city in [1,2]

%city('YVR', 3, 5, P+1) :- ant_visit(city('YVR', 3, 5, P), true)

% assume only one ant for now

%find_next_city(Ant, CurrentCity, Visited, Path) :-
  % Find distance between CurrentCity and all other cities -- find minimum or if
  % then use probability

% pheromone deposit
%pher_dep(ant(city(N, _, _, P), _, _), city(N, _, _, P+1)).


%tour_ant(lst

%tour_all(

%aco_tsp(

% for ant in ants:
%    current_city = ant.current_city
%    transition_probabilities=[]
%    for city in unvisited:
%        denominator = 0
%        numerator = (pow(tau[current_city][city],alpha)*(eta[current_city][city], beta))
%        for city in unvisited:
%            denominator += (pow(tau[current_city][city],alpha)*(eta[current_city][city], beta))
%
%        p_ij = numerator/float(denominator)
%        transition_probabilities.append(p_ij)
%
%    next_city = numpy.choice(unvisited, 1, transition_probabilities)

% unvisited cities (indices), pheromone matrix, distance so far, current city, adjacency matrix
% Paths is list of paths (as tuples) so far traversed
%ant_tour(Unvisited, PherMatr, Dist, Current, AM, Paths)
%ant_tour([], _, _, _, _).
%ant_tour([H|R], _, _, 

% assume: head of list in this function is the current city (the colony)
% tour for a single ant
% NOTE Visited is in reverse order!
ant_tour([H|Unvisited], PherMatr, AdjMat, Dist, Visited) :-
  ant_tour(Unvisited, PherMatr, AdjMat, H, 0, Dist, [H], Visited).

ant_tour([], _, _, _, D, D, N, N).

ant_tour(Unvisited, PherMatr, AdjMat, Current, AccD, Dist, Acc, Visited) :-
  trans_prob(Unvisited, PherMatr, AdjMat, Current, Probs),
  choice(Probs, Ind), 
  nth0(Ind, Unvisited, Next, R), % delete and store Ind^th element
  at(AdjMat, Current, Next, D),
  NewDist is D+AccD,
  ant_tour(R, PherMatr, AdjMat, Next, NewDist, Dist, [Next|Acc], Visited).


%%%% HELPER FUNCTIONS %%%%
% calculate transition probabilities based on pheromones and distance
% TODO I'm not 100% sure if this is correct (might be reversed results)
trans_prob(Lst, PherMatr, AdjMat, Current, Probs) :- 
  numerators(Lst, PherMatr, AdjMat, Current, Numerators), % get numerators
  sumlist(Numerators, Denom), % get denominator
  maplist(divide(Denom), Numerators, Probs).

% helper function for maplist
divide(V, A, B) :- B is A/V.

% tmp TODO
% make_sq_ones_matrix(3, ph).
% adjacency_matrix([(1,2), (0,0), (5,2)], d).
% [0,1,2], [[1, 1, 1], [1, 1, 1], [1, 1, 1]], [[0, 5, 16], [5, 0, 29], [16, 29, 0]]

% helper function for transmission probabilities
% assuming Current is *not* in the list
numerators(Lst, PherMatr, AdjMat, Current, Numerators) :-
  numerators(Lst, PherMatr, AdjMat, Current, [], Numerators).

numerators([], _, _, _, N, N).

numerators([H|T], PherMatr, AdjMat, Current, Acc, Numerators) :- 
  at(PherMatr, Current, H, Tij),
  at(AdjMat, Current, H, Dij), 
  NH is Tij/Dij,
  numerators(T, PherMatr, AdjMat, Current, [NH|Acc], Numerators).


% replace Ind^th item in Old with E (resulting list is New)
replace(Ind, Old, E, New) :-
  nth0(Ind, Old, _, R),
  nth0(Ind, New, E, R).

% fancy schmancy matrix creation function
make_sq_ones_matrix(N, Matrix) :-
    make_ones_matrix(N, N, Matrix).

make_ones_matrix(_, N, []) :-
    N =< 0,
    !. % ??? TODO
make_ones_matrix(M, N, [R|Rs]) :-
    make_ones_list(M, R),
    N2 is N - 1,
    make_ones_matrix(M, N2, Rs).

% make list of zeros
make_ones_list(N, []) :-
    N =< 0,
    !.
make_ones_list(N, [1|Rest]) :-
    N > 0,
    N2 is N - 1,
    make_ones_list(N2, Rest).

% Update pheromone levels after a tour
% Input:  The first variable is the list cities visited in the tour
%         PherMat is the current pheromone matrix
%         UpdatedPherMat is the updated pheromone matrix
update_pheromone([X,Y], PherMat, UpdatedPherMat) :-
  at(PherMat, X, Y, Val),
  nth0(X, PherMat, XRow),
  NewVal is Val+1,
  replace(Y, XRow, NewVal, UpdatedRow),
  replace(X, PherMat, UpdatedRow, UpdatedPherMat).

update_pheromone([X,Y|T], PherMat, UpdatedPherMat) :-
  at(PherMat, X, Y, Val),
  nth0(X, PherMat, XRow),
  NewVal is Val+1,
  replace(Y, XRow, NewVal, UpdatedRow),
  replace(X, PherMat, UpdatedRow, UPM),
  update_pheromone([Y|T], UPM, UpdatedPherMat).

% Returns the value at specific element
% 0_based indexing
% Val is the value at that specific element
at(Mat, Row, Col, Val) :-
  nth0(Row, Mat, ARow),
  nth0(Col, ARow, Val).

% starting pheromone matrix
% make_sq_zero_matrix(num_cities, Pher_matr)

% Create adjacency matrix of distances
% Input a list of (x,y) coordinates
% Outputs an adjacency AM
adjacency_matrix(Coords, AM) :- adjacency_matrix(Coords, Coords, AM).

% adjacency_matrix([(3,2),(4,5),(6,7)],[(3,2),(4,5),(6,7)], AM).
adjacency_matrix([], _, []).
adjacency_matrix([H|T], Coords, [L|AM]) :-
  distance_list(H, Coords, L),
  adjacency_matrix(T, Coords, AM).

% Creates a distance list between the current coordinate with the rest of the list
distance_list((_,_), [], []).
distance_list((X1,Y1), [(X2,Y2)|T], [D|L]) :-
  dist((X1,Y1), (X2,Y2), D),
  distance_list((X1,Y1), T, L).

% distance squared
dist((X1, Y1), (X2, Y2), D) :- D is (X1-X2)^2+(Y1-Y2)^2.

% distance squared
dist2(city(_, X1, Y1), city(_, X2, Y2), D) :- D is (X1-X2)^2+(Y1-Y2)^2.

% find argmax (0 indexing)
argmax(Lst, Ind) :-
  max_list(Lst, Max),
  nth0(Ind, Lst, Max).

% turn discrete probabilities to cumulative probabilities
cum_sum(X, T) :-
  cum_sum(X, 0, T).

cum_sum([], _, []).
cum_sum([X|R], S, [T|R2]) :-
  T is S+X,
  cum_sum(R, T, R2).

% choose based on values in Probs (return index)
choice(Probs, Ind) :-
  random(Rand),
  cum_sum(Probs, CumProbs),
  choice(CumProbs, Rand, 0, 0, Ind).

% first arg is a cumulative discrete probability distribution
choice([], _, _, IA, IA).
choice([H|_], Rand, SA, IA, IA) :-
  SA =< Rand,
  Rand < H+SA.

choice([H|R], Rand, SA, IA, Ind) :-
  Rand > H+SA,
  SA2 is H+SA,
  IA2 is IA+1,
  choice(R, Rand, SA2, IA2, Ind).

%choice([H|R], SA, IA, Ind) :- H
%choice(A) :- 1 =< A, A < 5.
