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

% Need to somehow represent ants
% also need to represent the current city
%ant :: {
%  city('Vancouver', 3, 5) &
%  distance(0) &
%  visited([])
%}.



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




%%%% HELPER FUNCTIONS %%%%
% replace Ith item in L with E (this is K) 
replace(I, L, E, K) :-
  nth1(I, L, _, R),
  nth1(I, K, E, R).

% fancy schmancy matrix creation function
make_sq_zero_matrix(N, Matrix) :-
    make_matrix(N, N, Matrix).

make_zero_matrix(_, N, []) :-
    N =< 0,
    !. % ??? TODO
make_zero_matrix(M, N, [R|Rs]) :-
    make_zero_list(M, R),
    N2 is N - 1,
    make_matrix(M, N2, Rs).

% make list of zeros
make_zero_list(N, []) :-
    N =< 0,
    !.
make_zero_list(N, [0|Rest]) :-
    N > 0,
    N2 is N - 1,
    make_list(N2, Rest).

% starting pheromone matrix
% make_sq_zero_matrix(num_cities, Pher_matr)

% TODO adjacency matrix

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
