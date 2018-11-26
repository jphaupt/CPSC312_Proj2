https://www.aransena.com/blog/2015/12/22/python-ant-colony-optimization


% Represents the city, (x,y) coordinates
city('Vancouver', 3, 5).
city('Montreal', 7, 9).
city('Toronto', 2, 8).
city('Calgary', 8, 11).
city('Ottawa', 9, 13).

% assume only one ant for now

find_next_city(Ant, CurrentCity, Visited, Path) :-
  % Find distance between CurrentCity and all other cities -- find minimum or if
  % then use probability

% Need to somehow represent ants
% also need to represent the current city
ant :: {
  city('Vancouver', 3, 5) &
  distance(0) &
  visited([])
}.

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
