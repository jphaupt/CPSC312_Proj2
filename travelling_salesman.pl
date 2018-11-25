
edge(bi,br, 9).
edge(lo,bi, 3).
edge(lo,br, 6).
edge(lo,pl, 5).
edge(pl,lo, 5).
edge(po,lo, 4).
edge(po,pl, 10).

% Finds a path between the two places
find_path(Start, End, Visited, Result) :-
    find_path(Start, End, [Start], 0, Visited, Result).

find_path(Start, End, Path, DistAcc, Visited, TotalDist) :-
    edge(Start, End, Dist),
    reverse([End|Path], Visited),
    TotalDist is DistAcc + Dist.

find_path(Start, End, Path, DistAcc, Visited, TotalDist) :-
      edge(Start, Next, Dist),
      \+ member(Next, Path),
      NewTotalDistAcc is DistAcc + Dist,
      find_path(Next, End, [Next|Path], NewTotalDistAcc, Visited, TotalDist).

% Searches for the optimal path
% TODO: find a way to look for the lowest cost - need to call find_path to find
% possible paths until you find the cheapest total path
find_optimal(Start, End) :-
  find_path(Start, End, Visited, Distance).


% find_path(po, pl, Visited, Distance).
% TODO: add more functionality (i.e. weights of the graph)
% https://github.com/LazoCoder/Ant-Colony-Optimization-for-the-Traveling-Salesman-Problem (cool demos)
