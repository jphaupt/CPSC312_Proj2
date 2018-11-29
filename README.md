# CPSC312_Proj2
**By J. Philip Haupt and Frances Chong**

Here we use Prolog to visit every node in a complete graph exactly once, and find the shortest path doing so (this is the travelling salesman problem except without returning to the original node). We solve it using a "swarm intelligence" algorithm called ant colony optimisation. In this approach, we have "ants" traverse the graph and visit every node probabilistically (at first completely random), and leave "pheromone trails" for the other ants, based on how long their path was. Each ant is completely independent of the other and they only interact via pheromones, but they converge to the correct solution. Output is a pheromone matrix (like an adjacency matrix but instead of distance, how much pheromone is deposited for each edge: larger means an ant is more likely to take that edge), as well as the final paths and distances associated. 

This repository contains all files for this project.

[Link to a useful resource we used.](https://www.aransena.com/blog/2015/12/22/python-ant-colony-optimization)
