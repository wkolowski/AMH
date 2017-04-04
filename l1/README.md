# Metaheuristic algorithms in Haskell

I'm doing this for my university class. I choose Haskell because, compared to faster languages like C, it gives me huge advantages when it comes to control and composability. I was also curious how fast it can be made with some effort (like using mutable arrays).

## Traveling Salesman Problem (TSP)

In this task, _x_ and _y_ coordinates of points are given and one has to find the shortest path that passes through all points (the begin- and endpoints have to be equal).

### Solutions

1. Brute force — works for data of size < 10.
2. Greedy algorithm — works pretty quickly for all datasets I tested (size < 15000). As an initial solution for hill climbing it isn't very good for small data (as it's an unescapable local optimum), but is superior to any hill climbing for data of large sizes.
3. Hill Climbing — a simple algorithm that starts with an initial solution and then keeps modifying ("tweaking") it and checking if the solution quality improved. It can also restart itself if it encounters a local optimum. It's very general — the exact meaning of "initial solution", "tweak" etc. can be controlled by passing adequate functions as arguments. It comes in two versions: one based on lists and the other on arrays. The second one seems far quicker, but some things are left unimplemented yet.

### Test data

A quite large dataset with various problem sizes and known optima is the [TSPLIB](http://elib.zib.de/pub/mp-testdata/tsp/tsplib/tsplib.html)

### Miscellaneous

The Hill Climbing algorithms uses STM to log the best solution each second. There's some Julia code that can then draw some nice graphs using this data.

### TODO

1. The array version of Hill Climbing is not yet fully implemented.
2. The Hill Climbing's condig got conflated with its state. This should be refactored.
3. I should implement Tabu Search.
