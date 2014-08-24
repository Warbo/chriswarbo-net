---
title: Hill climbing
---
Hill climbing is a simple enhancement to random walks that only takes a step if the fitness of the new solution is better than the current solution. This means that we definitely get better and better solutions, but it is also guaranteed to get stuck at the first peak it finds, whether it's any good or not.

In this example, like with the other random walks, the fitness of a particular location is shown by how light the grey colour is. This is randomly generated when the page loads.

Each (x, y) point in the square is a solution, and our goal is to find the best (lightest). We start in the centre and move a set distance at a random angle at each step. Like the enumeration method, I've made the edges wrap around. There's no need for a trick like using irrational angles this time, since we're not imposing an order on the search space. This is an advantage of algorithms based on random walks, since they can be dumped on to any problem without any messing around like  craft an exhaustive enumeration.

<div id="hill_playfield" style="width: 500px; height: 500px;"></div>
<form action="#" type="get">
<div>
  <input type="range" name="_" id="hill_number" min="0" max="10" value="0" style="width: 500px;" />
  <label for="hill_number">Number of searches:</label>&nbsp;&nbsp;<a id="hill_number_display"></a>
</div>
<div>
  <span>Current fitness:</label>&nbsp;&nbsp;<a id="hill_fitness_display"></a>
</div>
<div>
  <input type="range" name="_" id="hill_step" min="1" max="10" value="2" style="width: 500px;" />
  <label for="hill_step">Step size:</label>&nbsp;&nbsp;<a id="hill_step_display"></a>
</div>
</form>
<script src="/js/jquery.js"></script>
<script src="/js/jquery_svg.js"></script>
<script src="/js/underscore.js"></script>
<script src="/js/optimisation/hill.js"></script>

Moving the "Number of searches" slider will add and remove instances of the search. All of them are running the same code, but their random angles will be different (since they're, um, random). The "Step size" tells the searches how far to move from their current location when they take a step; low values mean that not much of the space gets explored, high values mean that nearby solutions get missed. We don't need a "Desired fitness" slider this time, since the search will stop once it can't find a better solution. We have no idea if the solution it finds will be the best one though; welcome to the [halting problem](http://en.wikipedia.org/wiki/Halting_problem)!

Whilst hill climbing is quite a simple search algorithm, it can give reasonable enough results for simple, smooth search spaces (ie. where it can't get stuck easily). For problems which have a bumpy search space, hill climbing will get stuck at the top of whichever bump it started on. Since hill-climbing will never sacrifice its current fitness, which makes it get stuck, this does give it the nice property of being an [i]anytime algorithm[/i], which means we can stop it at any point and be sure that we're given the best solution found so far. If the algorithm were to cross the valleys between the peaks, then it wouldn't get stuck; however, we may end up stopping it while it's in the middle of a valley, and thus our final solution would be terrible.
