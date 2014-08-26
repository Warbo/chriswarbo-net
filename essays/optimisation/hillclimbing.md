---
title: Hill climbing
---
Hill climbing is a simple enhancement to random walks that only takes a step if the fitness of the new solution is better than the current solution. This means that we definitely get better and better solutions, but it is also guaranteed to get stuck at the first peak it finds, whether it's any good or not.

In this example, like with the other random walks, the fitness of a particular location is shown by how light the grey colour is. This is randomly generated when the page loads.

Each (x, y) point in the square is a solution, and our goal is to find the best (lightest). We start in the centre and move a set distance at a random angle at each step. Like the enumeration method, I've made the edges wrap around. There's no need for a trick like using irrational angles this time, since we're not imposing an order on the search space. This is an advantage of algorithms based on random walks, since they can be dumped on to any problem without any messing around like  craft an exhaustive enumeration.

<div id="hill_playfield" style="width: 500px; height: 500px;"></div>
<form action="#" type="get">
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

Click the box to start the search. The "Step size" tells the search how far to move from its current location when taking a step; low values mean that not much of the space gets explored, high values mean that nearby solutions may get missed. Since the search will only move to points with higher fitness, there's no need for a separate "best so-far" marker.

Whilst hill climbing is quite a simple search algorithm, it can give reasonable enough results for simple, smooth search spaces (ie. where it can't get stuck easily). For problems which have a bumpy search space, hill climbing will get stuck at the top of whichever bump it started on.
