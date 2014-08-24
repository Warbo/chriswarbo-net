---
title: Random Walk
---
Random walks are slightly more complex search algorithms than enumeration. Rather putting all of the solutions in some kind of order (which may end up putting good solutions very far away), we keep every parameter (in our case the x and y coordinates) separate, and just adjust each one by a small amount each time. Once again, we keep doing this until we find a solution that's "good enough".

In this example, like with enumeration, the fitness of a particular location is shown by how light the grey colour is. This is randomly generated when the page loads.

Each (x, y) point in the square is a solution, and our goal is to find the best (lightest). We start in the centre and move a set distance at a random angle at each step. Like the enumeration method, I've made the edges wrap around. There's no need for a trick like using irrational angles this time, since we're not imposing an order on the search space. This is an advantage of random walks, since they can be dumped on to any problem without any messing around like  craft an exhaustive enumeration. The disadvantage, as you may notice, is that the same (or nearly the same) solutions are checked again and again!

<div id="walk_playfield" style="width: 500px; height: 500px;"></div>
<form action="#" type="get">
<div>
  <input type="range" name="_" id="walk_number" min="0" max="10" value="0" style="width: 500px;" />
  <label for="walk_number">Number of searches:</label>&nbsp;&nbsp;<a id="walk_number_display"></a>
</div>
<div>
  <input type="range" name="_" id="walk_fitness" min="0" max="100" value="10" style="width:500px;" />
  <label for="walk_fitness">Desired fitness:</label>&nbsp;&nbsp;<a id="walk_fitness_display"></a>
</div>
<div>
  <input type="range" name="_" id="walk_step" min="1" max="10" value="2" style="width: 500px;" />
  <label for="walk_step">Step size:</label>&nbsp;&nbsp;<a id="walk_step_display"></a>
</div>
</form>
<script src="/js/jquery.js"></script>
<script src="/js/jquery_svg.js"></script>
<script src="/js/underscore.js"></script>
<script src="/js/optimisation/walk.js"></script>

Moving the "Number of searches" slider will add and remove instances of the search. All of them are running the same code, but their random angles will be different (since they're, um, random). The "Step size" tells the searches how far to move from their current location when they take a step; low values mean that not much of the space gets explored, high values mean that nearby solutions get missed. The "Desired fitness" slider tells the searches when to stop. Setting it low makes the search complete quickly, but with a poor solution; setting it high guarantees a better solution, but this may never be found. Welcome to the [halting problem](http://en.wikipedia.org/wiki/Halting_problem)!

Whilst random walks are very simple search algorithms, their results can be pretty poor. Their lack of bias makes them easily applicable to many problems, but their repetition makes them not worth using ;)
