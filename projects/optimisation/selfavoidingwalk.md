---
title: Self-avoiding Random Walk
---
Self-avoiding random walks improve upon regular random walks by keeping a memory of each visited location/solution. If the next (randomly chosen) step would end up revisiting a location then a different choice is made (and so on until a new location is found). Once again, we keep doing this until we find a solution that's "good enough".

This avoids the problem of random walks repeating themselves, but it comes with two costs: an ever-increasing use of memory as all of the locations are stored, and the possibility of getting trapped (the algorithm "paints itself into a corner").

In this example, like with a standard random walk, the fitness of a particular location is shown by how light the grey colour is. This is randomly generated when the page loads.

Each (x, y) point in the square is a solution, and our goal is to find the best (lightest). We start in the centre and move a set distance at a random angle at each step. If we've been there before (actually, we round to the nearest pixel to avoid arbitrarily-close fractions) then we discard the move and choose a different random angle. Like the standard random walk, I've made the edges wrap around. Note that if you choose a step size of 1 then the trails won't be removed like they were for the random walk. This allows the visited areas to be seen (we couldn't do this for larger step sizes, as it's only the end points that are chosen, not the line in-between). Moving the slider down to remove a search, then up to add it again will reset it's list of visited locations to zero.

<div id="avoid_playfield" style="width: 500px; height: 500px;">
</div>

<form action="#" method="get">
 <div>
  <input type="range" name="_" id="avoid_step" min="1" max="10" value="2" style="width: 500px;" />
  <label for="avoid_step">Step size:</label>&nbsp;&nbsp;<a href="#" id="avoid_step_display"></a>
 </div>
</form>

<script src="/js/jquery.js">
</script>

<script src="/js/jquery_svg.js">
</script>

<script src="/js/underscore.js">
</script>

<script src="/js/optimisation/avoid.js">
</script>

Clicking the box will start the search. The "Step size" tells the search how far to move from its current location when taking a step; low values mean that not much of the space gets explored, high values mean that nearby solutions may get missed. The best solution found so far is marked in green. Note that, to prevent killing your computer, the search will start forgetting old locations once 10,000 steps have been taken.

Whilst random walks are very simple search algorithms, their results can be pretty poor. Their lack of bias makes them easily applicable to many problems, but their repetition makes them not worth using ;)
