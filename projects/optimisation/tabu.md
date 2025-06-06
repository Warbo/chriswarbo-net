---
title: Tabu Search
---
Tabu search (or taboo search) improves self-avoiding random walks by generalising the forbidden (tabu/taboo) moves. Rather than just forbidding repetition, tabu search can impose other arbitrary limitations. To keep things simple and general, I'll stick to just forbidding previously visited solutions. Note that this would seem to give us exactly the self-avoiding random walk, but actually the forbidden list in tabu search must be finite; in other words, we forget old locations once our memory limit is reached. This allows the search to keep going as long as necessary, unlike the self-avoiding random walk, and it reduces the potential for getting trapped (forming a ring of visited solutions around ourself). Once again, we keep going until we find a solution that's "good enough".

In this example, like with the other random walks, the fitness of a particular location is shown by how light the grey colour is. This is randomly generated when the page loads.

Each (x, y) point in the square is a solution, and our goal is to find the best (lightest). We start in the centre and move a set distance at a random angle at each step. If the location is in our tabu list (once again, rounded to the nearest pixel) then we keep choosing new angles until we find a non-tabu location. Once we've moved to a new location, we add it to our tabu list, and we shift off an item from the start of the list if it's over a certain length. Like the standard random walk, I've made the edges wrap around. Note that if you choose a step size of 1 then the trails will only be removed once they leave the tabu list. This allows the tabu areas to be seen (we couldn't do this for larger step sizes, as it's only the end points that are chosen, not the line in-between).

<div id="tabu_playfield" style="width: 500px; height: 500px;">
</div>

<form action="#" method="get">
 <div>
  <input type="range" name="_" id="tabu_step" min="1" max="10" value="2" style="width: 500px;" />
  <label for="tabu_step">Step size:</label>&nbsp;&nbsp;<a href="#" id="tabu_step_display"></a>
 </div>
</form>

<script src="/js/jquery.js">
</script>

<script src="/js/jquery_svg.js">
</script>

<script src="/js/underscore.js">
</script>

<script src="/js/optimisation/tabu.js">
</script>

Clicking the box will start the search. The "Step size" tells the search how far to move from its current location when it takes a step; low values mean that not much of the space gets explored, high values mean that nearby solutions may get missed. The fittest solution found so far is marked in green.

Whilst tabu search is a very simple search algorithm, its results end up depending on the particular problem it's given. This is because tabu search is a "local search" method, which means it can't effectively exploit the whole potential of the search space; if will prefer reasonable solutions that are nearby to  excellent solutions that are far away. However, for simple problems where there's an obvious path to the good solutions, tabu search can do well (especially when augmenting a more sophisticated algorithm like hill climbing, rather than a naïve random walk).
