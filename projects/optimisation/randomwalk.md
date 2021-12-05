---
title: Random Walk
---
Random walks are slightly more complex search algorithms than enumeration.
Rather than putting all of the solutions in some kind of order (which may end up
putting good solutions very far away), we keep every parameter separate (in our
case the x and y coordinates), and adjust each one by a small amount each time.
Once again, we keep doing this until we find a solution that's "good enough".

In this example, like with enumeration, the fitness of a particular location is
shown by how light the grey colour is. This is randomly generated when the page
loads (if the square looks black, try reloading the page).

Each (x, y) point in the square is a solution, and our goal is to find the best
(lightest). We start in the centre and adjust our coordinates by moving a set
distance in a randomly-chosen direction. Like the enumeration method, I've made
the edges wrap around. There's no need for tricks in the way we represent the
search space, since we're not imposing a fixed order on the points. This is an
advantage of random walks, since they can be dumped on to any problem without
any messing around like crafting an exhaustive enumeration. The disadvantage, as
you may notice, is that the same (or nearly the same) solutions are checked
again and again!

<div id="walk_playfield" style="width: 500px; height: 500px;"></div>

<form action="#" method="get">
 <div>
  <input type="range"
         name="_"
         id="walk_step"
         min="1" max="10" value="2"
         style="width: 500px;" />
  <label for="walk_step">Step size:</label>&nbsp;&nbsp;<a
         href="#" id="walk_step_display"></a>
 </div>
</form>

<script src="/js/jquery.js">
</script>

<script src="/js/jquery_svg.js">
</script>

<script src="/js/underscore.js">
</script>

<script src="/js/optimisation/walk.js">
</script>

Click the box to start the search. The "Step size" tells the search how far to
move from its current location when taking a step; low values mean that not much
of the space gets explored, high values mean that nearby solutions may get
missed. The best solution found so far is highlighted in green. The search will
keep going until you leave/reload the page.

Whilst random walks are very simple search algorithms, their results can be
pretty poor. Their lack of bias makes them easily applicable to many problems,
but their repetition makes them not worth using ;)
