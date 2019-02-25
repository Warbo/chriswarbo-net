---
title: Levy Flight
---
Levy flights are a modification of the standard random walk which uses a random step length as well as a random direction. The characteristic of Levy flights is that the step lengths are chosen from a "heavy tailed" probability distribution, which means the decreasing probabilities of longer lengths are not small enough to overpower the increasing lengths; technically, a heavy tailed distribution has infinite variance (possible length).

The probability distribution I've used here is called the [Pareto distribution](http://en.wikipedia.org/wiki/Pareto_distribution), and it's roughly 1/(x^A). This gives us a more complex search algorithm than our standard random walks, but the advantage is that various length scales are used. The result of having small distances with a high probability and long distances with a low probability is that each search will generally check a few solutions in an area before moving on to a far-off area to repeat the process. This is much more efficient than our constant-length random walks, which have to make a tradeoff between checking nearby and checking far away. Levy flights do both! We do still end up with a parameter to tweak (the constant "A" in the 1/(x^A) behaviour, or alpha in the Wikipedia article), but this can be chosen quite roughly based on the problem, rather than fiddled-with based on how badly the simulation's doing (which is usually the case for random walks' step sizes).

In this example, like with random walks, the fitness of a particular location is shown by how light the grey colour is. This is randomly generated when the page loads.

Each (x, y) point in the square is a solution, and our goal is to find the best (lightest). We start in the centre and move a random distance at a random angle at each step, with distances following a Pareto distribution and angles being uniform.

Like the other algorithms, the edges will wrap around. For simplicity, when this happens we don't draw a line. Sometimes it may look like the search has teleported from one place to another, but it's actually gone off the edge and wrapped around!

<div id="levy_playfield" style="width: 500px; height: 500px;">
</div>

<form action="#" method="get">
</form>

<script src="/js/jquery.js">
</script>

<script src="/js/jquery_svg.js">
</script>

<script src="/js/underscore.js">
</script>

<script src="/js/optimisation/levy.js">
</script>

Click the box to start the search. The fittest solution found so far is highlighted in green.

Levy flights are quite simple search algorithms, but their results can be pretty good compared to naive enumeration and random walks. Their lack of any form of memory is their major weakness, as memory can give us huge improvements.
