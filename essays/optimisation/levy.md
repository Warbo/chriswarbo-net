---
title: Levy Flight
---
Levy flights are a modification of the standard random walk which uses a random step length as well as a random direction. The characteristic of Levy flights is that the step lengths are chosen from a "heavy tailed" probability distribution, which means the decreasing probabilities of longer lengths are not small enough to overpower the increasing lengths; technically, a heavy tailed distribution has infinite variance (possible length).

The probability distribution I've used here is called the [Pareto distribution](http://en.wikipedia.org/wiki/Pareto_distribution), and it's roughly 1/(x^A). This gives us a more complex search algorithm than our standard random walks, but the advantage is that various length scales are used. The result of having small distances with a high probability and long distances with a low probability is that each search will generally check a few solutions in an area before moving on to a far-off area to repeat the process. This is much more efficient than our constant-length random walks, which have to make a tradeoff between checking nearby and checking far away. Levy flights do both! We do still end up with a parameter to tweak (the constant "A" in the 1/(x^A) behaviour, or alpha in the Wikipedia article), but this can be chosen quite roughly based on the problem, rather than fiddled-with based on how badly the simulation's doing (which is usually the case for random walks' step sizes).

In this example, like with random walks, the fitness of a particular location is shown by how light the grey colour is. This is randomly generated when the page loads.

Each (x, y) point in the square is a solution, and our goal is to find the best (lightest). We start in the centre and move a random distance at a random angle at each step, with distances following a Pareto distribution and angles being uniform.

Due to the random path lengths, the wrap-around technique used in the enumeration and random walk examples isn't very easy to get right. Instead, if the next step ends up outside the visible area we do 2 things: if the step length is more than half the visible width then we keep choosing new ones until it's less; this avoids trying to take such a big step that it will never fit on the screen. Next we keep choosing new angles until we end up with a step that will fit on the screen. This biases the search a little compared to wrapping around, but makes it simpler to understand what's going on.

<div id="levy_playfield" style="width: 500px; height: 500px;"></div>
<form action="#" type="get">
<div>
  <input type="range" name="_" id="levy_number" min="0" max="10" value="0" style="width: 500px;" />
  <label for="levy_number">Number of searches:</label>&nbsp;&nbsp;<a id="levy_number_display"></a>
</div>
<div>
  <input type="range" name="_" id="levy_fitness" min="0" max="100" value="10" style="width:500px;" />
  <label for="levy_fitness">Desired fitness:</label>&nbsp;&nbsp;<a id="levy_fitness_display"></a>
</div>
<div>
  <input type="range" name="_" id="levy_scale" min="1" max="3" value="1" style="width: 500px;" />
  <label for="levy_scale">Scale:</label>&nbsp;&nbsp;<a id="levy_scale_display"></a>
</div>
</form>
{$REQUIRE_JAVASCRIPT,javascript_jquery_svg}
{$REQUIRE_JAVASCRIPT,javascript_underscore}
{$REQUIRE_JAVASCRIPT,javascript_cedi_levy}

Moving the "Number of searches" slider will add and remove instances of the search. All of them are running the same code, but their random angles and distances will make them go to different places. "Scale" changes the length distribution used. The "Desired fitness" slider tells the searches when to stop. Setting it low makes the search complete quickly, but with a poor solution; setting it high guarantees a better solution, but this may never be found. Welcome to the [halting problem](http://en.wikipedia.org/wiki/Halting_problem)!

Levy flights are quite simple search algorithms, but their results can be pretty good compared to naive enumeration and random walks. Their lack of any form of memory is their major weakness, as memory can give us huge improvements.
