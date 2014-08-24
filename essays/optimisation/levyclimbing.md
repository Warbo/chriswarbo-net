---
title: Hill climbing levy flight
---
Hill climbing is a simple enhancement to levy flights that only takes a step if the fitness of the new solution is better than the current solution. This means that we definitely get better and better solutions, but there is also a high probability of getting stuck at a small peak. By using a levy flight, we can theoretically reach any point in the search space from any other point, so unlike hill climbing we won't [i]definitely[/i] get stuck on the first peak we find; however, since we never take a step that's less fit than the current location, we are forced to jump between peaks in one move, which is highly unlikely. The better the peak we're on, the smaller the acceptable area of better peaks is, and thus the harder it is to jump to them.

In this example, like with the other random walks, the fitness of a particular location is shown by how light the grey colour is. This is randomly generated when the page loads.

Each (x, y) point in the square is a solution, and our goal is to find the best (lightest). We start at a random location (otherwise multiple searches would be too similar to be interesting) and we move a random (Pareto-distributed) distance at a random angle at each step. If the new location has a lower fitness than our current location, we don't bother moving; otherwise we finish the move to the new location. Like the standard levy flight example, the edges don't wrap around. Instead, we forbid steps that go outside the search space.

<div id="levyhill_playfield" style="width: 500px; height: 500px;"></div>
<form action="#" type="get">
<div>
  <input type="range" name="_" id="levyhill_number" min="0" max="10" value="0" style="width: 500px;" />
  <label for="levyhill_number">Number of searches:</label>&nbsp;&nbsp;<a id="levyhill_number_display"></a>
</div>
<div>
  <span>Current fitness:</label>&nbsp;&nbsp;<a id="levyhill_fitness_display"></a>
</div>
<div>
  <input type="range" name="_" id="levyhill_scale" min="1" max="3" value="1" style="width: 500px;" />
  <label for="levyhill_scale">Scale:</label>&nbsp;&nbsp;<a id="levyhill_scale_display"></a>
</div>
</form>
{$REQUIRE_JAVASCRIPT,javascript_jquery_svg}
{$REQUIRE_JAVASCRIPT,javascript_underscore}
{$REQUIRE_JAVASCRIPT,javascript_cedi_levyhill}

Moving the "Number of searches" slider will add and remove instances of the search. All of them are running the same code, but they start in random places and take random steps so their solutions will be different. The "Scale" is a parameter for the Pareto distribution which governs the probabilities of short and long steps. Like the random walk hill climber, we don't need a "Desired fitness" slider this time, since the search will stop once it can't find a better solution. We have no idea if the solution it finds will be the best one though; welcome to the [halting problem](http://en.wikipedia.org/wiki/Halting_problem)!

I haven't found an explicit example of hill climbing levy flights before (although I have seen random walk hill climbing, and levy flights used in more complex ways), so it is quite interesting to see how it performs. My instinct is that it beats hill climbing with random walks, but classifying it is difficult; it is certainly performing a global search of the whole space, since its possible steps can be at any angle and of any length; however, it is also performing a local search since it only considers a single point at a time (as opposed to population-based algorithms), and the higher it climbs on its current peak, the harder it has to try to find other peaks. Without a more in-depth analysis, it's hard to give advice about its use, but I would guess that it's worth using over random walk hill climbing, for those who don't want the complexity of a more sophisticated algorithm (like, for example, cuckoo search).

Since hill-climbing levy flights will never sacrifice their current fitness, which makes them get stuck, this does give them the nice property of being *anytime algorithms*, which means we can stop them at any point and be sure that we're given the best solution found so far. If the algorithm were to cross the valleys between the peaks (without jumping over them), then it wouldn't get stuck; however, we may end up stopping it while it's in the middle of a valley, and thus our final solution would be terrible.
