---
title: Mutating Genetic Algorithm
---
A genetic algorithm differs from the simpler search techniques (random walks, hill climbing, etc.) because it is *population-based*. Rather than moving from solution to solution, a genetic algorithm keeps track of multiple solutions at the same time and uses this combination to cover more of the search space.

A genetic algorithm, as its name suggests, uses the idea of genetics from biology. In the natural world, life is governed by the environment, where the problem to solve is how to live long enough to reproduce effectively. The solutions to this problem are organisms, which are defined by their genes. Each gene makes up part of the solution, and they combine together in arbitrarily complicated ways to make each organism.

Although genes are safely tucked away in the nuclei of cells, they are implicitly competing with each other; this is because the most dangerous force in the environment is usually other organisms (large or microscopic). This, in *incredibly* loose terms, is survival of the "fittest".*

In a genetic algorithm, these ideas are implemented literally in the code: full solutions are made out of a collection of partial solutions; the worst solutions are constantly being discarded, whilst those that remain are used as templates for new solutions. This exactly mirrors an idealised model of genetics, however the fitness is specified explicitly which directs the solutions to a desirable area.

In the genetic algorithm on this page, I've only included random genetic mutation; this makes a crude model of asexual reproduction, where offspring are imperfect copies of a single parent. To start the simulation, click on the playfield (the box showing the greyscale fitness function).

<div id="mutate_playfield" style="width: 500px; height: 500px;"></div>
<form action="#" type="get">
  <div>
    <input type="range" min="1" max="100" value="20" id="mutate_population" style="width: 500px;" />
    <label for="mutate_population">Population:&nbsp;</label><a id="mutate_population_display"></a>
  </div>
  <div>
    <input type="range" min="1" max="10" id="mutate_rate" value="1" style="width: 500px;" />
    <label for="mutate_rate">Mutation rate:&nbsp;</label><a id="mutate_rate_display"></a>
  </div>
  <div>
    Current fitness:&nbsp<a id="mutate_fitness"></a>
  </div>
  <div>
    <input type="checkbox" id="mutate_stable" value="0" /><label for="mutate_stable">Stable sort. Slower, but stops the green blob jumping between equal-fitness solutions.</label>
  </div>
</form>
<script src="/js/jquery.js"></script>
<script src="/js/jquery_svg.js"></script>
<script src="/js/underscore.js"></script>
<script src="/js/optimisation/mutate.js"></script>

In the example above, the same greyscale fitness landscape is used as in the individual (ie. non-population-based) search algorithms. Since our genetic algorithm contains a population of many competing solutions, there's no advantage to running multiple instances at the same time. Instead, we have a population slider which tells us how many solutions are kept in the population at any one time.

Warning: This algorithm is incredibly resource-hungry; increasing the population will make it run much slower! The slowest part of the algorithm is drawing the SVG graphics, so for this reason only 5 solutions are shown at once. We take the fittest (green), the least fit (red) and one from 25% (red), 50% (orange) and 75% (orange) inbetween. As with all of these algorithms, you're free to do what you want with the code. However, I'll warn you now that I've had to optimise this quite a bit to make it run at an acceptable speed; this has made the code uglier than I'd like.

The way I've split the solutions ((x, y) points) into genes is to write out each number in binary and treat each bit (0 or 1) as a gene. Thus the point (100, 400), in binary, is (00110010, 11001000). Each time a new solution is made, we take the binary numbers from the parent and flip each bit (0->1, 1->0) with a probability of r/16 where r is the mutation rate. We use 16 because x and y are 8 bits each, so r is effectively the number of mutations per solution, on average.

Large amounts of mutation benefit the algorithm by diversifying the population (ie. exploring more of the search space). Small amounts of mutation benefit the algorithm by concentrating the search around the fittest known solutions. The quality of the genetic algorithm's search relies on balancing these 2 forces: too little mutation and the population soon gets dominated by the fittest solution found so far, which isn't particularly good because it hasn't had time to explore (this is exactly the same problem as hill-climbing getting stuck at a nearby peak rather than a good peak). Too much mutation and the population becomes essentially random, which loses the key feature that makes the algorithm home in on good solutions: concentrating the search around the best results found so far.

To choose which existing solution will be the parent of the next new solution I've used a simple probability distribution. We sort the solutions by fitness, and make the probability of selecting the worst solution zero. The probability of selecting the best solution is 2/population, and the rest follow a straight line y=2x/population^2. This gives us some nice properties like a 75% chance that a new solution will be descended from the best 50%. At each step, we replace the least-fit quarter of the population with new solutions. We start with uniformly random solutions.

Mutation-based genetic algorithms are good at exploring large search spaces effectively, but their effectiveness goes down rapidly over time. Given a poor starting population (like the randomly chosen one in this simulation), this algorithm will improve it rapidly. However, given a somewhat decent population it will take a very long time to turn this into a very good population. This is because we are randomly changing bits of the solutions. Given a solution that's mostly bad, there's a pretty good probability that causing a random mutation will make it better. Given a solution that's mostly good, a random mutation will probably make it worse. This is actually a case of the Second Law of Thermodynamics, where random changes tend to cause an 'even' outcome (not very good, but not very bad). In fact, there's likely a strong bias towards bad solutions, since there will usually be far more of them than good solutions (otherwise it's not worth using such a sophisticated search algorithm, just pick random solutions until you get a good one).

A good analogy is shaking a box of coins, where the goal is to make them all heads. If they're mostly tails to begin with, then giving the box a good hard shake will tend to flip them closer to 50/50, which improves the number of heads. If they're mostly heads to begin with then such a shake will be more likely to flip some of the abundant heads into tails than to flip some of the rare tails into heads.

This problem might seem surmountable in this case, since a solution that's 1 bit away from perfect only needs one mutation to become perfect, which will work 1 out of every 16 times. Since we discard the least-fit solutions, this means we only need to run about 16 steps (assuming 1 mutation per step) to get our perfect solution. However, genetic algorithms have been applied successfully to problems with a billion bits. Waiting for a one-in-a-billion successful mutation isn't a viable search strategy in such cases.

In my opinion, mutation-only genetic algorithms aren't worth using. If you're going to go to the effort of implementing all of the required population management for a search algorithm like this then you may as well go a bit further and use a more sophisticated technique than just random mutation.

 * The reason I'm apprehensive to use this phrase is that the scientific term "fittest" comes with all kinds of dangerous linguistic baggage. Eugenics and mass murder have been committed in the name of weeding out the "least fit"; however this misses the point entirely, which is that survival [i]defines[/i] the fittest. In nature, the only goal is to stay around for as long as possible, either as an individual or, more commonly, via reproduction. The goal is implied; you don't have to bother surviving, but those that didn't aren't around any more. Crucially, how you manage to survive is completely irrelevant. For example the plump, flightless, slow, clumsy chicken is the most successful bird on the planet; there are more chickens than there are any other bird. In fact, the chicken is the most successful dinosaur there's ever been. Their prospects look good too; human beings won't let them die out any time soon, and unless they turn out to be specifically unsuited for some reason, it's a pretty sure bet that once we colonise other planets the chicken won't be far behind. Of course chickens will never attain the unbelievably grand level of fitness enjoyed by bacteria, who's space colonies already boast far greater populations than there are humans on Earth.
