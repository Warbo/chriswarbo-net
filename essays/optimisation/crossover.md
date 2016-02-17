---
title: Crossover Genetic Algorithm
---
A genetic algorithm differs from the simpler search techniques (random walks, hill climbing, etc.) because it is *population-based*. Rather than moving from one solution to another solution, a genetic algorithm keeps track of multiple solutions at the same time and uses this combination to cover more of the search space.

A genetic algorithm, as its name suggests, uses the idea of genetics from biology. In the natural world, life is governed by the environment, where the problem to solve is how to live long enough to reproduce effectively. The solutions to this problem are organisms, which are defined by their genes. Each gene makes up part of the solution, and they combine together in arbitrarily complicated ways to make each organism.

Although genes are safely tucked away in the nuclei of cells, they are implicitly competing with each other; this is because the most dangerous force in the environment is usually other organisms (large or microscopic). This, in *incredibly* loose terms, is survival of the "fittest".*

In a genetic algorithm, these ideas are implemented literally in the code: full solutions are made out of a collection of partial solutions; the worst solutions are constantly being discarded, whilst those that remain are used as templates for new solutions. This exactly mirrors an idealised model of genetics, however the fitness is specified explicitly which directs the solutions to a desirable area.

In the genetic algorithm on this page, I've only included "crossover", which is the random combination of 2 parents into a pair of children, making a crude model of sexual reproduction. To start the simulation, click on the playfield (the box showing the greyscale fitness function).

<div id="crossover_playfield" style="width: 500px; height: 500px;"></div>
<form action="#" type="get">
 <div>
  <input type="range" min="1" max="100" value="20" id="crossover_population" style="width: 500px;" />
  <label for="crossover_population">Population:&nbsp;</label><a href="#" id="crossover_population_display"></a>
 </div>
 <div>
  <input type="range" min="1" max="10" id="crossover_rate" value="1" style="width: 500px;" />
  <label for="crossover_rate">Crossover rate:&nbsp;</label><a href="#" id="crossover_rate_display"></a>
 </div>
 <div>
  Current fitness:&nbsp;<a href="#" id="crossover_fitness"></a>
 </div>
 <div>
  <input type="checkbox" id="crossover_stable" value="0" /><label for="crossover_stable">Stable sort. Slower, but stops the green blob jumping between equal-fitness solutions.</label>
 </div>
</form>
<script src="/js/jquery.js"></script>
<script src="/js/jquery_svg.js"></script>
<script src="/js/underscore.js"></script>
<script src="/js/optimisation/crossover.js"></script>

In the example above, the same greyscale fitness landscape is used as in the individual (ie. non-population-based) search algorithms. Since our genetic algorithm contains a population of many competing solutions, there's no advantage to running multiple instances at the same time. Instead, we have a population slider which tells us how many solutions are kept in the population at any one time.

Warning: This algorithm is incredibly resource-hungry; increasing the population will make it run much slower! The slowest part of the algorithm is drawing the SVG graphics, so for this reason only 5 solutions are shown at once. We take the fittest (green), the least fit (red) and one from 25% (red), 50% (orange) and 75% (orange) inbetween. As with all of these algorithms, you're free to do what you want with the code. However, I'll warn you now that I've had to optimise this quite a bit to make it run at an acceptable speed; this has made the code uglier than I'd like.

The way I've split the solutions ((x, y) points) into genes is to write out each number in binary and treat each bit (0 or 1) as a gene. Thus the point (100, 400), in binary, is (00110010, 11001000). New solutions, as mentioned above, come in pairs. We take a pair of existing solutions and duplicate them to make the offspring; we then take these offspring and, starting at the most-siginificant bit, choose some "crossover points" (the number being determined by the "crossover rate" slider). Anything on the less-significant-side of a crossover point is swapped between the children. For example if we had parents 0000000000000000 and 1111111111111111 with 1 crossover point at position 6 (chosen randomly) then we would get children 0000001111111111 and 1111110000000000.

It is interesting to compare crossover with mutation. Whilst mutation seems like a series of modest changes, maybe a few bits at a time maximum, crossover is a complete chopshop affair that can end up affecting large numbers of bits/genes at once. However, crossover is more subtle than mutation because its behaviour varies as the population evolves. When there is a large amount of diversity in the population, which usually happens early on, crossover does indeed affect large sections of the new solutions. Many new solutions will be formed that are very different to their parents. However, as the population starts to converge around a particularly fit section of the search space, the diversity of the population goes down. Now each crossover affects the children much less, since the parents will be more similar and thus swapping sections of them might not do much (or anything at all, in the case of a population dominated by a single solution!). It is this refinement that makes crossover powerful. With mutation, we must wait for favourable mutations, which become very unlikely very quickly. With crossover, it is likely that nearby solutions each have a different part of the solution correct (eg. they may be on either side of the peak), in which case a crossover operation might well join these individually-perfect sections together into a perfect solution.

To choose which existing solutions will be the parents of the next new solution I've used a simple probability distribution. We sort the solutions by fitness, and make the probability of selecting the worst solution zero. The probability of selecting the best solution is 2/population, and the rest follow a straight line y=2x/population^2. This gives us some nice properties like a 75% chance that each parent will be in the best 50%. At each step, we replace the least-fit quarter of the population with new solutions. We start with uniformly random solutions.

Crossover-based genetic algorithms are good at exploring large search spaces effectively, and do a better job of refining nearly-optimal solutions that mutation does. However, a major problem with crossover is that it never contributes any new genes to the algorithm. For example, if every random solution has a 0 for its first x bit, the algorithm will never be able to explore the right (nearly-)half of the search space.

In my opinion, crossover-only genetic algorithms can do a decent job of rapidly turning a poor population into a great one, provided that they can access most of the search space. This last provision means that you'll always want to use something in addition to just crossover, in order to keep all of the search space available to the algorithm.

 * The reason I'm apprehensive to use this phrase is that the scientific term "fittest" comes with all kinds of dangerous linguistic baggage. Eugenics and mass murder have been committed in the name of weeding out the "least fit"; however this misses the point entirely, which is that survival *defines* the fittest. In nature, the only goal is to stay around for as long as possible, either as an individual or, more commonly, via reproduction. The goal is implied; you don't have to bother surviving, but those that didn't aren't around any more. Crucially, how you manage to survive is completely irrelevant. For example the plump, flightless, slow, clumsy chicken is the most successful bird on the planet; there are more chickens than there are any other bird. In fact, the chicken is the most successful dinosaur there's ever been. Their prospects look good too; human beings won't let them die out any time soon, and unless they turn out to be specifically unsuited for some reason, it's a pretty sure bet that once we colonise other planets the chicken won't be far behind. Of course chickens will never attain the unbelievably grand level of fitness enjoyed by bacteria, who's space colonies already boast far greater populations than there are humans on Earth.
