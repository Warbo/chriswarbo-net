---
title: Crossover and Mutating Genetic Algorithm
---
A genetic algorithm differs from the simpler search techniques (random walks, hill climbing, etc.) because it is *population-based*. Rather than moving from one solution to another solution, a genetic algorithm keeps track of multiple solutions at the same time and uses this combination to cover more of the search space.

A genetic algorithm, as its name suggests, uses the idea of genetics from biology. In the natural world, life is governed by the environment, where the problem to solve is how to live long enough to reproduce effectively. The solutions to this problem are organisms, which are defined by their genes. Each gene makes up part of the solution, and they combine together in arbitrarily complicated ways to make each organism.

Although genes are safely tucked away in the nuclei of cells, they are implicitly competing with each other; this is because the most dangerous force in the environment is usually other organisms (large or microscopic). This, in *incredibly* loose terms, is survival of the "fittest".*

In a genetic algorithm, these ideas are implemented literally in the code: full solutions are made out of a collection of partial solutions; the worst solutions are constantly being discarded, whilst those that remain are used as templates for new solutions. This exactly mirrors an idealised model of genetics, however the fitness is specified explicitly which directs the solutions to a desirable area.

In the genetic algorithm on this page, I've included the two commonly used solution generators: "mutation" and "crossover". Mutation randomly changes some of the genes, and is a good source of diversity for the algorithm. Crossover chops up two solutions and swaps sections between them, which allows new solutions to be formed without throwing away the genes that have evolved so far (they just get recombined in different patterns). Together these rules make a more sophisticated model of sexual reproduction than crossover alone. To start the simulation, click on the playfield (the box showing the greyscale fitness function).

<div id="mutatecross_playfield" style="width: 500px; height: 500px;">
</div>

<form action="#" method="get">
 <div>
  <input type="range" min="1" max="100" value="20" id="mutatecross_population" style="width: 500px;" />
  <label for="mutatecross_population">Population:&nbsp;</label><a href="#" id="mutatecross_population_display"></a>
 </div>
 <div>
  <input type="range" min="1" max="10" id="mutatecross_cross_rate" value="1" style="width: 500px;" />
  <label for="mutatecross_cross_rate">Crossover rate:&nbsp;</label><a href="#" id="mutatecross_cross_rate_display"></a>
 </div>
 <div>
  <input type="range" min="2" max="50" id="mutatecross_mutate_rate" value="16" style="width: 500px;" />
  <label for="mutatecross_mutate_rate">Mutation factor (lower = more mutation):&nbsp;</label><a href="#" id="mutatecross_mutate_rate_display"></a>
 </div>
 <div>
  Current fitness:&nbsp;<a href="#" id="mutatecross_fitness"></a>
 </div>
 <div>
  <input type="checkbox" id="mutatecross_stable" value="0" /><label for="mutatecross_stable">Stable sort. Slower, but stops the green blob jumping between equal-fitness solutions.</label>
 </div>
</form>

<script src="/js/jquery.js">
</script>

<script src="/js/jquery_svg.js">
</script>

<script src="/js/underscore.js">
</script>

<script src="/js/optimisation/mutatecross.js">
</script>

In the example above, the same greyscale fitness landscape is used as in the individual (ie. non-population-based) search algorithms. Since our genetic algorithm contains a population of many competing solutions, there's no advantage to running multiple instances at the same time. Instead, we have a population slider which tells us how many solutions are kept in the population at any one time.

Warning: This algorithm is incredibly resource-hungry; increasing the population will make it run much slower! The slowest part of the algorithm is drawing the SVG graphics, so for this reason only 5 solutions are shown at once. We take the fittest (green), the least fit (red) and one from 25% (red), 50% (orange) and 75% (orange) inbetween. As with all of these algorithms, you're free to do what you want with the code. However, I'll warn you now that I've had to optimise this quite a bit to make it run at an acceptable speed; this has made the code uglier than I'd like.

The way I've split the solutions ((x, y) points) into genes is to write out each number in binary and treat each bit (0 or 1) as a gene. Thus the point (100, 400), in binary, is (00110010, 11001000). New solutions come in pairs. We take a pair of existing solutions (parents) and duplicate them to make the offspring; we then take these offspring and, starting at the most-siginificant bit, choose some "crossover points" (the number being determined by the "crossover rate" slider). Anything on the less-significant-side of a crossover point is swapped between the children. For example if we had parents 0000000000000000 and 1111111111111111 with 1 crossover point at position 6 (chosen randomly) then we would get children 0000001111111111 and 1111110000000000. After this, we flip each bit with a probability of 1/mutation rate (note: the higher the value, the less probable the mutations).

By combining crossover and mutation, we get the best of both worlds. Crossover is the dominant strategy, since it shifts large chunks of genomes around. However, the problem with crossover is that no new genes ever enter the simulation. Since we kill off weak solutions at each step, this lowers the overall availability of genes that the algorithm can use over time. This causes crossover to converge on solutions not only because they are good, but also because the diversity of the population is 'ratcheted': it can only ever go down, and the best result (as far as diversity is concerned) is that it remains the same. This limitation is mitigated by mutation. Whilst mutation is destructive, and more likely to cause harm than good, it ensures that crossover has a constant supply of new possibilities to try and exploit. For this reason, the mutation rate in a genetic algorithm with crossover should generally be low (to allow the crossover algorithm to work with each mutation as it arises).

To choose which existing solutions will be the parents of the next new solutions I've used a simple probability distribution. We sort the solutions by fitness, and make the probability of selecting the worst solution zero. The probability of selecting the best solution is 2/population, and the rest follow a straight line y=2x/population^2. This gives us some nice properties like a 75% chance that each parent will be in the best 50%. At each step, we replace the least-fit quarter of the population with new solutions. We start with uniformly random solutions.

Genetic algorithms with crossover and mutation are the archetypal genetic algorithm, and give good solutions to a wide range of problems. They are so effective that they have seen use in a [huge variety of areas](http://en.wikipedia.org/wiki/Genetic_algorithm#Problem_domains) throughout many fields. Genetic algorithms are often used as a fallback when an optimal solution can't be derived by a person, however I think that it should be perfectly acceptable to treat genetic algorithms as the default approach to any problem, and that we should only bother attempting to solve problems when the GA solution isn't giving acceptable results. A hindrance to this kind of approach is that there's no real support for such generic techniques as genetic algorithms in languages. There are libraries, but any such usage will be clunky at best. Thus, most genetic algorithms are custom written, by hand. This produces a lot of overhead, since you're essentially putting in a lot of effort to make a system which might give an acceptable answer, rather than putting in a lot of effort to get an excellent answer directly.

I think this is a symptom of stagnation in mainstream languages in general. For example, most languages have boolean values and if-then-else statements, but few make fuzzy logic or bayesian reasoning as elegant.

 * The reason I'm apprehensive to use this phrase is that the scientific term "fittest" comes with all kinds of dangerous linguistic baggage. Eugenics and mass murder have been committed in the name of weeding out the "least fit"; however this misses the point entirely, which is that survival *defines* the fittest. In nature, the only goal is to stay around for as long as possible, either as an individual or, more commonly, via reproduction. The goal is implied; you don't have to bother surviving, but those that didn't aren't around any more. Crucially, how you manage to survive is completely irrelevant. For example the plump, flightless, slow, clumsy chicken is the most successful bird on the planet; there are more chickens than there are any other bird. In fact, the chicken is the most successful dinosaur there's ever been. Their prospects look good too; human beings won't let them die out any time soon, and unless they turn out to be specifically unsuited for some reason, it's a pretty sure bet that once we colonise other planets the chicken won't be far behind. Of course chickens will never attain the unbelievably grand level of fitness enjoyed by bacteria, who's space colonies already boast far greater populations than there are humans on Earth.
