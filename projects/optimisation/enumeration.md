---
title: Enumeration
---
Enumeration is a very simple search algorithm that just puts every solution in some (total) order, checks the first one, then the second, then the third, and so on until it finds a solution with a good enough fitness.

In this example, the fitness of a particular location is shown by how light the grey colour is. This is randomly generated when the page loads.

Each (x, y) point in the square is a solution, and our goal is to find the best (lightest). To enumerate 2D points we need to interleave the x and y values somehow. There are a few different ways we could do it, but in this case we use the following algorithm:

```
denominator := 2
x_numerator := 1
y_numerator := 1
loop forever:
    try_solution((x_numerator / denominator, y_numerator / denominator))
    y_numerator := y_numerator + 2
    if y_numerator > denominator:
        y_numerator := 1
        x_numerator := x_numerator + 2
    if x_numerator > denominator:
        x_numerator := 1
        denominator := denominator * 2
end loop
```

This basically takes all of the odd coordinates, like (1, 1), (1, 3), (3, 1), etc., divided by increasing powers of 2 (2, 4, 8, 16, etc.). This way we never hit the same point twice; we end up missing a lot of solutions with this method, but we will always hit a solution nearby (for any finite value of "nearby").

<div id="enum_playfield" style="width: 500px; height: 500px;"></div>
<span>Fittest so far: <a href="#" id="enum_fitness_display">-1</a></span>

<script src="/js/jquery.js"></script>
<script src="/js/jquery_svg.js"></script>
<script src="/js/underscore.js"></script>
<script src="/js/optimisation/enum.js"></script>

Clicking the square above will start the search.

Whilst enumeration is a very simple search algorithm, its results can be pretty poor. It requires some prior knowledge of the search space in order to choose a reasonable enumeration order, otherwise the best solutions may so long time to find that it's not worth using ;)
