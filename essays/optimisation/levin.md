---
title: Levin Search
---
Levin Search is a specific type of enumeration, which has nice theoretical properties for real-world problems.

In this example, the fitness of a particular location is shown by how light the grey colour is. This is randomly generated when the page loads.

Each (x, y) point in the square is a solution, and our goal is to find the best (lightest). Levin Search is based on Algorithmic Information Theory, which says we should enumerate *programs* rather than coordinates. By converting the output of each program into a coordinate, we are enumerating solutions in order of (Kolmogorov) complexity, starting with the simplest. Unfortunately, due to the Halting Problem, we can't actually perform this enumeration directly, since some programs may never halt. Instead we use an approximation known as Levin complexity, where we kill programs after a number of steps, and we keep doubling the cutoff as the search progresses. Levin Search thus finds the fittest, simplest solution, based on Levin's complexity measure (program length + log2(runtime)).

Here our programs are written in a language known as BitBitJump. This is a very simple language, with only one instruction. Programs in BitBitJump keep shuffling around the contents of an array of memory, using addresses of a fixed size (the word size, or machine size). Since these address size is fixed, the BitBitJump machine can only use a fixed amount of memory, which prevents it from being Turing Complete. We get around this by incorporating machine size increases into our Levin Search, once we hit a program that won't fit in the current machine's memory.

The programs themselves are simply numbers, which we count up from 0, 1, 2, etc. We write out our current program's number in binary, and dump this into the BitBitJump machine's memory. If the "fill memory" box is ticked, we will repeat this binary pattern over and over to fill up the memory. If the box isn't ticked, we simply initialise the rest of the memory to zeroes. The only difference is that the early patterns are less boring when we fill the memory, as the machine has something to work with. One other consideration is that we enter the program in backwards. We must do this to counteract an artifact of our writing system: we don't bother putting leading zeros on our numbers (eg. we write "100" rather than, say, "0000100"); this causes our first bit to be 1 for every program greater than 0, unless we enter it backwards. For instance, if our program is 11, then in binary this is "1011", so our memory will be "110100000...." when the "fill" box is unticked, and "1101110111011101..." when it is ticked.

Levin Search will always find a solution if one exists, since it will eventually evaluate any solution for any number of steps. The first solution found will be the "simplest" according to Levin's complexity measure, which in our case is the length of the solution plus the logarithm of the number of steps it takes to reach the desired result. Note that this is simply the current 'phase' (outer loop).

<div id="levin_playfield" style="width: 500px; height: 500px;"></div>
<form action="#" type="get">
<div>
  Best fitness so far: <a id="levin_fitness_display"></a>
</div>
<div>
  Fittest program: <a id="levin_winner"></a>
</div>
<div>
  Current phase: <a id="levin_phase">0</a>
</div>
<div>
  Current machine size: <a id="levin_m">1</a>
</div>
<div>
  Current enumeration: <a id="levin_this_enum">0</a> (Max so far: <a id="levin_enum">0</a>)
</div>
<div>
  <label for="#fill">Fill memory?</label><input type="checkbox" id="fill" checked="checked" />
</form>
{$REQUIRE_JAVASCRIPT,javascript_jquery_svg}
{$REQUIRE_JAVASCRIPT,javascript_underscore}
{$REQUIRE_JAVASCRIPT,javascript_cedi_levin}
{$REQUIRE_JAVASCRIPT,javascript_cedi_zot}

Click the square above to start the search. Since it's completely deterministic, there's no point running more than one instance at a time. The "Best fitness so far" slider tells us what the search has found so far.

Whilst Levin Search is very simple and has nice theoretical properties, its naivety makes it incredibly inefficient for all but the simplest problems.
