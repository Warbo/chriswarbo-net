---
title: Joining Parentheses
---

<small>Part of [my units pages](/projects/units)</small>

TODO: Maybe just draw boxes?

Parentheses are used to 'group together' parts of an expression; for example
$8 + ((3 + 2) \times (1 + 4))$. When many parentheses are nested, a common
problem is having to physically *count them* to figure out which ones match up;
or, in other words, to determine the extent of what's in them.

Prior to the use of parentheses, it was common to use an over-bar (called a
'vinculum') to express grouping. This would render the above like:
$8 + \overbar{\overbar{3 + 2} \times \overbar{1 + 4}}$

<div style="display: none;">

\newcommand{\overparens}[1]{\ensuremath{\bigl(\text{$\overline{\mathstrut\smash{\text{#1}}}$}\bigr)}}
\newcommand{\underparens}[1]{\ensuremath{\bigl(\text{$\underline{\mathstrut\smash{\text{#1}}}$}\bigr)}}

</div>

The vinculum makes its extent more clear, since it is physically present above
its entire contents. However, I still appreciate the way parentheses 'split up'
their interior from their exterior: we can combine both by using the vinculum to
'join the parentheses', like so: $\overparens{3 + 2} \times \overparens{1 + 4}$

Note that this may cause clutter when using
[overbars for negatives](negative_bar_notation.html), in which case it may be
preferable to join parentheses *below* their enclosed expression:
$\underparens{3 + 2} \times \underparens{1 + 4}$

When there's no overbar ambiguity, and we have heavily-nested parentheses, I
find it helpful to alternate between over- and under-bars at each level of
nesting. This original example above would thus become:

$$
8 + \overparens{\underparens{3 + 2} \times \underparens{1 + 4}}
$$

I use this extensively when working in combinatory logic (which makes extensive
use of nested tree structures!). For example, here's function composition:

$$
\overparens{\underparens{\overparens{C f} g} x}
  &= \overparens{\underparens{\overparens{\underparens{\overparens{S \underparens{K S}} K} f} g} x} \\
  &= \overparens{\underparens{\overparens{\underparens{\overparens{K S} f} \underparens{K f}} g} x} \\
  &= \overparens{\underparens{\overparens{S \underparens{K f}} g} x} \\
  &= \overparens{\underparens{\overparens{K f} x} \underparens{g x}} \\
  &= \overparens{f \underparens{g x}} \\
$$
