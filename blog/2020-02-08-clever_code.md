---
title: Clever Code
---

Here's another post taken from [a comment of mine on Hacker
News](https://news.ycombinator.com/item?id=16223583), reproduced for posterity.

The discussion was about code review, and trying to avoid (overly) "clever"
solutions:

> I look for code that is well-documented (both inline and externally), and code
> that is clear rather than clever. I’d rather read ten lines of
> verbose-but-understandable code than someone’s ninja-tastic one-liner that
> involves four nested ternaries.

"Clear" and "clever" aren't in opposition, and likewise "verbose" and
"understandable" aren't correlated.

I think this characterisation, and especially the example, shows a
lowest-common-denominator straw man of "clever one-liners" which seems to miss
the reason that some people like them. In particular, it seems to be
bikeshedding about how to write branches. The author doesn't say what those "ten
lines of verbose-but-understandable code" would be, but given the context I took
it to mean "exactly the same solution, but written with intermediate variables
or if/else blocks instead".

This seems like an analogous situation to
[Wadler's Law](https://wiki.haskell.org/Wadler's_Law) where little thought is
given to what the code means, more thought is given to how that meaning is
encoded (e.g. ternaries vs branches) and religious crusades are dedicated to
how those encodings are written down (tabs vs spaces, braces on same/new lines,
etc.).

Note that even in this simple example there lurks a slightly more important
issue which the author could have mentioned instead: nested ternaries involve
boolean expressions; every boolean expression can be rewritten in a number of
ways; some of those expressions are more clear and meaningful to a human than
others. For example, `loggedIn && !isAdmin` seems pretty clear to me; playing
around with truth tables, I found that `!(loggedIn -> isAdmin)` is apparently
equivalent, but it seems rather cryptic to me. This is more obvious if
intermediate variables are used, since they're easier to name if they're
meaningful.

In any case, compressing code by encoding the same thing with different symbols
doesn't make something "clever". It's a purely mechanical process which doesn't
involve any insights into the domain.

To me, code is "clever" if it works by exploiting some non-obvious
structure/pattern in the domain or system. For example, code which calculates a
particular index/offset in a non-obvious way, based on knowledge about
invariants in the data model. Another example would be using a language
construct in a way which is unusual to a human, but has the perfect semantics
for the desired behaviour (e.g. duff's device, exceptions for control flow,
etc.).

Such "clever" code is often more terse than a "straightforward" alternative, but
that's a side-effect of the "cleverness" (finding an existing thing which
behaves like the thing we want) rather than the goal.

If the alternative to some "clever" code is "10 lines of verbose but
understandable code" then it's probably not that clever; so it's probably a safe
bet to go with the latter. The real issues with clever code are:

 - Whether the pattern it relies on is robust or subject to change. Would it end
   up coupling components together, or complicate implementation changes in
   unrelated modules?

 - How hard it is to understand. Even if it's non-obvious, can it be understood
   after a moment's pondering; or does it require working through a textbook and
   several research papers?

 - Whether the insights it relies on are enlightening or incidental, i.e. the
   payoff gained from figuring it out. This is more important if it's harder to
   understand. Enlightening insights can change the way we understand the
   system/domain, which may have many benefits going forward. Incidental
   insights are one-off tricks that won't help us in the future.

 - How difficult it would be to replace; or whether it's possible to replace at
   all.

This last point is what annoys me in naïve "clever vs verbose" debates, and
prompted this rant, since it's often assumed that the only difference is line
count. To me, the best "clever" code isn't that which reduces its own line
count; it's the code which removes problems entirely; i.e. where the alternative
has caveats like "before calling, make sure to...", "you must manually free the
resulting...", "watch out for race conditions with...", etc.

One example which comes to mind is some Javascript I wrote to estimated prices
based on user-provided sliders and tick-boxes, and some formulas and constants
which sales could edit in our CMS (basically, I had to implement a spreadsheet
engine).

Recalculating after user input was pretty gnarly, since formulas could depend on
each other in arbitrary ways, resulting in infinite loops and undefined
variables when I tried to do it in a "straightforward" way. The "clever"
solution I came up with was to evaluate formulas and values lazily: wrapping
everything in thunks and using a memo table to turn exponential calculations
into linear ones. It was small, simple and heavily-commented; but the team's
unfamiliarity with concepts like lazy evaluation and memoising made it hard to
get through code review.

Also, regarding "straightforward" or "verbose" code being "readable": it's
certainly the case that any particular part of such code can be read and
understood locally, but it can make the overall behaviour harder to
understand. Just look at machine code: it's very verbose and straightforward:
'load address X into register A then add the value of register B', simple! Yet
it's very hard to understand the "big picture" of what's going on. Making code
more concise, either by simplifying it or at least abstracting away low-level,
nitty-gritty details into well-named functions, can help with this.

When used well, "clever" code can reframe problems into a form which have very
concise solutions; not because they've been code-golfed, but because there's so
little left to say. This can mean the difference between a comprehensible system
and a sprawling tangle of interfering patches. This may harm local reasoning in
the short term, since it requires the reader to view things from that new
perspective, when they may be expecting something else.

When used poorly, it results in things like nested ternaries, chasing
conciseness without offering any deeper understanding of anything.
