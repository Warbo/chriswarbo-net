---
title: Small Functions
---

I saw
[Small functions considered harmful](https://medium.com/@cindysridharan/small-functions-considered-harmful-91035d316c29)
on [Hacker News](https://news.ycombinator.com/item?id=14988206) today, and
wanted to comment but it got a little long, so I've posted it here ;)

As a lot of other commenters point out, too much of anything is a bad thing (by
definition!).

Whilst reading the article, a few things bubbled up in my subconscious (NOTE:
the below "quotes" are paraphrased, but hopefully not misrepresenting the
arguments):

> Splitting functionality over many small functions makes it harder to read/see
> what's happening

If you find yourself wanting/needing to read a whole bunch of definitions at
once, then *that* is the problem that needs solving; not necessarily the fact
that so many functions are used. This can be partially due to the code (i.e.
when it *really does* require digging down a few levels to understand what's
going on), but it can also be due to the developer or team psychology
(i.e. subconsciously doubting that `doFoo` actually does 'foo').

The latter might be related to the difficulty many people have with recursion,
since that also requires the ability to reason *under the assumption that* some
"black box" function call works as expected. For example, `mergesort` is trivial
to understand *under the assumption that* `mergesort(firstHalf)` and
`mergesort(secondHalf)` work as advertised; without that suspension of
disbelief, it's utter voodoo.

Note that as a code smell (rather than an overly distrustful dev), this might be
due to *not enough* abstraction, as well as too much or misaligned abstractions
like the article mentioned. For example, I might open a file at random and pick
a random function, say `sendOrder`: if that contains a bunch of
number-twiddling, deletes some fields, rearranges some lists, etc. then I might
doubt its correctness, since I have no idea why those things have anything to do
with sending orders; on the other hand, if that stuff were pulled out into a
`formatForInitech` function then I might find it reasonable that sending an
order requires formatting it for initech, and if I stumbled upon the
`formatForInitech` function at random then I might find it reasonable that such
formatting requires deleting fields, twiddling numbers, etc. Note that this
could also be achieved by adding a comment, but I don't think that changes the
argument: if I see `// Required to comply with Initech systems` followed by some
gobbledegook, I should (be able to) have confidence that the code is doing what
the comment says (and no more).

This confidence in randomy-chosen functions isn't just a nice-to-have property
of a codebase either: when we're debugging we *know* there's a problem
somewhere, and it saves time and mental capacity if we can skim over unrelated
things when doing a deep dive into the problematic area.

Another problem with *lack* of abstraction, which can force us to dig into
function definitions, is when our API is too fine-grained: it allows things
which don't make sense in the domain, and hence places a burden on us to avoid
such incoherent states. If we try sticking to the "language keyword" level, it's
very easy to end up in such situations. For example, a common (anti?) pattern is
an "iterator" object, with methods like `hasNext` and `next`. This lets us use
our language's built-in `for` loops, but it couples control flow to
otherwise-meaningless state (the "cursor position" of our iterator) whose leaks
can cause problems like composition-breaking interference (if some inner call
tries to loop over the same iterator). Much better to provide small functions
like `map` and `filter` (or, even better, domain-specific alternatives), which
are "unfamiliar" compared to keywords but which avoid inconsistent states and
make sense in the domain.

If functions are "invisibly" coupled to each other, e.g. `doBar(...)` assumes
that `doFoo(...)` has been called beforehand, then I'd say these may
legitimately be "too small"; specifically, we might say they aren't "doing one
thing", they're actually doing "half a thing": again, the API is allowing too
many things (i.e. "barring in an unfooed state"). A common example is
`dbQuery(...)` requiring `dbConnect(...)` to be run first.

Inlining isn't the only, or necessarily best, solution though. If a "fooed
state" makes sense in the problem domain, e.g. "a connected database", then
another solution is to have `doFoo` *return* the relevant state and have `doBar`
consume it, so we get `doBar(..., doFoo(...))`. This is especially useful if we
might want to re-use the same result (e.g. the same DB connection) many
times. We can introduce nice types to ensure correct usage too,
e.g.

    dbConnect : DbCredentials -> Database

    dbQuery : Database -> Sql -> Table

This way there's no chance to attempt a query without having connected.

Alternatively, if the "intermediate state" doesn't make sense in our domain and
is only an implementation detail, then another solution is to have a `withFoo`
function, which takes a function as argument and runs it in the correct state,
e.g.

    withFoo = function(f) { doFoo(); return f(); };

This came to mind when I saw `renderPageWithSetupAndTeardown`: I agree that it
seems rather pungent. One possible alternative would be to have the `setup` and
`teardown` functions be (optional?) fields of a `page`, which the regular
`renderPage` function would call if found. Alternatively, we could pull out a
function:

    withSetupAndTeardown = function(setup, teardown, f) {
                             result = f(setup());
                             teardown(result);
                             return result;
                           };

This can be used with anything, including the regular `renderPage` function
(this is analogous to the widely-used `bracket` function in Haskell, for
example).

I think that this specific `renderPageWithSetupAndTeardown` example is actually
quite weak, since it smells to me of coming from a language without first-class
functions, which is going to make any proposed solutions overly convoluted
(since "setup" and "teardown" are inherently function-like concepts), and hence
there are many ways to improve it in languages *with* first-class functions
(like Ruby, or whatever).
