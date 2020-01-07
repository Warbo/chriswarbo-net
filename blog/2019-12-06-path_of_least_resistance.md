---
title: Paths of Least Resistance
---

> There are two types of programming languages: those which people complain
> about, and those which nobody uses.

My previous boss used to trot out this line whenever I bemoaned some failing of
the technologies we were using (which almost always meant PHP doing something
braindead). This is an example of a [thought-terminating cliche](
https://en.wikipedia.org/wiki/Thought-terminating_clich%C3%A9): a pithy way to
end a discussion with a whiff of resolution, without actually addressing any
point or preventing the problem arising again. I've seen this same "apologist"
attitude come up a few times in online discussions too, so I thought it was time
I wrote the sort of rebuttal I think it deserves. I apologise for the verbosity
of this response; it is unfortunate that, by design, the densely-packed nonsense
implicit in a thought-terminating cliche can take a lot of work to extract,
expose and debunk.

---

Like many things in life, everything in programming has problems; that *doesn't*
mean everything is equally problematic! In particular, I've become wary of
approaches which are fraught with:

 - Gotchas: These are known, acknowledged breakages in functionality or
   abstraction, which seemingly come out of nowhere for the uninitiated
   (violating the principle of least surprise). Those with more experience tend
   to instinctively code defensively to avoid them, obfuscating otherwise-good
   code for the fear of triggering one of these situations.
 - Misaligned Incentives: Where following the "correct" practice is directly in
   conflict with some other objective. For example, if the "correct" solution is
   more verbose, slower, harder to debug, harder to read, less modular, etc.
 - Lack of Objectively Checkable Criteria: Where there's no way to agree if
   something is done "correctly" or not. This makes it hard for learners to
   guess what the "correct" approach calls for in any given situation; and also
   allows goalposts to be moved after the fact, to denounce genuine attempts to
   follow the practice as "not done *properly*" if they turn out to make things
   worse.

This can be summarised as fixing *the path of least resistance*. Here are some
examples of practices which are *all* flawed, but where some are less prone to
the above than the alternatives.

# Static Typing #

Most static typing gotchas are conservative, i.e. some code isn't allowed even
though it *might* be correct. This is preferable to allowing broken code, as
long as it's not too burdensome to pass the type-checker. The widespread use of
statically typed languages (e.g. Java and C#) shows that it need not be too
burdensome. Examples of gotchas are [Haskell's "monomorphism restriction"](
https://wiki.haskell.org/Monomorphism_restriction) and [Java's lack of
multiple-inheritance](
https://stackoverflow.com/questions/52620936/why-does-java-not-allow-multiple-inheritance-but-does-allow-conforming-to-multip);
in both of these cases it's *possible* to cause a problem, so the type checker
forbids it (even though it *might* be fine).

For 'incentives', type-checkers don't have any of their own, but they're a
mechanism to bring the programmer's incentives ("get this code to compile") into
alignment with those of library designers ("make sure users call things
correctly"). Hence they're the *opposite* of misaligned incentives. An example
is string concatenation, which is the easiest way to dynamically create URLs,
HTML pages, SQL queries, shell commands, etc. but is vulnerable to injection
attacks. If a library/framework makes each of these a different type then this
easy-but-vulnerable approach is no longer possible; if escaping
functions/methods are the only way to convert a string from one language to
another, then the easiest way to combine strings is to escape them
appropriately, hence bringing the user's incentive ("do the easiest thing that
compiles") into alignment with the designer's ("prevent vulnerabilities").
Another example is sequential coupling, where one function/method, like
`dbConnect`, must be called before others, like `dbQuery`. The easiest thing is
to just call `dbQuery`, which won't work; the designer can forbid this misuse by
having that function require a `ConnectedDB`, and have `dbConnect` be the only
way to obtain one. Again, the easiest thing for the user to do (in order for the
compiler to succeed) is to use the library correctly. There are many other
examples of this sort of thing: in general, we *cannot* necessarily increase the
safety or correctness of the easiest approach to a problem; but we *can* use
types to easily *forbid* such easy "solutions", forcing the use of more
"correct" approaches. Overall this can make life more annoying, but it increases
the safety of the path of least resistance.

Regarding objective checkability, static types are not only objective, but also
*automatically* checkable, since that's what type-checkers are all about. It's
less trivial to know whether a particular *choice* of types is objectively
"good", but there *are* some general criteria, e.g. "invalid states should be
unrepresentable". Relatedly, we can objectively (and *sometimes* automatically)
check whether all cases have been handled, which gives an indication of whether
our types are a good fit for the problem (e.g. if we're passing around JSON data
using strings, there will be lots of boilerplate and/or unhandled cases for what
to do when given a non-JSON string; using a more precise type would reduce
this, indicating that it's a better fit for the problem). Note that these are
general rules, not vague sometimes-"proper"-sometimes-not heuristics.

# Automated Testing #

Testing has both overly-conservative and overly-liberal gotchas.
Overly-conservative examples are things like depending too heavily on
implementation details (e.g. checking if one list equals another, when their
order doesn't actually matter) or testing invalid situations (e.g. generating
test data which doesn't satisfy some required invariant). Like static typing,
these will forbid some correct code, making some tasks harder than
necessary. Overly-liberal examples are things like only testing the happy path,
or forgetting some edge case or space of inputs (e.g. negative numbers), or
failing to test the right code (e.g. mocking the thing we want to test). These
allow easy-but-broken code through, which might otherwise be caught. This is a
real problem with automated testing, and one reason why it's not a silver
bullet.

Automated testing also has misaligned incentives, if the developer of some
feature is the one deciding which tests to write. This is because the path of
least resistance is to have no tests at all. Some measures try to prevent this,
e.g. code coverage and mutation testing, but they have their own
issues. Incentives can be aligned more if the some of the tests come from a
separate source, e.g. acceptance tests based on some requirements spec.

There are objective criteria for automated tests, like code coverage and
mutation testing mentioned above. They're not perfect, but seem to work well as
long as they're not being gamed (i.e. when they're treated as an indicator, not
as a goal).

# Purity #

This is another example of gotchas being overly-conservative, since we might
want to use impure components like an internal cache or in-place mutation, which
are impure but might just-so-happen to be safe. Again, I think it's better to
make the path of least resistance more correct, even if the general way to do
that is to forbid the easy things (which are often wrong).

I think the only real incentive misalignment for purity is efficiency; yet as
Knuth tells us, this is usually fine to ignore. It's certainly the case that
having more pervasive use of pure languages in our stack (from the kernel up to
our scripting) would probably make things slower overall; yet I think the safety
benefits would outweigh those downsides for many (me included).

The interesting thing about purity is how much it can simplify (automated)
reasoning: sure it might seem inefficient at first glance, but compilation can
perform much more invasive changes on pure code than are possible in the
presence of side-effects. Supercompilation and superoptimisation come to mind;
although there is certainly a cognitive burden when trying to keep track of how
our code will ultimately compile.

Purity is trivially (automatically) checkable, since we simply don't bother
putting mutable things into our language. Haskell has famously struggled with
this, sticking to lazy evaluation (since, after all, anyone wanting strict
semantics could already pick any of the MLs, Schemes, etc.) which seems to
necessitate purity (due to evaluation being forced "back to front"). This "hair
shirt" approach paid off massively with the recognition of the importance of
monads; more recent investigations into algebraic effects have also proved
useful, again precisely because of the need to deal with purity (arrows were
interesting back in the day, but fell out of favour once applicative functors
and profunctors were adopted).
