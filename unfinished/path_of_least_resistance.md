---
title: Paths of Least Resistance
---

> > OOP is fraught with gotchas, misaligned incentives and lacks objectively
> checkable criteria
>
> Like, almost everything in programming these days (especially the frontend
> part)?

Everything has problems; that *doesn't* mean everything is equally problematic.

Static typing, as opposed to dynamic typing:

 - Most gotchas are conservative, i.e. some code isn't allowed even though it
   might be correct. This is preferable to allowing broken code, as long as it's
   not too burdensome to pass the type-checker. The widespread use of statically
   typed languages (e.g. Java and C#) shows that it need not be too
   burdensome. Examples of gotchas are Haskell's 'monomorphism restriction' (
   https://wiki.haskell.org/Monomorphism_restriction ) and Java's lack of
   multiple-inheritance
   (e.g. https://stackoverflow.com/questions/52620936/why-does-java-not-allow-multiple-inheritance-but-does-allow-conforming-to-multip
   ); in both cases it's *possible* to cause a problem, so the type checker
   forbids it.

 - Type-checkers don't have any 'incentive' of their own, but they're a
   mechanism to bring the programmer's incentives ("get this code to compile")
   into alignment with those of library designers ("make sure users call things
   correctly"). Hence they're the *opposite* of misaligned incentives. An
   example is string concatenation, which is the easiest way to dynamically
   create URLs, HTML pages, SQL queries, shell commands, etc. but is vulnerable
   to injection attacks. If a library/framework makes each of these a different
   type then this easy-but-vulnerable approach is no longer possible; if
   escaping functions/methods are the only way to convert a string from one
   language to another, then the easiest way to combine strings is to escape
   them appropriately, hence bringing the user's incentive ("do the easiest
   thing that compiles") into alignment with the designer's ("prevent
   vulnerabilities"). Another example is sequential coupling, where one
   function/method, like 'dbConnect', must be called before others, like
   'dbQuery'. The easiest thing is to just call 'dbQuery', which won't work; the
   designer can forbid this misuse by having that function require a
   'ConnectedDB', and have 'dbConnect' be the only way to obtain one. Again, the
   easiest thing for the user to do (in order for the compiler to succeed) is to
   use the library correctly. There are many other examples of this sort of
   thing.

 - Static types themselves *are* objectively checkable, since that's what
   type-checkers do. It's less trivial to know whether a particular *choice* of
   types is objectively good, but there *are* some general criteria,
   e.g. "invalid states should be unrepresentable". Relatedly, we can
   objectively (and sometimes automatically) check whether all cases have been
   handled, which gives an indication of whether our types are a good fit for
   the problem (e.g. if we're passing around JSON data using strings, there will
   be lots of boilerplate and/or unhandled cases for what to do when given a
   non-JSON string; using a more precise type would reduce this). Note that
   these are general rules, not vague sometimes-"proper"-sometimes-not
   heuristics.

Automated testing:

 - Testing has both overly-conservative and overly-liberal
   gotchas. Overly-conservative examples are things like depending too heavily
   on implementation details (e.g. checking if one list equals another, when
   their order doesn't actually matter) or testing invalid situations
   (e.g. generating test data which doesn't satisfy some invariant). Like static
   typing, these will forbid some correct code, making some tasks harder than
   necessary. Overly-liberal examples are things like only testing the happy
   path, or forgetting some edge case or space of inputs (e.g. negative
   numbers), or failing to test the right code (e.g. mocking the thing we want
   to test). These allow easy-but-broken code through, which might otherwise be
   caught. This is a real problem with automated testing, and one reason why
   it's not a silver bullet.

 - Automated testing does have misaligned incentives, if the developer of some
   feature is the one deciding which tests to write. This is because the path of
   least resistance is to have no tests at all. Some measures try to prevent
   this, e.g. code coverage and mutation testing, but they have their own
   issues. Incentives can be aligned more if the some of the tests come from a
   separate source, e.g. acceptance tests based on some requirements spec.

 - There are objective criteria for automated tests, like code coverage and
   mutation testing mentioned above. They're not perfect, but can work well as
   long as they're not being gamed.

Purity:

 - This is another example of gotchas being overly-conservative, since we might
   want to use impure components like an internal cache or in-place mutation,
   which just-so-happen to be correct in this case. Again, I think it's better
   to make the path of least resistance more correct but harder than necessary,
   rather than have it less correct but easier.

 - Incentives
