---
title: Unit Testing Terminology
---

Despite the plethora of existing terminology, I think phrases from testing, like
"dependency", "mock", "unit", etc., still need to be more precise about what
they're talking about, to prevent being misunderstood and having people talk
past each other. The big problem, IMHO, is that some people use these phrases to
mean some abstract, domain-dependent concept, whilst others use them to mean
programming language features.

One common approach is the use the word "unit" to mean "chunk of code" (e.g. a
method, a function, a class, a module, etc., like `updateCustomerAddress`), and
"dependency" to mean "separate code which that chunk of code needs" (like
`postCodeLookup`).

There's an equally valid alternative for these same terms: "unit" can mean "unit
of functionality" regardless of how the instructions happen to be arranged (e.g.
"updating a customer’s address"); "dependencies" are those things which
just-so-happen to be outside our control (e.g. a third-party service; but not
necessarily a third-party library).

Another example is phrases like "public interface": some people take this to
mean "the fields and methods of a class which are annotated with the `public`
keyword", some take it to mean "the ways that information can get into and out
of the system". It's even tempting to rephrase the latter as "the API made
available to the 'consumer' or 'client' of this system", yet even that suffers
the same trap, since it assumes a "consumer"/"client" is a person using this
project (e.g. as a library for use in their own code), whilst others take it to
mean "classes which reference this one".

I wonder if there’s a de facto standard terminology for this difference?

Personally I prefer to think in the abstract, domain-dependent terms, where
"unit tests" are testing that some functionality works, and "dependencies" are
things like database servers and Web APIs; but I've worked in places where
"unit" means "method" and "dependency" means "other classes", and it's
remarkable how much confusion these seemingly "standard" terms can cause.

I know there's things like BDD (Behaviour Driven Development), but that also
seems imprecise in the same way. In fact, the workplace that followed the
"unit = method" approach referred to what we were doing as "BDD", presumably
because we used PHPSpec instead of PHPUnit.

Note that this post is mostly advocating for the use of more precise phrasing,
regardless of which side you're arguing for or against. Nevertheless, for those
who haven't encountered these two different styles, they're contrasted quite
nicely in [this talk](https://www.infoq.com/presentations/tdd-original), which
argues in favour of the "abstract, domain-dependent" approach, with reasoning
that closely matches how I reached my current opinion.
