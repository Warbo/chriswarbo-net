---
title: More Testing Terminology
---

This is mostly copypasta from [an answer I gave on
StackOverflow](https://softwareengineering.stackexchange.com/a/412281/112115)
about how to avoid mocks when unit testing. It's a continuation of the argument
I gave in
[an earlier blog post](/blog/2017-11-10-unit_testing_terminology.html).

I think the subject of automated testing suffers from conflated and co-opted
terminology, which causes people to talk past each other.

For example, the questioner asks the following:

> Should I be writing only integration tests when there is dependency, and unit
> tests for pieces of code without any dependency?

I think most people would answer this question by saying something like
(ideally, modulo common sense, etc.):

> When there is no dependency, unit tests are sufficient and mocks aren't
needed; when there is dependency, unit tests may need mocks and there should
also be integration tests.

Let's call this Answer A, and I'm going to assume that it's a relatively
uncontroversial thing to say.

However, two people might both give Answer A, but mean very different things
when they say it!

When a person who the questioner terms a "classicist" says Answer A, they might
mean the following (Answer B):

> Functionality that is internal to the application (e.g. a calculation which
> performs no I/O) doesn't need integration tests, and its unit tests don't need
> mocks. Functionality with some external dependency (e.g. a separate
> application like an RDBMS, or a third-party Web service) should have
> integration tests, and if it has unit tests they may need the external
> interactions to be mocked.

When others (let's call them "mockists") say Answer A, the might mean the
following (Answer C):

> A class which doesn't call methods of another class doesn't need integration
> tests, and its unit tests don't need mocks. Classes which call methods of
> other classes should mock those out during their unit tests, and they should
> probably have integration tests too.

These testing strategies are objectively very different, but they both
correspond to Answer A. This is due to the different meanings they are using for
words. We can caricature someone who says Answer A, but means Answer B, as
saying the following:

 - A "dependency" is a different application, Web service, etc. Possibly
   maintained by a third-party. Unchangeable, at least within the scope of our
   project. For example, our application might have MySQL as a dependency.
 - A "unit" is a piece of functionality which makes some sort of sense on its
   own. For example "adding a contact" may be a unit of functionality.
 - A "unit test" checks some aspect of a unit of functionality. For example,
   "if we add a contact with email address X, looking up that contact's email
   address should give back X".
 - An "interface" is the protocol our application should follow to interact
   with a dependency, or how our application should behave when used as a
   dependency by something else. For example, SQL with a certain schema when
   talking to a database; JSON with a certain schema, sent over HTTP, when
   talking to a ReST API.
 - An "integration test" checks that the interface our application is using with
   a dependency will actually have the desired effect. For example "There will
   always be exactly one matching row after running an UPSERT query".
 - A "mock" is a simplified, in-memory alternative to a dependency. For example,
   `MockRedisConnection` may follow the same interface as `RedisConnection`, but
   just contains a `HashMap`. Mocks can sometimes be useful, e.g. if some of our
   unit tests are annoyingly slow, or if our monthly bill from a third-party Web
   service is too high due to all of the calls made by our tests.

We can caricature someone who says Answer A, but means Answer C, as saying the
following:

 - A "dependency" is a different class to the one we're looking at. For example,
   if we're looking at the `Invoice` class, then the `Product` class might be a
   dependency.
 - A "unit" is a chunk of code, usually a method or class. For example
   `User::addContact` may be a unit.
 - A "unit test" checks only the code inside a single unit (e.g. one class).
   For example "Calling `User::addContact` with a contact with email address X
   will ask the `DBConnection` to insert a contacts row containing email address
   X".
 - An "interface" is like a class but only has the method names and types; the
   implementations are provided by each class extending that interface.
 - An "integration test" checks that code involving multiple classes gives the
   correct result. For example "Adding a `Discount` to a `ShoppingCart` affects
   the `Invoice` produced by the `Checkout`".
 - A "mock" is an object which records the method calls made on it, so we can
   check what the unit of code we're testing tried to do in a unit test. They
   are essential if we want to isolate the unit under test from every other
   class.

These are very different meanings, but the relationships between B's meanings
and between C's meanings are similar, which is why both groups of people seem to
agree with each other about Answer A (e.g. their definitions of "dependency" and
"integration test" differ, but both have the relationship "dependencies should
have integration tests").

For the record, I would personally count myself as what that questioner calls a
"classicist" (although I'd not come across that term before); hence why the
above caricatures are clearly biased!

In any case, I think this problem of conflated meanings needs to be addressed
before we can have constructive debates about the merits of one approach versus
another. Unfortunately every time someone tries to introduce some new, more
specialised vocabulary to avoid the existing conflations, those terms start
getting mis-used until they're just as conflated as before.

For example, "Thought Leader X" might want to talk about physical humans
clicking on a UI or typing in a CLI, so they say "it's important to describe how
users can interact with the system; we'll call these 'behaviours'". Their
terminology spreads around, and soon enough "Though Leader Y" (either through
misunderstanding, or thinking they're improving the situation), will say
something like "I agree with X, that when we design a system like the
`WidgetFactory` class, we should use behaviours to describe how it interacts
with its users, like the `ValidationFactory` class". This co-opted usage spreads
around, obscuring the original meaning. Those reading old books and blog posts
from X may get confused about the original message, and start applying their
advice to the newer meanings (after all, this is a highly regarded book by that
influential luminary X!).

We've now reached the situation where "module" means class, "entity" means
class, "unit" means class, "collaborator" means class, "dependency" means class,
"user" means class, "consumer" means class, "client" means class, "system under
test" means class, "service" means class. Where "boundary" means "class
boundary", "external" means "class boundary", "interface" means "class
boundary", "protocol" means "class boundary". Where "behaviour" means "method
call", where "functionality" means "method call", where "message send" means
"method call"...
