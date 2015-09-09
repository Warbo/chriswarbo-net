---
title: "Plumb: A Mini Language for Mini Functions"
---
Plumb is a [Domain Specific Language](http://en.wikipedia.org/wiki/Domain-specific_language) for defining small, incidental, throwaway functions. In other words, the 'plumbing' to join the component parts of a program.

Not very useful on its own, Plumb is designed to be *embedded* into more capable 'host' languages. [Implementations](implementations.html) are simple to create.

Plumb is actually a special syntax for [Lambda Calculus](http://en.wikipedia.org/wiki/Lambda_calculus). If you don't know what that is, don't worry ;)

### When To Use Plumb? ###

 - When verbosity puts you off using anonymous functions
 - When you lack an appropriate argument for map/reduce/filter, and fall back to a loop
 - When your codebase doesn't quite fit a library's API
 - When you need a little laziness
 - When your codebase spans multiple languages
 - When it's obvious what you're doing, but the code is tedious
 - When some simple argument shuffling is lost in a sea of keywords

### When To Avoid Plumb? ###

 - When you're writing important business logic - better to keep the explicit detail
 - When you're writing a non-obvious algorithm - don't make it harder to follow
 - When your Plumb spans more than a couple of lines - you probably want to refactor into smaller parts

## Getting Started ##

Loading/importing/including Plumb in your [language of choice](implementations.html) will give you access to the `plumb` function. This is the interpreter, which turns Plumb definitions into real functions of your language.

Check out the [usage guide](using.html) to see how Plumb definitions work.

I've also written a blog post [explaining Plumb and its implementation](/blog/2014-08-18-edsl.html).
