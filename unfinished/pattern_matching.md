---
title: The `case` for Pattern Matching in Python
---

[Python Enhancement Proposal 0643](https://www.python.org/dev/peps/pep-0634)
was [recently accepted](https://lwn.net/Articles/845480), which adds
[pattern-matching](https://en.wikipedia.org/wiki/Pattern_matching) to the
language.

This has [caused controversy](https://news.ycombinator.com/item?id=26080760),
with (from what I can tell), three main objections:

 * Those who simply don't want pattern-matching in the language, either because
   it's an unnecessary complication, or doesn't fit well with a "Pythonic" style
   of programming.
 * Those who wouldn't mind pattern-matching *per se*, but find the proposed form
   overly complicated and confusing; especially all the new syntax for patterns.
 * Those who wouldn't mind this proposal *per se*, but find the variable scoping
   rules problematic, especially binding names that already exist in scope.

I've weighed in on that Hacker News discussion myself, and after some thinking
I've come to a perspective that seems to be rather different from many others,
which hopefully offers insight, explanation and suggestions for moving forward.
Hence this blog post.

To understand this perspective, we need to begin with the most important part:

## Forget About `match` ##

Here's an example of the new pattern-matching functionality:

``` python
match result:
  case []:
    return (404, "Not found")
  case [x]:
    return (200, "Result was " + x
  case [x, *xs):
    return (500, "Ambiguous data")
```

Don't let the name "pattern *match*ing" fool you; despite the introduction of
the new keyword `match`, and its appearance at the beginning of this snippet,
it's mostly irrelevant to this new functionality.

Once we've understood pattern matching from my proposed perspective, we'll
revisit the `match` keyword, and see why it's completely redundant. For now,
we'll just ignore that first line completely.

## Let's Understand `case` ##

Here's the same example as above, without the irrelevant first line:

``` python
case []:
  return (404, "Not found")
case [x]:
  return (200, "Result was " + x
case [x, *xs):
  return (500, "Ambiguous data")
```

What's happening here? I claim that the most illuminating way to think about
the `case` keyword is a "piecewise `lambda`". The above code is similar to a
`lambda` performing "dictionary dispatch", like this:

``` python
lambda foo: {
  (len(foo) == 0): (404, "Not found"           ),
  (len(foo) == 1): (200, "Result was " + foo[0]),
  (len(foo) >  1): (500, "Ambiguous data"      ),
}[True]
```

In Python, the keyword `lambda` combines two things: an 'argument list' and a
'return expression'. For example:

``` python
double = lambda x: x + x
```

 * Pattern syntax behaves differently to expression syntax precisely because it
   is an *argument list*

 * Scoping is problematic precisely because it doesn't work like functions do

So why introduce `case` at all, if we can use `lambda`? The key feature of
`case` is that it works *piecewise* (i.e. on a `case`-by-`case` basis).

PEP- Because we can *stack* a set of cases expressions
