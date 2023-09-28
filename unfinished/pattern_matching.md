---
title: The Case for Pattern Matching in Python
packages: [ "expect", "python3" ]
---

```{pipe="tee delayed.sh > /dev/null"}
#!/usr/bin/env bash
set -e

# Copies stdin to stdout, like cat, but pauses between each line

sleep 1
while IFS='' read -r LINE
do
  sleep 0.5
  echo "$LINE"
done
sleep 1
```

```{pipe="tee repl.sh > /dev/null"}
#!/usr/bin/env bash
set -e

# Feeds stdin into a python3 REPL one line at a time, pausing between each.
# Uses 'unbuffer' from the expect package to trick Python into showing a REPL,
# even though its input is coming from a pipe. Uses process substitution with
# 'tee' to splice stdin into the REPL output (mimicing an interactive session)
./delayed.sh | tee >(unbuffer -p python3)
```

```{pipe="sh > /dev/null"}
ls 1>&2
chmod +x delayed.sh repl.sh
(source "$stdenv/setup" && patchShebangs .)
```

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
    return (200, "Result was " + x)
  case [x, *xs]:
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
  return (200, "Result was " + x)
case [x, *xs]:
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

There are some differences, such as the name binding, only calculating one of
the entries/branches, etc. which can mostly be avoided using a setup like this:

```{.python pipe="tee case_example.py"}
def make_case(*cases):
  from functools import reduce
  return lambda(*args, **kwargs): reduce(
    lambda result, f: f(*args, **kwargs) if result == [] else result,
    cases,
    []
  )[0]

f = make_case(
  lambda foo: [(404, "Not found"           )] if len(foo) == 0 else [],
  lambda foo: [(200, "Result was " + foo[0])] if len(foo) == 1 else [],
  lambda foo: [(500, "Ambiguous data"      )] if len(foo) >  1 else [],
)
```

The value of `f` is a function which, if called, will send its arguments to each
of the given `lambda`s, until one of the returns a non-empty list. We can call
this function just like any other, e.g.

```{.python pipe="./repl.sh"}
from case_example import f
f(['x'])
f([])
f(['x', 'y'])

```

The point is we have defined our `lambda` using a set of mutually-exclusive
options (each check has an implicit `and allPreviousChecksFailed`). That's what
the `case` construct is doing, but in a more direct way.

In Python, the keyword `lambda` does a few things:

 - It constructs and returns a particular sort of value, known as a "closure",
   "anonymous function", "first-class function", "function object" or
   "callable". For example:

```{.python pipe="./repl.sh"}
print(repr(lambda: None))

```

 - It also *delays evaluation* of an expression, namely the "body" or "return
   expression" of the lambda is not reduced to a value immediately (unlike, say,
   an element in a list would be). For example:

```{.python pipe="./repl.sh"}
def throw(s): raise Exception(s)

print([throw("I am evaluated immediately, raising an exception")])
print(lambda: throw("I am delayed, so my exception isn't raised yet"))

```

 - It also *binds variables*: the argument list of the closure specifies which
   names will be bound (along with default arguments, etc.). For example:

```{.python pipe="./repl.sh"}
lambda x, y=42: x + y  # Variables x and y are bound in the "body" expression

```


 * Pattern syntax behaves differently to expression syntax precisely because it
   is an *argument list*

 * Scoping is problematic precisely because it doesn't work like functions do

So why introduce `case` at all, if we can use `lambda`? The key feature of
`case` is that it works *piecewise* (i.e. on a `case`-by-`case` basis).

PEP- Because we can *stack* a set of cases expressions
