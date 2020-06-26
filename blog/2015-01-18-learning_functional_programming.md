---
title: Learning Functional Programming
packages: [ 'python' ]
---

I've seen a [few][] [questions][] about where to begin learning functional
programming, so I thought I'd finally combine my individual answers into a page
that I can link to in the future.

## Functional Style ##

Functional programming is a *style*, which can be used in many languages, just
like Object Oriented programming is a style. Some languages are created with
these styles in mind (Haskell, ML, Scheme, etc. for functional; Java, C#,
Smalltalk, etc. for OO), so functional programming is more natural in some
languages than others.

It's obviously easiest to do functional programming in a functional language
like Haskell, with features like tail-call optimisation, currying, first-class
functions, list comprehensions, etc. It's not too bad in scripting languages
like Python or Javascript, with first-class functions. It's more difficult when
functions are second-class, like in PHP or C.

If you're trying to learn functional programming, I'd recommend to start with
the "most functional" language that you're already comfortable with. For
example, if you know C and Python, go with Python since it has first class
functions. As you learn about functional programming principles (no mutable
state, no side-effects, etc.) try following them in your language of choice, as
well as a language designed for functional programming
(eg. [Haskell](https://tryhaskell.org), [OCaml](http://try.ocamlpro.com)
or [Scheme](http://tryscheme.sourceforge.net)).

This will do a few things:

 - Make you more familiar with functional programming techniques
 - Give you fresh perspectives on existing code, eg. the similarities and
   differences between OO and functional styles
 - Make you appreciate various functional programming features, by trying to
   live without them ;)

## Value-Oriented Programming ##

Most functional programming boils down to *defining values*; asking "what should
this be?". In contrast, imperative programming (eg. OO or procedural) is about
*performing steps*; asking "what should happen next?".

This "value-oriented programming" crops up in a lot of places, some of which are
explained below. We can point out the difference using the following
tongue-in-cheek example:

```javascript
// An imperative mindset performs a series of actions

// Check the value of "foo"
if (foo) {
    // Set the value of "bar"
    bar = FALSE;
}
else {
    // Set the value of "bar"
    bar = TRUE;
}

// A functional mindset asks what value "bar" should have

// "bar" is the opposite of "foo"
bar = !foo;
```

## Functional Programming Techniques ##

Here are a few ways to become more "value-oriented":

### Ternaries instead of `if`/`then`/`else` ###

Branches send the instruction pointer along different paths, which is a very
non-functional thing to do. Instead of choosing which *action* to perform, try
using ternary operators to choose between different *values*:

```php
// Imperative
if (valid($x)) return $x;
return $default;

// Functional
return valid($x)? $x : $default;
```

`if`{.python} branches can exist without an `else`{.python}, which makes no
sense from a value-oriented perspective. Ternaries define a value for each case:

```php
// Imperative thinking
if ($condition) {
    $x = foo();
}
// $x might be undefined here!

// Functional thinking
$x = $condition? foo() : null;
// $x always has a value here
```

One thing to keep in mind is that too much nesting can be difficult to
understand. This is the case whether we're using branches or ternaries, but
ternaries tend to look more difficult because they're usually much more compact
than branches. We can get rid of nesting by using extra variables.

Some languages have `case`{.ruby} expressions. If ternaries are the functional
equivalent of `if`{.python} statements, then `case`{.ruby} expressions are the
functional equivalent to `switch`{.c} statements:

```php
// Imperative
switch ($x) {
  case 1:
  case 2:
  case 3:
    $y = "hello";
    break;
  case 4:
    $y = "world";
    break;
  default:
    $y = "foo";
}
```

```ruby
# Functional
y = case x
      when 1..3 then "hello"
      when 4    then "world"
      else           "foo"
```

Ternary and `case`{.ruby} expressions are a limited form of the
*pattern-matching* found in many functional languages:

```haskell
case my_list of
  []        -> "Empty"
  [x]       -> "Just " ++ x
  [x, y]    -> "Both " ++ x ++ " and " ++ y
  [x, y, z] -> "Triple: " ++ x ++ ", " ++ y ++ ", " ++ z
  _         -> "A very long list!"
```

The `cond`{.commonlisp} expression found in LISP is also a more sophisticated
form of ternary/case:

```commonlisp
(cond
  ((> x 10)
   (concat x " is more than 10"))

  ((< x 0)
   (concat "0 is more than " x))

  ((equal y 100)
   "y is large")

   "None of the above")
```

### Static Single Assignment ###

Mutating variables is a symptom of imperative thinking ("then do this"), whereas
defining a load of constant values is a symptom of value thinking ("I'll need
this as well"). Rather than re-assigning variables to new values, try defining
new variables instead:

```python
def imperativeAverage(a, b, c):
  total  = a
  total += b
  total += c
  return total / 3

def functionalAverage(a, b, c):
  ab  = a  + b
  abc = ab + c
  return abc / 3
```

Instead of providing "setter" methods on objects, instantiate fresh objects:

```python
class ImperativeObject:
    def __init__(self, x, y):
        self.set_x(x)
        self.set_y(y)

    def set_x(self, x):
        self.x = x

    def set_y(self, y):
        self.y = y

class FunctionalObject:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def with_x(self, x):
        return self.__class__(x, self.y)

    def with_y(self, y):
        return self.__class__(self.x, y)
```

This is much easier when functions/methods return copies, rather than mutating
existing objects in-place:

```python
def imperativeMerge(x, y):
    x.extend(y)

def functionalMerge(x, y):
    return x + y
```

Code written in SSA style follows *data flow* instead of *control flow*

 - Control flow is full of questions and uncertainty
    - When will this happen?
    - Are these happening in the right order?
    - Is it OK to move this over here?
 - When each variable is only defined once, these questions go away.
    - A definition like `y = f(x)`{.python} must come after the definition of
      `x`{.python}
    - If the definition of `y`{.python} *doesn't* depend on `x`, their order
      doesn't matter (except for side-effects, which I'll get to later!).

### Embrace Recursion ###

We can use recursion instead of loops, to make everything constant:

```python
def imperativeMap(func, lst):
    result = []
    for elem in lst:               # This keeps changing the value of elem!
        result.append(func(elem))  # This keeps changing the value of result!
    return result

def functionalMap(func, lst):
    # None of these values change; they're all constant
    return [func(lst[0])] + functionalMap(func, lst[1:]) if len(lst) > 0
        else []
```

Of course, many languages implement this example directly!

```python
functionalMap = map
```

We can use recursion to set default values, rather than re-assigning variables:

```php
function imperativeDefault($x, $y = null) {
  if (is_null($y)) $y = makeY($x);
  return process($x + $y);
}

function functionalDefault($x, $y = null) {
  return is_null($y)? functionalDefault($x, makeY($x))
                    : process($x + $y)
}
```

Of course, There's More Than One Way To Do It. In this case, Static Single
Assignment works just as well:

```php
function functionalDefault2($x, $y_arg = null) {
  $y = is_null($y_arg)? makeY($x) : $y_arg;
  return process($x + $y);
}
```

Recursion is much easier with *tail-call optimisation*. Without it, stack
overflows force you to limit this to a small, known number of iterations
(eg. retrying a certain number of times before failing).

We can often encapsulate a loop into a generic function, then use this function
to avoid stack overflows *and* avoid explicitly writing loops everywhere.

 - Many languages come with some of these functions already, like
   `map`{.python}, `reduce`{.python} (AKA `fold`) and `filter`{.python}.
 - Functions allow us to abstract, compose and encapsulate.
    - Loops force us to break apart our encapsulation and deal with raw values
      in a low-level, first-order way.

### Separate Functionality From Structure ###

Functional programming is about *values*. Despite the name, functional
programming doesn't give functions any particularly special treatment; they're
just values like everything else (unlike, for example, in procedural or object
oriented programming, where functions are not at all like other data).

By building up a large collection of small functions (possibly managed using a
*module system*), we get a powerful toolkit for manipulating all kinds of data.

Likewise, data in functional programs is just that: data. It has no associated
"behaviours", like the kind found in object oriented programming. Thanks to
this, we can use the same generic datastructures again and again
(eg. `List`{.haskell}, `BinaryTree`{.haskell}, `Pair`{.haskell}, etc.), and
re-use the functions we write in many projects.

This is a well-known principle in many programming paradigms: if we write 100
functions, but they involve 10 different datastructures, then on average we can
only do 10 things to any particular value. Instead, if we write those 100
functions to all use *the same* datastructure, then we can do 100 things to
*any* value. A great example of this is LISP, which uses the "cons cell" (a
tuple of two values) to represent key/value mappings, linked-lists, tree
structures, syntax trees, etc.

### Code Is Inherently Interesting ###

In functional programming, there is less emphasis on what code is *intended for*
than in other approaches. Instead there is much more focus on what code
*actually is*. For example, Object Oriented code is very specialised towards a
particular application:

```java
class Person {
    private Name        name;
    private DateOfBirth dob;

    public Person(Name name, DateOfBirth dob) {
        this.name = name;
        this.dob  = dob;
    }

    public Name getName() {
        return this.name;
    }

    public DateOfBirth getDob() {
        return this.dob;
    }
}
```

Functional code concentrates on the underlying structure of the solution, in
this case pairs of values:

```haskell
data Pair a b = P a b

first  (P x y) = x

second (P x y) = y
```

Applications are just a thin veneer over this structure. In this case we remove
some polymorphism (the structure is *accidentally* too generic!) and make more
descriptive aliases:

```haskell
type Person = Pair Name DateOfBirth

getName = first
getDob  = second
```

This goes hand-in-hand with the ideas of generic datastructures and re-usable
functions. In functional programming we tend to build up more and more functions
to manipulate existing datastructures in new ways. Since any of these functions
may be used with any other (of compatible type!), the only assumptions we can
make in our code are the properties of the data structures themselves. In fact,
it's routine for casual blog posts about functional programming to contain
*Mathematical proofs* about code based on these properties!

With all this reusable code available, there has to be a pretty compelling
reason to write a *new* datastructure, and hence abandon all of those existing
libraries. That's why proofs are useful: we can use examples to show that
something *is* possible with a datastructure, but to show that our desired
functionality *isn't* possible we need a proof.

With such an emphasis on *possibilities* and *reasoning about properties*, the
*code* in a library/datastructure becomes the main focus of our attention; the
fact that it may be well-suited to a particular application isn't too
important. As shown above, functional applications are a thin layer on top of
generic libraries. If requirements change and the approach taken with the
existing library isn't suitable anymore, we can transplant this layer on to a
new, more appropriate library. The libraries themselves remain useful,
consistent, generic and abstract.

### *Compose* as well as Abstract ###

The functions in functional programs are usually *much* smaller than equivalents
in other styles. The reason is that small functions are much easier to re-use
than large, complicated ones. In fact, we can create and combine small reusable
functions *using* small, re-usable functions. For example, if we define function
composition:

```javascript
function compose(f, g) {
    return function(x) {
        return f(g(x));
    };
}
```

Then we can avoid *defining* new functions like this:

```javascript
function complicated(x) {
    var y = foo(x);
    var z = bar(y);
    return baz(z);
}
```

We can instead *calculate* new functions, like this:

```javascript
var simple = compose(compose(baz, bar), foo);
```

By turning each part of our algorithm into a separate function, we can reuse
them trivially without having to copy/paste code around. The overhead of
plumbing them back together isn't too bad if we use higher-order functions like
`compose`. These plumbing functions are very general, so even if our language
doesn't provide many, we only have to write them once to get the benefit all
over the place.

One of the examples in the section on recursion mentions the `map`{.python}
function, which is a plumbing function for looping through lists. It can be used
all over the place, and since it's a regular function (unlike, say, a
`for`{.python} loop), we can pass it around as a value, eg. as an argument to
other plumbing functions:

```python
# Non-functional code often conflates how to *get* values with *what to do* with
# them
result = []
for elem in collection:
    result.append(len(str(elem + 10)))

# Functional code separates the two into distinct functions
process = lambda elem: len(str(elem + 10))
result  = map(process, collection)
```

### Separate Calculating *What* To Do From *Actually Doing It* ###

Performing irreversible actions in the middle of a calculation leads to all
kinds of headaches. If you've ever used "dependency injection" then you've
suffered these headaches.

By removing side-effects from our calculations, we no longer need to care which
order they happen in, how many times they run, whether they'll mess up in a
test, etc.

An easy way to stop calculations from performing actions is to *calculate the
action without performing it*. For example, if trying to calculate a user's age
requires a database access, don't bother trying to calculate their age. Instead,
calculate *which* DB query will get their age, and return that query. You're
then free to *either* run the query, or never run it and treat it just like any
other value, eg. checking it during tests.

Ideas like dependency injection, mock objects, etc. disappear when we're just
dealing with actions-as-values; although these are easier to deal with when we
have functions to combine them, like `<*>`{.haskell} and `>>=`{.haskell} in
Haskell.

For example:

```{.python pipe="tee actions.py"}
def imperative(x):
  if len(x) is 0:
    print "empty"      # Print a value
    return 1
  if len(x) > 10:
    print str(len(x))  # Print a value
  print "recursing"    # Print a value
  return sum(map(imperative, x))  # Recurse

```

```python
val1 = imperative(...) # Run the actions and calculation
```

```{.python pipe="tee -a actions.py"}
def functional(x):
  # Recurse, to get return values and printable strings
  rec = map(functional, x)

  recstrs =     [s      for val in rec for s in val[0]]
  recval  = sum([val[1] for val in rec])

  # Combine any recursive results with our own
  big  = [str(len(x))] if len(x) > 10 else []
  strs = big + ["recursing"] + recstrs

  # Return both the strings and the value
  return (["empty"], 1) if len(x) is 0 else (strs, recval)

```

```python
(strs, val2) = functional(...) # Run the calculation
map(sys.stdout.write, strs)    # Optionally, run the actions
```

```{pipe="python > test.out"}
# Tests
from actions import imperative, functional

def test(x, y):
  if x == y:
    return True
  raise Exception("Not equal: " + repr(x) + " and " + repr(y))

print "Imperative tests"

test(imperative([]), 1)
test(imperative(11 * [[]]), 11)
test(imperative([20 * [[]], 5 * [[]]]), 25)

print "Functional tests"

test(functional([]), (["empty"], 1))
test(functional(11 * [[]]), (["11", "recursing"] + 11 * ["empty"], 11))
test(functional([20 * [[]], 5 * [[]]]),
     (["recursing", "20", "recursing"] + 20 * ["empty"] +
      ["recursing"] + 5 * ["empty"],
      25))

import sys
sys.stderr.write("Action tests passed\n")
```

```{pipe="python 1>&2"}
import sys

imp    = [l.strip() for l in open('test.out').readlines()]
expect = ["Imperative tests"]                              + \
         ["empty",     "11", "recursing"] + 11 * ["empty"] + \
         ["recursing", "20", "recursing"] + 20 * ["empty"] + \
         ["recursing"]                    + 5  * ["empty"] + \
         ["Functional tests"]
if imp != expect:
  print("imperative output doesn't match functional")
  print("Expected:\n" + repr(expect))
  print("Got:\n"      + repr(imp))
  sys.exit(1)
```

### Pass Functions Around ###

One good use of first-class functions is to avoid the complexities of
`switch`{.c}/`elif`{.python} statements, and even simple classes, by looking up
functions from a collection:

```python
def imperativeProcess(x):
    if thing(x):
        return doThing(x)
    elif stuff(x):
        return doStuff(x)
    elif blah(x):
        return doBlah(x)
    return doDefault(x)`

def functionalProcess(x):
    funcs = {thing(x): doThing,
             stuff(x): doStuff,
             blah(x): doBlah}
    return funcs[True](x) if True in funcs else doDefault(x)
```

Features like Haskell-style type-classes, ML-style modules and Scala/Agda-style
implicits make this easier.

[few]: http://programmers.stackexchange.com/questions/28964/challenges-for-the-experienced-coder-to-learn-functional-programming
[questions]: http://programmers.stackexchange.com/questions/108105/what-should-i-understand-before-i-try-to-understand-functional-programming
