---
title: Curry Your Calls, Uncurry Your Returns
---

## Currying ##

[Currying](https://en.wikipedia.org/wiki/Currying) is widely used in functional
programming. Some languages, like ML and Haskell, curry everything by default;
in others, like Scheme and Javascript, we can implement currying as a
transformation (either a higher-order function or a macro), and call it
explicitly wherever we want to use it.

These explicit, manual implementations often look something like the following
pseudocode:

```
curryWith1 = function(f, args) {
  return function(newArgs...) {
    allArgs = args + newArgs;
    return (if length(allArgs) >= argCount(f)
               then f(allArgs...)
               else curryWith1(f, allArgs));
  };
}

curry1 = function(f) {
  return curryWith1(f, []);
}
```

Let's use `curry1` to curry a function:

```
func = function(w, x) {
  return function(y) {
    return function(z) {
      return w + x + y + z;
    };
  };
}

curriedF1 = curry1(func);
```

If we call this `curriedF1` function, the evaluation will look something like
this if we give the right number of arguments for `func`:

```
curriedF1(1, 2);

// Substitute in curriedF1
curry1(func)(1, 2)

// Substitute in curry1
(function(f) {
  return curryWith1(f, []);
})(func)(1, 2)

// Evaluate '(function(f) {...})(func)' call
curryWith1(func, [])(1, 2)

// Substitute in curryWith1
(function(f, args) {
  return function(newArgs...) {
    allArgs = args + newArgs;
    return (if length(allArgs) >= argCount(f)
               then f(allArgs...)
               else curryWith1(f, allArgs));
  }
})(func, [])(1, 2)

// Evaluate '(function(f, args) {...})(func, [])' call
(function(newArgs...) {
  allArgs = [] + newArgs;
  return (if length(allArgs) >= argCount(func)
             then func(allArgs...)
             else curryWith1(func, allArgs));
})(1, 2)

// Evaluate '(function(newArgs...) {...})(1, 2)' call
{
  allArgs = [] + [1, 2];
  if length(allArgs) >= argCount(func)
     then func(allArgs...)
     else curryWith1(func, allArgs));
}

// Calculate allArgs and argCount(func)
{
  allArgs = [1, 2];
  if length(allArgs) >= 2
     then func(allArgs...)
     else curryWith1(func, allArgs);
}

// Substitute in allArgs
if length([1, 2]) >= 2
   then func(1, 2)
   else curryWith1(func, [1, 2]);

// Calculate branch
func(1, 2);

// Call func
function(y) {
  return function(z) {
    return 1 + 2 + y + z;
  };
}
```

If we call `curriedF1` with too few arguments, e.g. `curriedF1(1)`, then we'll
get back another function, which we can call with more arguments, e.g.
`curriedF1(1)(2)`:

```
curriedF1(1)(2);

// Proceed as above, down to substituting in allArgs
(if length([1]) >= 2
    then func(1)
    else curryWith1(func, [1]))(2)

// Calculate branch
curryWith1(func, [1])(2)

// Proceed as above, up to calculation of allArgs
{
  allArgs = [1] + [2];
  if length(allArgs) >= argCount(func)
     then func(allArgs...)
     else curryWith1(func, allArgs);
}

// Calculate allArgs
{
  allArgs = [1, 2];
  if length(allArgs) >= argCount(func)
     then func(allArgs...)
     else curryWith1(func, allArgs);
}

// Proceed as before
func(1, 2)

function(y) {
  return function(z) {
    return 1 + 2 + y + z;
  };
}
```

Hence calling `curriedF1(1)(2)` gives the same result as calling
`curriedF1(1, 2)`. It *looks like* we can we can either pass arguments to a
curried function one (or a few) at a time, or all at once, and get the same
result. Indeed, that's what we *want*, but in general that's not actually the
case, if we use this `curry1` function.

Consider the following, where we call `curriedF1` with 4 arguments all at once:

```
curriedF1(1, 2, 3, 4);

// Proceed as above, up to calculating allArgs
{
  allArgs = [1, 2, 3, 4];
  if length(allArgs) >= argCount(func)
     then func(allArgs...)
     else curryWith1(func, allArgs));
}

// Substitute in allArgs
if length([1, 2, 3, 4]) >= argCount(func)
   then func(1, 2, 3, 4)
   else curryWith1(func, [1, 2, 3, 4]);

// Calculate argCount(func)
if length([1, 2, 3, 4]) >= 2
   then func(1, 2, 3, 4)
   else curryWith1(func, [1, 2, 3, 4]);

// Calculate the branch
func(1, 2, 3, 4);

// Call func, as before
function(y) {
  return function(z) {
    return 1 + 2 + y + z;
  };
}
```

This is what we'll get in a language like Javascript or PHP, where it's fine to
call a function with more arguments than its signature specifies (this is one
way to implement functions with variable arity, or "var args"). In other
languages, like Python or Scheme, this will trigger an error. Such language
differences aren't important to us here; my point will hold regardless.

Now let's call `curriedF1` with 4 arguments, but pass them in one at a time:

```
curriedF1(1)(2)(3)(4);

// Evaluate `curriedF1(1)(2)` as above
(function(y) {
  return function(z) {
    return 1 + 2 + y + z;
  };
})(3)(4);

// Call '(function(y) {...})(3)'
(function(z) {
  return 1 + 2 + 3 + z;
})(4);

// Call '(function(z) {...})(4)'
1 + 2 + 3 + 4;

// Perform arithmetic
10
```

This is certainly a very different result to what we got above! Notice that it
doesn't matter whether our language allows functions to be called with too many
arguments or not: the number `10` is not the same as a function (in the case of
Javascript/PHP/etc.) *or* an error (in the case of Python/Scheme/etc.).

Hence, if we have a curried function `f`, it is *not* the case that
`f(a, b)` and `f(a)(b)` will return the same thing.

Also, it's important to point out how we triggered this discrepancy: we ran into
problems because `func` is returning a function. This may sound like an edge
case, certainly to those whose programming style avoids such things; yet this is
*exactly* the sort of thing that is encouraged in functional programming. Not
only that, but the pattern
`function (y) { return function(z) { return ... + y + z; }; }` *is what currying
is all about*!

This seems like an under-discussed problem with such explicit/manual currying
implementations.

However, there is a solution! As I explained in
[better currying in Javascript](/blog/2012-10-01-better_currying_in_javascript.html),
the problem is that currying the functions we call is only half the story.

## Uncurrying ##

To get this desired behaviour, we must also perform the "dual" operation:
*uncurrying* the values we return!

As the name suggests, "uncurrying" is the opposite of currying. Given a function
in curried form, like:

```
example = function(x) {
  return function(y) {
    return function(z) {
      return x + y + z;
    };
  };
};
```

We can uncurry it to get a function which takes all of its arguments in one go:

```
uncurry(example) == function(x, y, z) { return example(x)(y)(z); }
```

To implement a general uncurry function, we need to handle a variable number of
arguments, and call the underlying function with one at a time. We can do the
latter using a loop: given a function `f` and a list of arguments `args` we can
do something like this:

```
val = f;
while length(args) > 0 {
  arg  = head(args);
  args = tail(args)
  val  = val(arg);
}
return val;
```

## Combining Both ##

The first change we should make to our `curryWith1` function is to avoid calling
the underlying function `f` with too many arguments. Instead, we should only
take however many `f` needs from the front of the `allArgs` list:

```
curryWith2 = function(f, args) {
  return function(newArgs...) {
    allArgs = args + newArgs;
    needed  = argCount(f);
    return (if length(allArgs) >= needed
               then f(take(needed, allArgs)...)
               else curryWith2(f, allArgs));
  };
}

curry2 = function(f) {
  return curryWith2(f, []);
}
```

This version will avoid errors in the Python/Scheme case; extra arguments will
simply be ignored, like in Javascript/PHP.

The next step is to run our uncurry loop over those remaining arguments (if
any):

```
curryWith3 = function(f, args) {
  return function(newArgs...) {
    allArgs = args + newArgs;
    needed  = argCount(f);
    if length(allArgs) < needed {
      return curryWith3(f, allArgs);
    }
    val  = f(take(needed, allArgs)...);
    rest = drop(needed, allArgs);
    while length(rest) > 0 {
      arg  = head(rest);
      rest = tail(rest);
      val  = val(arg);
    }
    return val;
  };
}

curry3 = function(f) {
  return curryWith3(f, []);
}
```

This change doesn't affect the behaviour in the first two cases: when a function
is called with the "right" number of arguments, or too few arguments. We can
step through the too-many-arguments examples to see how the previous discrepancy
is avoided. First, calling `curriedF3(1, 2, 3, 4)`:

```
curriedF3 = curry3(func);

curriedF3(1, 2, 3, 4);

// Proceed as above, until we substitute into 'function(newArgs...) {...}'
(function(newArgs...) {
  allArgs = [] + newArgs;
  needed  = argCount(func);
  if length(allArgs) < needed {
    return curryWith3(func, allArgs);
  }
  val  = func(take(needed, allArgs)...);
  rest = drop(needed, allArgs);
  while length(rest) > 0 {
    arg  = head(rest);
    rest = tail(rest);
    val  = val(arg);
  }
  return val;
})(1, 2, 3, 4);

// Call '(function(newArgs...) {...})(1, 2, 3, 4)'. Note we introduce an 'else'
// branch since we can no longer return early.
{
  allArgs = [] + [1, 2, 3, 4];
  needed  = argCount(func);
  if length(allArgs) < needed {
    curryWith3(func, allArgs);
  } else {
    val  = func(take(needed, allArgs)...);
    rest = drop(needed, allArgs);
    while length(rest) > 0 {
      arg  = head(rest);
      rest = tail(rest);
      val  = val(arg);
    }
    val;
  }
}

// Calculate and substitute allArgs and needed
if length([1, 2, 3, 4]) < 2 {
  curryWith3(func, [1, 2, 3, 4]);
} else {
  val  = func(take(2, [1, 2, 3, 4])...);
  rest = drop(2, [1, 2, 3, 4]);
  while length(rest) > 0 {
    arg  = head(rest);
    rest = tail(rest);
    val  = val(arg);
  }
  val;
}

// Calculate if branch
{
  val  = func(take(2, [1, 2, 3, 4])...);
  rest = drop(2, [1, 2, 3, 4]);
  while length(rest) > 0 {
    arg  = head(rest);
    rest = tail(rest);
    val  = val(arg);
  }
  val;
}

// Calculate take and drop
{
  val  = func([1, 2]...);
  rest = [3, 4];
  while length(rest) > 0 {
    arg  = head(rest);
    rest = tail(rest);
    val  = val(arg);
  }
  val;
}

// Call 'func([1, 2]...)' AKA 'func(1, 2)'
{
  val  = function(y) {
           return function(z) {
             return 1 + 2 + y + z;
           };
         };
  rest = [3, 4];
  while length(rest) > 0 {
    arg  = head(rest);
    rest = tail(rest);
    val  = val(arg);
  }
  val;
}

// Unroll loop
{
  val  = function(y) {
           return function(z) {
             return 1 + 2 + y + z;
           };
         };
  rest = [3, 4];

  // First iteration
  arg  = 3;
  rest = [4];
  val  = val(arg);

  // Second iteration
  arg  = 4;
  rest = [];
  val  = val(arg);

  val;
}

// Substitute in arg, discard rest
{
  val  = function(y) {
           return function(z) {
             return 1 + 2 + y + z;
           };
         };
  val = val(3);
  val = val(4);
  val;
}

// Substitute in 'val' variables
((function(y) {
  return function(z) {
    return 1 + 2 + y + z;
  };
})(3))(4);

// Call '(function(y) {...})(3)'
(function(z) {
  return 1 + 2 + 3 + z;
})(4);

// Call '(function(z) {...})(4)'
1 + 2 + 3 + 4;

// Perform arithmetic
10
```

Now we can compare this with `curriedF3(1)(2)(3)(4)`:

```
curriedF3(1)(2)(3)(4);

// Evaluate as far as '(function(newArgs...) {...})(...)'
(function(newArgs...) {
  allArgs = [] + newArgs;
  needed  = argCount(func);
  if length(allArgs) < needed {
    return curryWith3(func, allArgs);
  }
  val  = func(take(needed, allArgs)...);
  rest = drop(needed, allArgs);
  while length(rest) > 0 {
    arg  = head(rest);
    rest = tail(rest);
    val  = val(arg);
  }
  return val;
})(1)(2)(3)(4);

// Call '(function(newArgs...) {...})(1)'
({
  allArgs = [] + [1];
  needed  = argCount(func);
  if length(allArgs) < needed {
    curryWith3(func, allArgs);
  } else {
    val  = func(take(needed, allArgs)...);
    rest = drop(needed, allArgs);
    while length(rest) > 0 {
      arg  = head(rest);
      rest = tail(rest);
      val  = val(arg);
    }
    val;
  }
})(2)(3)(4);

// Calculate constants
({
  allArgs = [1];
  needed  = 2;
  if length(allArgs) < needed {
    curryWith3(func, allArgs);
  } else {
    val  = func(take(needed, allArgs)...);
    rest = drop(needed, allArgs);
    while length(rest) > 0 {
      arg  = head(rest);
      rest = tail(rest);
      val  = val(arg);
    }
    val;
  }
})(2)(3)(4);

// Substitute allArgs and needed
({
  if length([1]) < 2 {
    curryWith3(func, [1]);
  } else {
    val  = func(take(2, [1])...);
    rest = drop(2, [1]);
    while length(rest) > 0 {
      arg  = head(rest);
      rest = tail(rest);
      val  = val(arg);
    }
    val;
  }
})(2)(3)(4);

// Calculate if branch
curryWith3(func, [1])(2)(3)(4);

// Evaluate as above, but the value of 'args' is '[1]'
(function(newArgs...) {
  allArgs = [1] + newArgs;
  needed  = argCount(func);
  if length(allArgs) < needed {
    return curryWith3(func, allArgs);
  }
  val  = func(take(needed, allArgs)...);
  rest = drop(needed, allArgs);
  while length(rest) > 0 {
    arg  = head(rest);
    rest = tail(rest);
    val  = val(arg);
  }
  return val;
})(2)(3)(4);

// Evaluate as above
({
  if length([1, 2]) < 2 {
    curryWith3(func, [1, 2]);
  } else {
    val  = func(take(2, [1, 2])...);
    rest = drop(2, [1, 2]);
    while length(rest) > 0 {
      arg  = head(rest);
      rest = tail(rest);
      val  = val(arg);
    }
    val;
  }
})(3)(4);

// Calculate if branch
({
  val  = func(take(2, [1, 2])...);
  rest = drop(2, [1, 2]);
  while length(rest) > 0 {
    arg  = head(rest);
    rest = tail(rest);
    val  = val(arg);
  }
  val;
})(3)(4);

// Calculate take and drop
({
  val  = func([1, 2]...);
  rest = [];
  while length(rest) > 0 {
    arg  = head(rest);
    rest = tail(rest);
    val  = val(arg);
  }
  val;
})(3)(4);

// Unroll loop
({
  val  = func([1, 2]...);
  rest = [];
  val;
})(3)(4);

// Remove 'rest' and substitute 'val'
func([1, 2]...)(3)(4);

// Call 'func([1, 2]...)' AKA 'func(1, 2)'
(function(y) {
  return function(z) {
    return 1 + 2 + y + z;
  };
})(3)(4);

// Call '(function(y) {...})(3)'
(function(z) {
  return 1 + 2 + 3 + z;
})(4);

// Call '(function(z) {...})(4)'
1 + 2 + 3 + 4;

// Perform arithmetic
10
```

We get the same result in each case! Notice that in the second form,
`curriedF3(1)(2)(3)(4)`, our currying machinery was only used to plumb the `1`
and `2` arguments through to the right place; the `...(3)` and `...(4)` calls
were just normal function calls with no indirection or overhead.

This does have one important implication: if we return a function which *isn't*
in curried form, e.g. `function(a, b) {...}`, then it will only be called with
one argument. In order to work with our "uncurry loop", such functions need to
be wrapped up such that they can take one argument at a time. Yet that's easy to
achieve: we just need to curry them!

I've not yet decided if it's a good idea to automatically curry return values in
our loop, e.g.

```
val = curry3(val)(arg);
```

## Conclusion ##

Currying introduces wrapper-functions when a function is not given enough
arguments. In my opinion, we should also be *eliminating* wrapper-functions (by
calling them) when a function is given *too many* arguments.

This plugs a leak in the abstraction offered by many existing approaches to
currying, namely that `f(x)(y)` should be equivalent to `f(x, y)`. This lets us
hide more implementation details (the nesting structure of our functions),
simplify the calling conventions we use, and is one less thing to care about
when we're hacking.
