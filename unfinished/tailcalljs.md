---
title: Tail-Call Optimisation via Currying, in Javascript
---

## Tail-Call Optimisation ##

Whenever I'm using a language without tail-call optimisation, I usually find myself wanting it. As you might guess, tail-call optimisation applies to 'tail-calls' (or 'tail-recursion', if we're calling ourselves). Roughly, a 'tail-call' is anything of the form `return my_function(x, y, z);`{.javascript}. We might interpret this tail-call as saying:

> Go and calculate `my_function`{.javascript} with `x`{.javascript}, `y`{.javascript} and `z`{.javascript}. Once we have the result, return it.

Whilst correct, that's quite a naïve perspective. Here's an example of some nested tail-calls, with the functions being tail-called in-lined for simplicity:

```javascript
foo();
return (function() {
  bar();
  return (function() {
    baz();
    return (function() {
      quux();
      return 42;
    }());
  }());
}());
```

We can only nest calls like this to a finite-depth before exhausting the memory. Most languages bail out much earlier than that, after 100 or 1000 levels.

Why is this naïve? Well, the above execution path is clearly equivalent (ignoring backtraces, exception handling, etc.) to the following:

```
foo();
bar();
baz();
quux();
return 42;
```

This is what tail-call optimisation does: use the relationship between function calls and return statements to collapse nested tail-calls into an equivalent form without nesting. With tail-call optimisation, there is no limit to how much we nest our calls: tail-calls never increase the size of the stack, so they can never cause it to overflow. With this in mind, we can go back to our tail-call example `return my_function(x, y, z);`{.javascript} and use the new interpretation:

> My result is the same as that of `my_function`{.javascript} applied to `x`{.javascript}, `y`{.javascript} and `z`{.javascript}

As a language implementor, this gives us more insight: when we push a new frame on to the stack to calculate `my_function(x, y, z)`{.javascript}, rather than pushing it *on top of* the current frame, instead we can *replace* the current frame, since it's no longer needed. The definition of this function's return value no longer depends on this function, only on `my_function`{.javascript}!

## Example ##

Let's say we write a simple factorial function:

```javascript
function factorial(n) {
  if (n < 2) return 1;
  return n * factorial(n-1);
}
```

This is *not* a tail-call, since we're not immediately returning the result of `factorial(n-1)`{.javascript}. `factorial(n)` will use O(`n`{.javascript}) stack frames, and hence overflow for large `n`{.javascript}.

We can write an alternative implementation which *does* use a tail-call, by adding an *accumulator* argument. The accumulator lets us multiply *arguments* instead of *return values*. Now that we're not altering our return values, we can return them immediately with a tail-call:

```javascript
function factorial_helper(acc, n) {
  if (n < 2) return acc;
  return factorial_helper(n * acc, n-1);
}

function factorial(n) {
  return factorial_helper(1, n);
}
```

Both `factorial`{.javascript} and `factorial_helper`{.javascript} are now tail-recursive, so they will run in 1 stack frame regardless of how large `n`{.javascript} is.

## Implementing Tail-Call Optimisation ##

All this talk of stack frames is pretty useless if we're working *inside* a language without tail-call optimisation, rather than in the meta-level world of interpreters and compilers. Can we write tail-call optimisation as a *library*?

We can, but it requires explicit invocation; we can't just import a module and have *existing* code automatically optimised (well, without some horrible monkey-patching at least!).

There are many existing approaches, eg. using 'trampolines', but I want to do something a little different: I want to obtain tail-call optimisation as a *consequence of currying*.

## Currying ##

My approach to currying differs from others in the way we handle *too many arguments*. My approach has the nice consequence that it simultaneously *curries* uncurried functions *and* it *uncurries* 'manually' curried functions.

Here's a quick recap:

Many languages, like Javascript, force functions to use one of three equivalent APIs:

A "normal" function looks like this:

```javascript
function foo(x, y, z) {
  return x + y + z;
}
foo(3, 5, 7);  // 15
```

A *curried* function looks like this:

```javascript
function foo(x) {
  return function(y) {
    return function(z) {
      return x + y + z;
    };
  };
}
foo(3)(5)(7);  // 15
```

An *uncurried* function looks like this:

```javascript
function foo(args) {
  return args[0] + args[1] + args[2];
}
foo([3, 5, 7]);  // 15
```

"Normal" functions are the most pleasant to *write*, but they're far less flexible than curried or uncurried functions. In fact, what I've called "normal" functions are also uncurried, although it's done at the language level using 'argument lists'. The form I've called "uncurried" is done at the library level using regular array values. In a sense, the "uncurried" function above is "more uncurried" than the "normal" function.

We can convert between these APIs by *currying* and *uncurrying*. For the sake of clarity, from now on I'll only use the term "uncurry" to mean library-level uncurrying. I'll call conversion from 'curried' to 'normal' as 'normalising curried functions' and from 'uncurried' to 'normal' as 'normalising uncurried functions'.

We can write higher-order functions `curry`{.javascript} and `uncurry`{.javascript} to transform between these APIs, such that:

```javascript
function foo(x, y, z) {
  return x + y + z;
}
curry(foo)(3)(5)(7);     // 15
uncurry(foo)([3, 5, 7])  // 15
```

This is all pretty standard stuff.

Where it gets interesting is that in some languages, like Javascript, we can make `curry`{.javasript} smart enough to allow *multiple* APIs:

```javascript
function foo(x, y, z) {
  return x + y + z;
}
var cfoo = curry(foo);

cfoo(3)(5)(7);  // 15
cfoo(3, 5)(7);  // 15
cfoo(3)(5, 7);  // 15
cfoo(3, 5, 7);  // 15
```

Functions curried in this "smart" way seem to be curried "on demand", based on how they're used. In fact, it's the other way around: they get completely curried, then normalised "on demand". This makes smartly curried functions, in a sense, *more general* than normal functions. For this reason, I tend to smartly curry every function I write, unless I have a reason not to.

We can make a slightly smarter `uncurry`{.javascript} too, but I'm going to ignore `uncurry`{.javascript} from now on since it provides us with fewer "on-demand" opportunities. This is because we can't, in general, tell the difference between an uncurried function and a one-argument function, or between an array of arguments and a single argument which just-so-happens to be an array.

Again, this is all pretty standard stuff. Why am I writing about it? Well, *my* approach to currying is, in a sense, *even smarter* than the standard approach. I've written before about how it simultaneously curries normal functions *and* normalises curried functions:

```javascript
function foo(x) {
  return function(y) {
    return function (z) {
      return x + y + z;
    };
  };
}

var cfoo = curry(foo);
cfoo(3)(5)(7); // 15
cfoo(3, 5)(7); // 15
cfoo(3)(5, 7); // 15
cfoo(3, 5, 7); // 15
```

Due to the "on-demand" way we normalise, it works even on functions which are already curried 'manually' (ie. defined in a curried form, rather than converted by `curry`{.javascript}).

Note that this is 'dynamic'; any time a "smart curried" function returns another function, we can normalise them "on-demand" to act like one big definition, no matter how convoluted the execution path. In other words, we're not just checking the source code for `return function(...`{.javascript}:

```javascript
var foo = curry(function(x, y, z) {
                  return x + y + z;
                });

var bar = curry(function(a, b, c) {
                  return a * b * c;
                });
var baz = curry(function(x, y) {
                  return (x == y)? foo : bar;
                });
baz(1, 1, 3, 5, 7);  // 15,  since 1 == 1 and foo(3, 5, 7) == 15
baz(1, 2, 3, 5, 7);  // 105, since 1 != 2 and bar(3, 5, 7) == 105
```

So that's the recap of currying over.

## Tail-Call Optimisation via Currying ##

It turns out that my version of "smart" currying *also* implements a form of tail-call optimisation!

Let's revisit the last example, but we'll keep both the normal and curried versions, for the sake of comparison:

```javascript
function foo(x, y, z) {
  return x + y + z;
}
var cfoo = curry(foo);

function bar(a, b, c) {
  return a * b * c;
}
var cbar = curry(bar);

function baz(x, y) {
  return (x == y)? cfoo : cbar;
}
var cbaz = curry(baz);

cbaz(1, 1, 3, 5, 7);  // 15
cbaz(1, 2, 3, 5, 7);  // 105
```

When we call, for example, `cbaz(1, 1, 3, 5, 7)`{.javascript}, this is converted to `baz(1, 1)(3)(5)(7)`{.javascript}. Since `baz(1, 1)`{.javascript} returns the function `cfoo`{.javascript}, this is equivalent to `cfoo(3)(5)(7)`{.javascript}. In turn, `cfoo(3)(5)(7)`{.javascript} becomes `foo(3, 5, 7)`{.javascript} which returns 15.

This is implemented in a very straightforward way:

```javascript
function curry_(args, n, f) {
  return (args.length < n)? function() {
                              return curry_(args.concat(as_array(arguments)),
                                            n,
                                            f);
                            }
                          : args.slice(n)
                                .reduce(call,
                                        f.apply(null, args.slice(0, n)));
}

var curry = curry_([], 1, function(f) {
                            return curry_([], f.length, f);
                          });

```

The `curry_`{.javascript} function does all of the work: it builds up `args`{.javascript} until it contains at least `n`{.javascript} elements, then sends them to `f`{.javascript}. That's the standard part. Where my implementation differs is that only the first `n`{.javascript} elements are used. The rest, if any, are passed into the *return value*, *one at a time*. This is why we end up with chains like `baz(1, 1)(3)(5)(7)`{.javascript} above, since `baz`{.javascript} only requires 2 arguments so we only send it 2 arguments. Every other implementation of currying I've seen will send *all* arguments, resulting in calls like `baz(1, 1, 3, 5, 7)`{.javascript}, in which case the last 3 values will be ignored.
