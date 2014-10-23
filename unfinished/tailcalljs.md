---
title: Javascript Tail-Call Optimisation
---
Whenever I'm using a language without tail-call optimisation, I usually find myself wanting it.

There are other functional features which can be hacked on to a language using a library after-the-fact, such as [currying] (), can we do the same with TCO?

It turns out that we can, by augmenting the currying functions I've previously blogged about.

Specifically, the unique feature of my implementations is that we don't pass all arguments straight to the wrapped function; we only pass it the number that it asked for. If there are some left over, we pass them to the return value.

This has the nice consequence that we can also *uncurry* any "manually curried" functions, like this:

```javascript
// A ridiculous example
var func1 = function(a, b, c) {
  return function(d, e) {
    return function(f, g) {
      return [a, b, c, d, e, f, g];
    };
  };
};

// Curry it
var cfunc1 = curry(func1);

// cfunc1 is now curried
cfunc1('0')('1')('2') == func1('0', '1', '2')

// cfunc1 can still be treated as an uncurried function
cfunc('0', '1', '2') == func1('0', '1', '2')

// cfunc1 and its return value also act like an uncurried function
cfunc1('0', '1', '2', '3', '4') == func1('0', '1', '2')('3', '4')

// The previous implementations stopped there, but we would like to go
// further and uncurry all return values:
cfunc1('0', '1', '2', '3', '4', '5', '6') ==
 func1('0', '1', '2')('3', '4')('5', '6') ==
      ['0', '1', '2', '3', '4', '5', '6']
```

How can we implement such a thing? By looping until we exhaust our arguments:

```javascript

```
