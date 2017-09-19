---
title: Partial Application & Currying in PHP
---
## History ##

It's been a while since I wrote about [currying] [1] in [Javascript] [2] and
I've been meaning to do the same in PHP. I've had a prototype PHP implementation
knocking around from the same time as my Javascript one, but I was never happy
with it, specifically because it took a function's parameter number
explicitly. In other words, whereas in Javascript we can do this:

[1]: http://en.wikipedia.org/wiki/Currying
[2]: /blog/2012-10-01-better_currying_in_javascript.html

```javascript
var my_func = curry(function(x, y) { return x + y; });
```

In PHP I would have to do this:

```php
<?
$my_func = $curry(2, function($x, $y) { return $x + $y; });
```

Note the explicit `<? 2`{.php}, which tells `<? $curry`{.php} how many
parameters to accumulate before calling the function. I've since revisited the
code and removed this restriction, as well as implementing a couple of
alternatives.

## Context ##

Suppose we want to do a bunch of processing to a value. One way we might do it is imperatively, like this:

```php
<?
$val2 = $function1($val );
$val3 = $function2($val2);
$val4 = $function3($val3);
$val5 = $function4($val4);
$val6 = $function5($val5);
$val7 = $function6($val6);
$val8 = $function7($val7);
return  $function8($val7);
```

This is clearly a mess, since we've got all kinds of intermediate results hanging around. There are a few ways we could change this. First of all, let's put our functions in an array so they're easier to deal with:

```php
<?
$functions = [$function1, $function2, $function3, $function4,
              $function5, $function6, $function7, $function8];
```

That's starting to look much cleaner, but now we need to chain them together. One way we can do this is with a loop:

```php
<?
foreach ($functions as $function) {
  $val = $function($val);
}
return $val;
```

The problem with this approach is that we're clobbering the contents of `<? $val`{.php}. Of course, we could introduce an extra variable like `<? $val2`{.php} but then we're creeping back to the mess we started with.

The root cause of our problems is that we're trying to calculate a value, ie. an *expression*, but we're using *statements* to do it. Statements are PHP's way of sequencing side-effects (eg. variable assignment), but we should be keeping side-effects to a minimum if we want our code to be easy to understand and reusable.

We can easily turn a loop statement into an expression by asking what it is the loop is doing. In this case, we're using the loop to turn a collection of values (`<? $functions`{.php}) into a single value (the result of sending `<? $val`{.php} through them). This is known as a *fold* or a *reduction*, so we can use PHP's [`array_reduce`] [3] function:

[3]: http://uk1.php.net/array_reduce

```php
<?
return array_reduce($functions,
                    function($result, $function) {
                      return $function($result);
                    },
                    $val);
```

Here we've managed to get rid of most statements, but we've had to write a whole new function for something which seems pretty basic: passing a value into a function. This is pretty ugly, but at least if we pull out this function definition we won't have to do it again:

```php
<?
$pass_into = function($x, $f) { return $f($x); }

return array_reduce($functions, $pass_into, $val);
```

In fact PHP does have a function very similar to `<? $pass_into`{.php}, known as [call_user_func] [4]. Unfortunately it takes its arguments the other way around:

[4]: http://php.net/manual/en/function.call-user-func.php

```php
<?
$pass_into = function($x, $f) { return call_user_func($f, $x); }

return array_reduce($functions, $pass_into, $val);
```

Since we always want our code to be as generic as possible, we're actually better off abstracting `<? $pass_into`{.php} into a more general-purpose argument-flipping function:

```php
<?
$flip1 = function($f) {
  return function($x, $y) use ($f) { return $f($y, $x); };
};

return array_reduce($functions, $flip1('call_user_func'), $val);
```

Notice that our 'flip' function has a special structure: we accept one argument `<? $f`{.php}, then we return a function which accepts another two arguments `<? $x`{.php} and `<? $y`{.php}, before finally calling `<? $f`{.php} with `<? $y`{.php} and `<? $x`{.php}. Why didn't we use any of the following definitions?

```php
<?
// Accepts $f, $x and $y at the same time
$flip2 = function($f, $x, $y) { return $f($y, $x); };

// Accepts $f and $x then waits for $y
$flip3 = function($f, $x) {
  return function($y) use ($f, $x) { return $f($y, $x); };
};

// Accepts $f and waits for $x, then waits for $y
$flip4 = function($f) {
  return function($x) use ($f) {
    return function($y) use ($f, $x) { return $f($y, $x); };
  };
};

// etc.
```

If turns out that our definition of `<? $flip1`{.php} coincides exactly with the way `array_reduce` handles its arguments, but the other `<? $flip`{.php}N functions don't. However, there's nothing *inherently* wrong with any of the `<? $flip`{.php}N functions, they may be perfectly appropriate in different circumstances.

So, should we keep all of them just in case? Well if we do, we will need to consider *even more* cases: notice that in all of the definitions above we are getting our result by calling `<? $f($y, $x)`{.php}, but what if `<? $f`{.php} doesn't take both of its arguments at once? That may sound unlikely, but notice that it's exactly what some of our `<? $flip`{.php}N functions do! For example:

```php
<?
call_user_func($flip1($flip4('array_map')),
               'intval', ['0', '1', '2']);
```

This (admittedly contrived) code *looks like* it will flip the arguments to [`<? array_map`{.php}] [5], then flip them back and act like the original array_map. However, it will actually give the following:

[5]: http://uk1.php.net/manual/en/function.array-map.php

```php
<?
class Closure#9 (2) {
  public $static =>
  array(2) {
    'f' =>
    string(9) "array_map"
    'x' =>
    array(3) {
      [0] =>
      string(1) "0"
      [1] =>
      string(1) "1"
      [2] =>
      string(1) "2"
    }
  }
  public $parameter =>
  array(1) {
    '$y' =>
    string(10) "<required>"
  }
}
```

This is the inner function of `<? $flip4`{.php}. Since we passed *both* arguments to `<? $flip4('array_map')`{.php} at the same time, they were *both* given to the inner function `<? function($x) use ($f) { ... }`{.php}, ie. the first argument was kept (and bound to `<? $x`{.php}) but the second was silently ignored.

How can we handle such cases? Well, we need *even more* `<? $flip`{.php}N functions to handle all of these possible calling conventions:

```php
<?
$flip5 = function($f, $x, $y) {
  return call_user_func($f($y), $x);
};

$flip6 = function($f) {
  return function($x, $y) use ($f) {
    return call_user_func($f($y), $x);
  };
};

$flip7 = function($f) {
  return function($x) use ($f) {
    return function($y) use ($f, $x) {
      return call_user_func($f($y), $x);
    };
  };
};

// etc.
```

Clearly we're going too far down a rabbit hole here. How about we step back and consider a different solution, rather than writing more and more code?

## Partial Application ##

We'll start with a simple, but rather weak, solution: partial application. Recently at work I wrote a helper function to recursively insert a value at a given location in an array/object, something like the following:

```php
<?
// Usage:         path             value collection
$x = array_insert(['a', 'b', 'c'], 42,   ['a' => new stdClass]);

// Result
$x['a']->b['c'] === 42

// In fact it's the inverse of an existing 'lookup' function, such that:
lookup(array_insert($path, $value, $collection), $path) === $value;
```

An implementation of `<? array_insert`{.php} is given below:

```php
<?
function array_insert($path, $val, $ao = array()) {
  if (!$path) return $val;

  $i = array_shift($path);

  if (is_object($ao)) {
    $ao->{$i} = array_insert($path,
                             $val,
                             isset($ao->{$i})? $ao->{$i} : array());
  }
  else {
    $ao  [$i] = array_insert($path,
                             $val,
                             isset($ao  [$i])? $ao  [$i] : array());
  }
  return $ao;
}
```

Clearly there is redundancy here, since the if/else branches *both* call `<? array_insert`{.php} with `<? $path`{.php} and `<? $val`{.php}. This violates [DRY] [6] (Don't Repeat Yourself), but it's difficult to see how we could remove the redundancy since it's a recursive call.

[6]: http://en.wikipedia.org/wiki/Don%27t_repeat_yourself

In fact, the way I solved this was to use partial application:

```php
<?
function array_insert($path, $val, $ao = array()) {
  if (!$path) return $val;

  $i = array_shift($path);
  $f = papply('array_insert', $path, $val);  // Partial application

  if (is_object($ao)) $ao->{$i} = $f(isset($ao->{$i})? $ao->{$i} : array());
  else                $ao  [$i] = $f(isset($ao  [$i])? $ao  [$i] : array());

  return $ao;
}
```

So what's going on here? Here's the `<? papply`{.php} function:

```php
<?
function papply() {
  $args = func_get_args();
  return function() use ($args) {
    return call_user_func_array('call_user_func',
                                array_merge($args, func_get_args()));
  };
}
```

`<? papply`{.php} will accept any number of arguments, since it uses [`<? func_get_args`{.php}] [7], then it returns a function also accepting any number of arguments. These two argument arrays are merged together and sent to `<? call_user_func`{.php}.

[7]: http://uk3.php.net/func_get_args

Here's a trivial example:

```php
<?
$replace_amps = papply('str_replace', '&', 'and');
```

The resulting closure, in this case `<? $replace_amps`{.php}, has access to all of the arguments we gave, in this case `<? str_replace`{.php}, `<? '&'`{.php} and `<? 'and'`{.php}.

Now we can pass some more arguments to this closure:

```php
<?
$my_string2 = $replace_amps($my_string1);
```

In this example, the merged arguments will be `<? ['str_replace', '&', 'and', $my_string1]`{.php}. The closure uses [`<? call_user_func_array`{.php}] [8] to pass these as the arguments to another function (a process known as [uncurrying] [9]). It just-so-happens to pass them all to `<? call_user_func`{.php}, which performs a regular function call. Let's step through its execution:

[8]: http://uk3.php.net/manual/en/function.call-user-func-array.php
[9]: http://www.haskell.org/hoogle/?hoogle=uncurry

```php
<?
$my_string2 = $replace_amps($my_string1);

// Substitute in the definition of $replace_amps
$my_string2 = call_user_func(
  papply('str_replace', '&', 'and'),
  $my_string1);

// Substitute in the definition of papply
$my_string2 = call_user_func(
  call_user_func(
    function() {
      $args = func_get_args();
      return function() use ($args) {
        return call_user_func_array(
          'call_user_func',
          array_merge($args, func_get_args()));
      };
    },
    'str_replace', '&', 'and'),
  $my_string1);

// Apply the inner function
$my_string2 = call_user_func(
  function() {
    return call_user_func_array(
      'call_user_func',
      array_merge(['str_replace', '&', 'and'], func_get_args()));
  },
  $my_string1);

// Apply the remaining function
$my_string2 = call_user_func_array(
  'call_user_func',
  array_merge(['str_replace', '&', 'and'], [$my_string1]));

// Apply array_merge
$my_string2 = call_user_func_array(
  'call_user_func',
  ['str_replace', '&', 'and', $my_string1]);

// Apply call_user_func_array
$my_string2 = call_user_func(
  'str_replace', '&', 'and', $my_string1);

// Apply call_user_func
$my_string2 = str_replace('&', 'and', $my_string1);
```

As you can see, the first argument we gave to papply (`<? str_replace`{.php}) will be called with the remaining arguments (`<? '&'`{.php}, `<? 'and'`{.php}) **and** the arguments we gave to the resulting closure (`<? $mystring1`{.php}).

The same thing happens to the recursive calls in `<? array_insert`{.php}:

```php
<?
if (is_object($ao)) $ao->{$i} = $f(isset($ao->{$i})? $ao->{$i} : array());
else                $ao  [$i] = $f(isset($ao  [$i])? $ao  [$i] : array());

// Substitute in the definition of $f
if (is_object($ao)) $ao->{$i} = call_user_func(
  papply('array_insert', $path, $val),
  isset($ao->{$i})? $ao->{$i} : array());
else $ao[$i] = call_user_func(
  papply('array_insert', $path, $val),
  isset($ao[$i])? $ao[$i] : array());

// Substitute in the definition of papply
if (is_object($ao)) $ao->{$i} = call_user_func(
  function() {
    return call_user_func_array(
      'call_user_func',
      array_merge(['array_insert', $path, $val], func_get_args()));
    };
  },
  isset($ao->{$i})? $ao->{$i} : array());
else $ao[$i] = call_user_func(
  function() {
    return call_user_func_array(
      'call_user_func',
      array_merge(['array_insert', $path, $val], func_get_args()));
    };
  },
  isset($ao[$i])? $ao[$i] : array());

// Apply call_user_func
if (is_object($ao)) $ao->{$i} = call_user_func_array(
  'call_user_func',
  array_merge(['array_insert', $path, $val],
              [isset($ao->{$i})? $ao->{$i} : array()]));
else $ao[$i] = call_user_func_array(
  'call_user_func',
  array_merge(['array_insert', $path, $val],
              [isset($ao[$i])? $ao[$i] : array()]));

// Apply array_merge
if (is_object($ao)) $ao->{$i} = call_user_func_array(
  'call_user_func',
  ['array_insert', $path, $val,
   isset($ao->{$i})? $ao->{$i} : array()]);
else $ao[$i] = call_user_func_array(
  'call_user_func',
  ['array_insert', $path, $val,
   isset($ao[$i])? $ao[$i] : array()]);

// Apply call_user_func_array
if (is_object($ao)) $ao->{$i} = call_user_func(
  'array_insert', $path, $val,
  isset($ao->{$i})? $ao->{$i} : array());
else $ao[$i] = call_user_func(
  'array_insert', $path, $val,
  isset($ao[$i])? $ao[$i] : array());

// Apply call_user_func
if (is_object($ao)) $ao->{$i} = array_insert(
  $path, $val, isset($ao->{$i})? $ao->{$i} : array());
else $ao[$i] = array_insert(
  $path, $val, isset($ao[$i])? $ao[$i] : array());
```

We've recovered the original, clunky code :)

In particular, notice that if we have a function which takes two arguments, like
`<? $f = function ($x, $y) { ... }`{.php}, then `papply` lets us give each
argument in a separate step, e.g.

```php
<?
$g = papply($f, $x);
$g($y)
```

We can go even further by splitting the `<? papply($f, $x)` call into two
separate steps as well, using *another* call to `papply`:

```php
<?
$h = papply('papply', $f);
```

The resulting `<? $h`{.php} function will accept one set of arguments (e.g.
`<? $x`{.php}), then another set of arguments (e.g. `<? $y`{.php}), just as if
it were defined as
`function($x) { return function($x) use ($x) { ... }; }`{.php}.

Turning a function with one set of arguments into a function with two sets of
arguments is really useful, so we can make a generic function to do this
transformation. How? By using `<? papply`{.php} *again*!

```php
<?
$splitArgs = papply('papply', 'papply');
```

With this general-purpose function, the above simplifies to:

```php
<?
$h = $splitArgs($f);
```

To see why this function is so useful, think back to our proliferation of
`<? $flip`{.php} functions: the reason we needed so many was because their
calling conventions differed in exactly the ways that `<? $splitArgs`{.php} can
convert between. With `<? papply`{.php} and `<? $splitArgs`{.php} in our tool
box, let's see how if we can simplify the `<? $flip`{.php} functions:

```php
<?
// This is the only $flip function we need for $f($y, $x)
$flip = function($f, $x, $y) { return $f($y, $x); };

// We can implement the others via simple transformations
$flip1 = $splitArgs($flip);

$flip2 = $flip;

$flip3 = $flip1;

$flip4 = $splitArgs($flip1);

// This is the only $flip we need for call_user_func($f($y), $x)
$flip5 = function($f, $x, $y) {
  return call_user_func($f($y), $x);
};

$flip6 = $splitArgs($flip5);

$flip7 = $splitArgs($flip6);
```

One of the reasons we still need two forms of `<? $flip`{.php} is because these
partially-applied functions are much weaker than proper curried ones which I'll
explain further down. On the other hand, their advantage is predictability: each
call to `<? papply`{.php} will delay a function call exactly once, which means a
single lambda will be generated. This is important in PHP since we don't
have [tail-call optimisation] [10], so we always have to worry about overflowing
the stack.

[10]: http://en.wikipedia.org/wiki/Tail-call_optimisation

Another place where this can be especially useful is in
our [array combinators] [11]. For example, we can turn the following:

[11]: http://www.giorgiosironi.com/2010/02/stop-writing-foreach-cycles.html

```php
<?
$a = array_map(function($x) { return str_replace('&', 'and', $x); },
               $my_array1);

$b = array_filter($my_array2,
                  function($x) { return preg_match('/[0-9]+/', $x); });
```

Into:

```php
<?
$a = array_map(papply('str_replace', '&', 'and'), $my_array1);

$b = array_filter($my_array2, papply('preg_match', '/[0-9]+/'));
```

This is known as [eta-reduction] [12], and it turns up all over the place in
PHP. For example, today I was reading the documentation for [Silex] [13]. Here
are some of their examples which can be easily eta-reduced:

[12]: http://www.lambda-bound.com/book/lambdacalc/node21.html
[13]: http://silex.sensiolabs.org/documentation

```php
<?
// Original
function ($id) { return (int) $id; }

// Eta-reduced
'intval'

// Original
function () use ($app) {
  return $app->redirect('/hello');
}

// Eta-reduced
papply([&$app, 'redirect'], '/hello')

// Original
function () use ($app) {
  // redirect to /hello
  $subRequest = Request::create('/hello', 'GET');

  return $app->handle($subRequest, HttpKernelInterface::SUB_REQUEST);
}

// Eta-reduced
papply([&$app, 'handle'], Request::create('/hello', 'GET'),
                          HttpKernelInterface::SUB_REQUEST)

// Original
function () use ($file) {
    readfile($file);
}

// Eta-reduced
papply('readfile', $file)
```

## Proper Currying ##

Using `<? papply`{.php} at work prompted me to have another stab at a more
general currying function, since I realised I could use [reflection] [14] to
overcome the parameter number issue. We can still use the `<? $curry`{.php}
function shown near the beginning, with its explicit parameter number, but I've
renamed it to `<? $curry_n`{.php} (ie. 'curry N parameters'). The reflective
replacement is simply:

[14]: http://en.wikipedia.org/wiki/Reflection_(computer_programming)

```php
<?
$curry = $curry_n(1, function($f) use ($curry_n) {
  return $curry_n(call_user_func([new ReflectionFunction($f),
                                  'getNumberOfParameters']), $f);
});
```

The `<? $curry`{.php} function will take a function `<? $f`{.php} and pass it to `<? $curry_n`{.php}, looking up the parameter number via reflection. Even though the `<? $curry`{.php} function only takes 1 parameter, we also curry it for reasons explained in the **Returning Functions** section.

"So what is the definition of `<? $curry_n`{.php}?" I hear you cry. Here it is:

```php
<?
// Curries n-argument functions
$curry_n = call_user_func(function() {
    // An uncurried version of $curry_n, encapsulated in this lambda
    $_curry_n = function($n, $f) {
        // An acceptor gathers $n arguments then calls $f
        $make_acceptor = function($args) use ($n, $f, &$make_acceptor) {
            return function() use ($n, $f, $args, &$make_acceptor) {
                $args2 = array_merge($args, func_get_args());
                if (count($args2) >= $n) {
                    // Send $n args to $f
                    $ret = call_user_func_array($f, array_slice($args2, 0, $n));
                    // Send any remaining args to the return value
                    return (count($args2) > $n)
                      ? call_user_func_array($ret, array_slice($args2, $n))
                      : $ret;
                }
                // Otherwise make an acceptor storing these arguments
                return $make_acceptor($args2);
            };
        };
        // A curried function is just an acceptor without any args
        return $make_acceptor([]);
    };

    // Only expose a curried version of $curry_n
    return $_curry_n(2, $_curry_n);
});
```

## Motivation ##

So what does currying get us, compared to not currying (I'll ignore partial application until the **Partial Application vs Currying** section)? It removes the arbitrary distinction PHP makes between functions with 'different numbers of arguments'. As well as all the different `<? $flip`{.php} functions above, PHP will distinguish between functions like these three:

```php
<?
$foo1  = 'foo';

$foo2 = function($x, $y) {
          return foo($x, $y);
        };

$foo3 = function($x) {
          return function($y) use ($x) {
            return foo($x, $y);
          };
        };
```

These all call the `<? foo`{.php} function, but they differ in how they take their arguments:

 - `<? $foo1`{.php} will accept any number of arguments and pass them all to `<? foo`{.php}
 - `<? $foo2`{.php} will accept any number of arguments and pass the first two to `<? foo`{.php}
 - `<? $foo3`{.php} will accept any number of arguments but keep only the first; it returns a function which likewise accepts any number of arguments and only keeps the first. The two arguments that were kept are passed to `<? foo`{.php}

I discuss `<? $foo1`{.php} in the **Taking All The Arguments** section; for now let's compare `<? $foo2`{.php} with `<? $foo3`{.php}. The first thing to spot is that we can replace every occurrence of `<? $foo2($a, $b)`{.php} with a call to `<? call_user_func($foo3($a), $b)`{.php} but we *cannot* replace occurrences of `<? $foo3`{.php} using `<? $foo2`{.php}, unless we create a wrapper equivalent to `<? $foo3`{.php} (this is what `<? papply`{.php} would do, for example). Hence, out of these two options, we should always prefer to use `<? $foo3`{.php}.

Unfortunately, functions like `<? $foo3`{.php} cause a few problems:
 1) The amount of boilerplate is greatly increased, since every abstraction must be delimited with the `<? function`{.php} and `<? return`{.php} keywords and we must explicitly list all of our free variables with `<? use`{.php}.
 2) Applying several abstractions at the same time is painful, since PHP's lexer doesn't accept chained function calls, ie. `<? $foo3(10)(20)`{.php}, which forces us to reify each application with `<? call_user_func`{.php}.
 3) There are pretty much no PHP libraries/frameworks/etc. which do this, so we would be forced to switch back and forth between each convention.

These problems would all be solvable if PHP had some kind of abstraction mechanism to encapsulate duplicate code in a reusable component. Thankfully it does, they're called functions! In fact *the `<? $curry`{.php} and `<? $curry_n`{.php} functions defined above solve all of these problems!*

## `<? $curry`{.php} ##

The `<? $curry`{.php} function converts an uncurried function like `<? $foo2`{.php} into a curried form like `<? $foo3`{.php}. This solves half of problem (3), since it lets us curry existing libraries and frameworks whenever we like.

Rather than accepting one argument at a time, like `<? $foo3`{.php}, using `<? $curry`{.php} and `<? $curry_n`{.php} actually lets us give *any* number of arguments at a time, including all of them at once. In other words, we can still use PHP's regular calling convention `<? $blah($x, $y, $z)`{.php} for curried functions. This solves problem (2) since we don't need to use `<? call_user_func`{.php} to supply extra arguments, and it also solves the remaining half of problem (3) since existing libraries and frameworks will work perfectly well if we give them curried functions.

What's more, the `<? $curry`{.php} and `<? $curry_n`{.php} functions allow us to write our own functions in an *non-curried* style (avoiding problem (1)), which we can then pass through `<? $curry`{.php} to get the behaviour we want.

For example, let's say we need some arithmetic functions. PHP doesn't have any of the basic functions, so we need to write our own:

```php
<?
$curry_all = papply('array_map', $curry);

list($sum, $product, $subtract, $divide) = $curry_all([
  function($x, $y) { return $x + $y; },
  function($x, $y) { return $x * $y; },
  function($x, $y) { return $x - $y; },
  function($x, $y) { return $x / $y; }]);
```

These definitions are non-curried, like `<? $foo2`{.php}, since they take two arguments and don't have any inner functions, but by passing them through `<? $curry`{.php} we've curried them to behave like `<? $foo3`{.php}, accumulating arguments until they have two. For example:

```php
<?
// Apply an argument to each
$increment  = $sum(1);
$negate     = $subtract(0);
$double     = $product(2);
$reciprocal = $divide(1);

// Some invariants
list($increment_test,
     $negate_test,
     $double_test,
     $reciprocal_test) = $curry_all([
       function($x) use ($increment) {
         return $increment($x) === 1 + $x;
       },
       function($x) use ($negate) {
         return $negate($x) === 0 - $x;
       },
       function($x) use ($double) {
         return $double($x) === 2 * $x;
       },
       function($x) use ($reciprocal) {
         return $reciprocal($x) === 1 / $x;
       }]);

// We can still use the regular calling convention if we like
$sum     (10, 20) === 30;
$product (3 , 4 ) === 12;
$subtract(10, 3 ) === 7;
$divide  (10, 2 ) === 5;
```

What the `<? $curry`{.php} function does, as mentioned above, is pass its first argument to `<? $curry_n`{.php} along with its number of named parameters (ie. explicit arguments). It turns out we don't always want to use this number, since many PHP functions use more (via `<? func_get_args`{.php}) or fewer (via default values). In these cases we can call `<? $curry_n`{.php} directly.

## `<? $curry_n`{.php} ##

The `<? $curry_n`{.php} function is the heart of the `<? $curry`{.php} function but can also be used standalone. It accepts a number `<? $n`{.php} and a function `<? $f`{.php} which it encapsulates in a closure, along with array `<? $args`{.php} of arguments which is initially empty. These closures are "acceptors" which only exist to accept arguments.

If we ever call an acceptor, we will receive back another acceptor encapsulating the same `<? $n`{.php} and `<? $f`{.php}, but its `<? $args`{.php} array will *also* have all of the arguments we passed in (if any) pushed on to the end. Note that the original acceptor is *not affected in any way*; the extra arguments are only stored in the *new* acceptor we received back.

This new acceptor will behave exactly like the first, except for the extra elements in its `<? $args`{.php} array. If we pass it some more arguments, we get back another *new* acceptor which has those arguments pushed on to the end of *its own* `<? $args`{.php} array, and so on.

We can keep calling these acceptors as many times as we like, in any order, passing in any amount of arguments we like. When any of the acceptors manage to accumulate `<? $n`{.php} arguments, those arguments will be sent to the function `<? $f`{.php} and the result will be returned (instead of a new acceptor).

Whenever `<? $f`{.php} is called, *nothing about any of the acceptors changes*. We can carry on calling them in any order with any number of arguments, just like before.

A couple of reasons why we might want to use `<? $curry_n`{.php} instead of `<? $curry`{.php} are:

 - We want to use some of `<? $f`{.php}'s default parameters, but `<? $curry`{.php} makes us provide a value for each one ourselves. In this case, we can use `<? $curry_n`{.php} with the number of parameters we actually want to supply.
 - `<? $f`{.php} uses `<? func_get_args`{.php} to look up a variable number of arguments, so it may not have any named parameters at all. Again, we can use `<? $curry_n`{.php} to tell it how many parameters we want to supply.

If we didn't use `<? $curry_n`{.php} for these cases, we would either end up calling our function without enough parameters, or else we would get back another acceptor when we expect a result.

Here's an example of `<? $curry_n`{.php}, where we want to implode `<? $n`{.php} strings:

```php
<?
// Some helper functions

// Takes an argument $n, then wraps the next $n arguments in an array.
// We use $curry_n on the inner function since it uses func_get_args.
$array = $curry(function($n) use ($curry_n) {
                  return $curry_n($n, function() { return func_get_args(); });
                });

// Takes an $n and composes a function $f with an $n-ary function $g.
// We use $curry_n on the inner function since it uses func_get_args.
$compose_n = $curry(function($n, $f, $g) use ($curry_n) {
                      return $curry_n($n, function() use ($f, $g) {
                        return $f(call_user_func_array($g, func_get_args()));
                      });
                    });

// Takes $n arguments and implodes them with spaces
$space_maker = $curry(function($n) use ($compose_n, $curry, $array) {
                        return $compose_n($n, $curry('implode', ' '),
                                              $array($n));
                      });

// Call $space_maker(10) in various equivalent ways

$sm_            = $space_maker(10);
$sm0            = $space_maker(10, 'zero');
$sm0123         = $sm0('one', 'two', 'three');
$sm_012345      = $sm_('zero', 'one', 'two', 'three', 'four', 'five');
$sm0123456      = $sm0123('four', 'five', 'six');
$sm_012345_     = $sm_012345();
$sm_0123456789  = $sm_('zero', 'one', 'two', 'three', 'four', 'five',
                       'six', 'seven', 'eight', 'nine');
$sm0123456789   = $sm0123456('seven', 'eight', 'nine');
$sm_012345_6789 = $sm_012345_('six', 'seven', 'eight', 'nine');

echo "$sm0123456789\\n";
echo "$sm_0123456789\\n";
echo "$sm_012345_6789\\n";
echo $space_maker(10, 'zero', 'one', 'two', 'three', 'four', 'five',
                  'six', 'seven', 'eight', 'nine');
```

This will print:

```
zero one two three four five six seven eight nine
zero one two three four five six seven eight nine
zero one two three four five six seven eight nine
zero one two three four five six seven eight nine
```

Since the `<? $space_maker`{.php} function is curried, we're free to call it however we like and as long as it eventually gets enough arguments (10 in this case) it will produce the imploded result.

## Partial Application vs Currying ##

Currying is very similar to partial application shown above:

 - Both will delay the evaluation of a function
 - Both will work with regular, uncurried functions
 - Both are idempotent
 - If we don't specify any extra arguments when we curry or partially-apply a function, the resulting closure will behave the same as the original function (although reflection will show them to differ).

However, currying is strictly more powerful:

 - A partially applied function lets us specify some arguments now and the rest later, whereas a curried function will keep accepting batches of arguments again and again until it reaches its threshold $n.
 - If we don't specify any extra arguments when we curry a function, we can *still* pass it batches of arguments.
 - The above makes it convenient to *curry all functions as they're defined*.
 - A curried function *cannot be given an invalid number of arguments* (although you may give it an *incorrect* number of arguments)

There is one more trick to currying compared to partial application, and that's what happens when we give *too many* arguments.

## Returning Functions ##

So far we've seen that `<? $curry`{.php} and `<? $curry_n`{.php} can automatically turn a function like this:

```php
<?
$bar1 = function($x, $y) { return $x + $y; };
```

Into a function like this:

```php
<?
$bar2 = function($x) {
          return function($y) use ($x) { return $x + $y; };
        };
```

However *it converts the other way too*! Our curried functions are much more powerful than `<? $bar1`{.php} or `<? $bar2`{.php}, since they let us use PHP's regular calling convention `<? $blah($x, $y)`{.php} on **both**.

Specifically **if your curried function returns another function, it will be all be treated as one**. Here's an example:

```php
<?
$output_if_both_positive = $curry(function($x, $y) {
  return ($x > 0 && $y > 0)? function() {
                               var_dump(func_get_args());
                               return TRUE;
                             }
                           : FALSE;
});

// Returns a closure
$output_if_both_positive(10);

// Returns FALSE
$output_if_both_positive(10, -4);

// Outputs ['hello', 'world'] and returns TRUE
$output_if_both_positive(10, 20, 'hello', 'world');
```

Note that this will work *even if the returned function is not curried*, since it will be passed *all* remaining arguments. However, it is especially useful when your returned functions *are* curried, because in that case they will only take as many arguments as they need and will pass the rest on to the *next* returned function.

This makes our life an awful lot simpler, since we no longer need to care about using `<? call_user_func`{.php}, remembering which parameters need to go to which function, etc. We just need to curry all of our functions when we define them, then throw all of our arguments to the first function we call and it will send them all to the right place. This is another reason why it's important to set the right number of parameters, eg. via `<? $curry_n`{.php}; if we don't, our acceptors won't know which arguments should be sent to their encapsulated function `<? $f`{.php} and which should be sent to its return value.

## Taking All The Arguments ##

There is one unanswered question: how do we implement `<? $foo1`{.php} if all of our functions are curried? Specifically, how do we accept *all* subsequent arguments?

This sounds tricky, but it's actually very simple. As far as functions like `<? $foo1`{.php} are concerned, only the first batch of arguments matters, since `<? $foo1`{.php} is not curried so it can't accept any more batches. Of course, `<? foo`{.php} could be curried, but then there would be no problem ;)

All we need to do is wrap our curried n-ary function in a non-curried function:

```php
<?
// If we're given a curried function like this
$foo4 = $curry('foo');

// We can wrap it in a non-curried function like this
$foo5 = function() use ($foo4) {
          return call_user_func_array($foo4, func_get_args());
        };
```

`<? $foo5`{.php} will accept all of the arguments it is given when called, then pass them on to `<? $foo4`{.php} which will either:

 - Accept them all then wait for more, since it's threshold hasn't been reached
 - Accept them all, call `<? foo`{.php} and return the result, if the number of arguments equals the threshold
 - Accept enough to call `<? $foo4`{.php}, then pass the rest to whatever return value it gets

What if we want to pass all of our (first batch of) arguments directly to `<? foo`{.php}, but we would like to behave like a curried function thereafter? In that case we just need to do the currying once we've got our arguments:

```php
<?
$foo6 = function() use ($curry_n) {
          $args = func_get_args();
          return call_user_func_array($curry_n(count($args), 'foo'),
                                      $args);
        };
```

## Drawbacks ##

Besides trying to further simplify the currying implementation itself, there are still some issues with this.

It's nice that curried functions 'take care of themselves', but this makes their stack usage a bit more difficult to predict, especially if we're currying all of our functions as we define them. As mentioned above, this is a problem since PHP doesn't do tail-call optimisation. Some ways our stack usage may increase unnecessarily are:

 - Calling a curried function without any arguments will give us back an equivalent function which uses one more stack frame.
 - Curried functions which return curried functions which return curried functions, etc. can be called all at once, as the **Returning Functions** section demonstrates, but each subsequent call will be further down the stack.
 - Currying a function multiple times, eg. to keep adjusting its argument number, will add to its stack usage.

Since PHP treats named functions differently to lambdas, it is more difficult than I'd like to curry a named function. In particular, we have to explicitly eta-expand the definitions of our named functions, in order to handle our arguments properly. For example, something like this:

```php
<?
function curry_n() {
  $_curry_n = function($n, $f) {
    $make_acceptor = function($args) use ($n, $f, &$make_acceptor) {
      return function() use ($n, $f, $args, &$make_acceptor) {
        $args2 = array_merge($args, func_get_args());
        if (count($args2) >= $n) {
          $ret = call_user_func_array($f, array_slice($args2, 0, $n));
          return (count($args2) > $n)
            ? call_user_func_array($ret, array_slice($args2, $n))
            : $ret;
        }
        return $make_acceptor($args2);
      };
    };
    return $make_acceptor([]);
  };

  // Call $_curry_n(2, $_curry_n) with our arguments; ie. we're an
  // eta-expansion of $_curry_n(2, $_curry_n)
  return call_user_func_array(
    $_curry_n(2, $_curry_n), func_get_args());
}

function curry() {
  // Call curry_n(...) with our (remaining) arguments, ie. we're an
  // eta-expansion of curry_n(...)
  $args = func_get_args();
  $f    = array_shift($args);
  return call_user_func_array(
    curry_n(call_user_func([new ReflectionFunction($f),
                           'getNumberOfParameters']), $f),
    $args);
}

function flip() {
  // Call curry(...) with our arguments, ie. we're an eta-expansion of
  // curry(...)
  return call_user_func_array(
    curry(function($f, $x, $y) { return $f($y, $x); }),
    func_get_args());
}
```

Of course, the idealist in me would avoid named functions altogether, since they're global and globals should always be avoided. However, the alternatives are:

 - Using static class properties, which require indirection to call (PHP assumes `<? Foo::bar()`{.php} is a method call), and classes are still global so we don't gain much
 - Lexically scoping everything, but PHP's `<? use`{.php} requirement gets old quickly and it would be nice if the ability to curry didn't force a particular application architecture on us

For these reasons, I've not used this currying code in a production system yet.

On the other hand, I've found `<? papply`{.php} to be extremely useful :)
