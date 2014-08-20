---
title: Using Plumb
---
[Plumb](index.html) is a simple language for defining simple functions. This page describes Plumb's syntax and semantics, wherever they differ from those of the [host language](implementations.html).

## Declaring Functions ##

Plumb exists to make function definition easy: just use [square brackets]. Functions will return their argument by default, so an empty pair of brackets is an [identity function](http://en.wikipedia.org/wiki/Identity_function): `[]`{.python}

+------------------------------------------------------+----------------+
| ```php                                               |                |
| <?                                                   |                |
| $id = plumb([]);                                     | **PHP**        |
| echo "Called {$id(__FUNCTION__)} at  {$id(time())}"; |                |
| ```                                                  |                |
+------------------------------------------------------+----------------+
| ```javascript                                        |                |
| elements.map(function process(x) {                   |                |
|                var result = real_process(x);         |                |
|                // Stop processing if finished        | **Javascript** |
|                if (result) process = plumb([]);      |                |
|                return result;                        |                |
|              });                                     |                |
| ```                                                  |                |
+------------------------------------------------------+----------------+

## Returning Values ##

A return value is just an expression written inside the brackets, in Plumb, the host language or a mixture of both. If there's any ambiguity, we assume you're writing Plumb. This makes it trivial to write [thunks](http://en.wikipedia.org/wiki/Thunk#Functional_programming):

+------------------------------------------------+---------+
| ```php                                         |         |
| <?                                             |         |
| public function testInvalidThrowsException() { |         |
|   try {                                        |         |
|     $this->handler                             |         |
|          ->handleWith(plumb([false]), rand()); | **PHP** |
|     $this->fail('Threw exception');            |         |
|   } catch (InvalidException $e) {}             |         |
| }                                              |         |
| ```                                            |         |
+------------------------------------------------+---------+

## Function Arguments ##

Every Plumb function takes a single argument. Multi-argument functions can be simulated using [currying](http://en.wikipedia.org/wiki/Currying).

Plumb doesn't give arguments absolute names when they're introduced. Instead, arguments are referred to *numerically*, based on their position *relative* to the reference. For example:

 - `0`{.python} refers to the "current" argument (ie. of the function enclosing the `0`)
 - `1`{.python} refers to the "parent's" argument (ie. of the the function enclosing the "current" function)
 - `2`{.python} refers to the "grandparent's" argument (ie. of the function enclosing the "parent" function)
 - and so on

This naming scheme is known as [de Bruijn indexing](http://en.wikipedia.org/wiki/De_Bruijn_index). Scope is [lexical](http://en.wikipedia.org/wiki/Lexical_scope#Lexical_scoping) (AKA "static"); references are looked up relative to where they're *defined*, not where they're *used*.

If there is no argument at a given position, for example `plumb([1])`{.python}, the resulting function will cause a (host-specific) error.

Notice that `[0]`{.python} is an identity function, just like `[]`{.python}.

+------------------------------+----------------+
| ```php                       |                |
| <?                           |                |
| $f = plumb([0]);             | **PHP**        |
|                              |                |
| $f("baz") === "baz";         |                |
| ```                          |                |
+------------------------------+----------------+
| ```python                    |                |
| f = plumb([1])               |                |
|                              | **Python**     |
| f("baz") == Error            |                |
| ```                          |                |
+------------------------------+----------------+
| ```javascript                |                |
| f = plumb([[0]]);            |                |
|                              | **Javascript** |
| f("baz")("quux") === "quux"; |                |
| ```                          |                |
+------------------------------+----------------+
| ```haskell                   |                |
| f = plumb [[1]]              |                |
|                              | **Haskell**    |
| f "baz" "quux" == "baz"      |                |
| ```                          |                |
+------------------------------+----------------+

## Calling Functions ##

We can call a function using the infix "`,`{.python}" operator, with the function on the left and the argument on the right. This makes it easy to delay function calls, the second half of laziness:

+-------------------------------------+------------------------------+----------------------------------+--------------------------------+
| PHP                                 | Python                       | Javascript                       | Haskell                        |
+=====================================+==============================+==================================+================================+
| ```php                              | ```python                    | ```javascript                    | ```haskell                     |
| <?                                  | count = plumb([len , "baz"]) | count = plumb([strlen , "baz"]); | count = plumb [length , "baz"] |
| $count = plumb(["strlen" , "baz"]); |                              |                                  |                                |
|                                     | count(None) == 3             | count(null) === 3;               | count () == 3                  |
| $count(null) === 3;                 | ```                          | ```                              | ```                            |
| ```                                 |                              |                                  |                                |
+-------------------------------------+------------------------------+----------------------------------+--------------------------------+

The Javascript version assumes `strlen = function(x) { return x.length; }`{.javascript}

## Chaining Function Calls ##

Plumb functions are unary, so it's easy to chain function calls. The comma operator associates to the left, so we can imagine that:

```python
0 , 1 , 2 , 3
```

Is equivalent to:

```python
((0 , 1) , 2) , 3
```

+----------------------------------------+---------------------------------+-------------------------------------+-------------------------+
| PHP                                    | Python                          | Javascript                          | Haskell                 |
+========================================+=================================+=====================================+=========================+
| ```php                                 | ```python                       | ```javascript                       | ```haskell              |
| <?                                     | f = plumb([[len] , 0 , "quux"]) | f = plumb([[parseInt] , null , 0]); | f = plumb [[1 , 0 , 0]] |
| $f = plumb([['strlen' , 0] , "quux"]); |                                 |                                     |                         |
|                                        | f(None) = 4                     | f("123") === 123                    | f (+) 4 == 8            |
| $f(null) === 4                         | ```                             | ```                                 | ```                     |
| ```                                    |                                 |                                     |                         |
+----------------------------------------+---------------------------------+-------------------------------------+-------------------------+

## Grouping Syntax ##

Chained commas cannot implement right-associative nesting patterns, like:

```python
0 , (1 , 2)
```

To define these, Plumb provides *grouping syntax*, which is equivalent to writing the parentheses above.

To open a new group, we use an underscore followed by an open parenthesis: `_(`{.python}

To close an existing group we use a regular closing parenthesis: `)`{.python}

This is the usual form, although some implementations may vary.

+----------------------------------------+-----------------------------+----------------------------------------+---------------------------+
| PHP                                    | Python                      | Javascript                             | Haskell                   |
+========================================+=============================+========================================+===========================+
| ```php                                 | ```python                   | ```javascript                          | ```haskell                |
| <?                                     | f = plumb([len , _("foo")]) | f = plumb([strlen , _(parseInt , 0)]); | f = plumb [[1 , (1 , 0)]] |
| $f = plumb(['strlen' _([] , "foo")]);  |                             |                                        |                           |
|                                        | f(None) = 3                 | f("123") === 3                         | f (1 +) 2 == 4            |
| $f(null) === 3                         | ```                         | ```                                    | ```                       |
| ```                                    |                             |                                        |                           |
+----------------------------------------+-----------------------------+----------------------------------------+---------------------------+
