---
title: Using Plumb
---
[Plumb](index.html) is a simple language for defining simple functions. This page describes Plumb's syntax and semantics, wherever they differ from those of the [host language](implementations.html).

## Declaring Functions ##

Plumb exists to make function definition easy: just use [square brackets]. Functions will return their argument by default, so an empty pair of brackets is an [identity function](http://en.wikipedia.org/wiki/Identity_function): `[]`{.python}

<div class="summarise">
 <span class="summary">
  Plumb function declarations embedded in various languages
 </span>

+-------------------------------------------------------+------------+
| ```php                                                |            |
| <?                                                    |            |
| $id = plumb([]);                                      | **PHP**    |
| echo "Called {$id(__FUNCTION__)} at  {$id(time())}";  |            |
| ```                                                   |            |
+-------------------------------------------------------+------------+
| ```php                                                |            |
| <?                                                    |            |
| $compose_all = function() {                           |            |
|                  return array_reduce(func_get_args(), | **PHP**    |
|                                      'compose',       |            |
|                                      plumb([]));      |            |
|                };                                     |            |
| ```                                                   |            |
+-------------------------------------------------------+------------+
| ```python                                             |            |
| id = plumb([])                                        |            |
| for handler in [str, str, id, repr]                   | **Python** |
|   results.append(handler(input.pop()))                |            |
| ```                                                   |            |
+-------------------------------------------------------+------------+

</div>

## Returning Values ##

To return a value explicitly, just write it inside the brackets. Plumb will try interpreting the expression according to the rules on this page; if it fails, the host language's semantics will be used. This makes it trivial to write [thunks](http://en.wikipedia.org/wiki/Thunk#Functional_programming):

<div class="summarise">
 <span class="summary">
  Returning values from Plumb functions, embedded in various languages.
 </span>

+------------------------------------------------+----------------+
| ```javascript                                  |                |
| var func = plumb(['hello world!']);            | **Javascript** |
| console.log(func(null));                       |                |
| ```                                            |                |
+------------------------------------------------+----------------+
| ```php                                         |                |
| <?                                             |                |
| public function testInvalidThrowsException() { |                |
|   try {                                        |                |
|     $this->handler                             |                |
|          ->handleWith(plumb([false]), rand()); | **PHP**        |
|     $this->fail('Threw exception');            |                |
|   } catch (InvalidException $e) {}             |                |
| }                                              |                |
| ```                                            |                |
+------------------------------------------------+----------------+

</div>

## Function Arguments ##

Every Plumb function takes a single argument. Multi-argument functions can be simulated using [currying](http://en.wikipedia.org/wiki/Currying).

Plumb doesn't give arguments absolute names when they're introduced. Instead, arguments are referred to *numerically*, based on their position *relative* to the reference. For example:

 - `0`{.python} refers to the "current" argument (ie. of the function enclosing the `0`{.python})
 - `1`{.python} refers to the "parent's" argument (ie. of the the function enclosing the "current" function)
 - `2`{.python} refers to the "grandparent's" argument (ie. of the function enclosing the "parent" function)
 - and so on

This naming scheme is known as [de Bruijn indexing](http://en.wikipedia.org/wiki/De_Bruijn_index). Scope is [lexical](http://en.wikipedia.org/wiki/Lexical_scope#Lexical_scoping): references are looked up relative to where they're *defined*, not where they're *used*.

If there is no argument at a given position, for example `plumb([1])`{.python}, the resulting function will cause a (host-specific) error.

Notice that `[0]`{.python} is an identity function, just like `[]`{.python}.

<div class="summarise">
 <span class="summary">
  Accepting arguments to Plumb functions, embedded in multiple languages.
 </span>

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
| ```python                    |                |
| f = plumb([[0]]);            |                |
|                              | **Python**     |
| f("baz", "quux") == "quux";  |                |
| ```                          |                |
+------------------------------+----------------+
| ```php                       |                |
| <?                           |                |
| $f = plumb([[1]])            | **PHP**        |
|                              |                |
| $f("baz", "quux") === "baz"  |                |
| ```                          |                |
+------------------------------+----------------+

</div>

## Calling Functions ##

We can call a function using the infix "`,`{.python}" operator, with the function on the left and the argument on the right. This makes it easy to delay function calls, the second half of laziness:

<div class="summarise">
 <span class="summary">
  Function calls in Plumb, embedded in multiple languages.
 </span>

+-------------------------------------+------------+
| ```php                              |            |
| <?                                  |            |
| $count = plumb(["strlen" , "baz"]); | **PHP**    |
|                                     |            |
| $count(null) === 3;                 |            |
| ```                                 |            |
+-------------------------------------+------------+
| ```python                           |            |
| count = plumb([len , "baz"])        |            |
|                                     | **Python** |
| count(None) == 3                    |            |
| ```                                 |            |
|                                     |            |
+-------------------------------------+------------+

## Chaining Function Calls ##

Plumb functions are unary, so it's easy to chain function calls. The comma operator associates to the left, so we can imagine that:

```python
0 , 1 , 2 , 3
```

Is equivalent to:

```python
((0 , 1) , 2) , 3
```

<div class="summarise">
 <span class="summary">
  Simulating multi-argument functions in Plumb, embedded in multiple languages.
 </span>

+----------------------------------------+------------+
| ```php                                 |            |
| <?                                     |            |
| $f = plumb([['strlen' , 0] , "quux"]); | **PHP**    |
|                                        |            |
| $f(null) === 4                         |            |
| ```                                    |            |
+----------------------------------------+------------+
| ```python                              |            |
| f = plumb([[len] , 0 , "quux"])        |            |
|                                        | **Python** |
| f(None) = 4                            |            |
| ```                                    |            |
+----------------------------------------+------------+

</div>

## Grouping Syntax ##

Chained commas cannot implement right-associative nesting patterns, like:

```python
0 , (1 , 2)
```

To define these, Plumb provides *grouping syntax*, which is equivalent to writing the parentheses above. We use the name "grouping syntax" since some host languages don't allow us to use parentheses in this way. For example, the PHP implementation uses `<? __(`{.php} instead of an opening parenthesis.

<div class="summarise">
 <span class="summary">
  Grouping/associativity, embedded in multiple languages.
 </span>

+----------------------------------------+------------+
| ```php                                 |            |
| <?                                     |            |
| $f = plumb(['strlen' __([] , "foo")]); |            |
|                                        | **PHP**    |
| $f(null) === 3                         |            |
|                                        |            |
| $compose = plumb([[[2 , __(1 , 0)]]]); |            |
| ```                                    |            |
+----------------------------------------+------------+
| ```python                              |            |
| f = plumb([len , ("foo")])             |            |
|                                        |            |
| f(None) = 3                            | **Python** |
|                                        |            |
| compose = plumb([[[2 , (1 , 0)]]])     |            |
| ```                                    |            |
+----------------------------------------+------------+

</div>

## That's It! ##

Plumb [implementations](implementations.html) should conform to the behaviour described here; everything else may vary from host to host.

If you have any ideas for improving Plumb, [let me know](/contact.html)!
