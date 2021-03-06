---
title: Unifying PHP's namespaces
---
Here's a simple macro for avoiding one of PHP's more frustrating problems: the 'static' nature of PHP's global function namespace.

For some reason, PHP used to make a distinction between functions and every other kind of value. Declaring a function was a special operation which was completely separate from the other computation performed by a script. This made it impossible to, for example, calculate a function's definition; instead, every function must be written out manually.

More recent versions of PHP try to remove this unnecessary distinction, by introducing proper (lexical) scope and (almost) first-class function values. These let us, amongst other things, calculate a function's definition:

```php
// Old way: define every function manually
function as_strings($arr) {
  return array_map('strval', $arr);
}

// New way: build functions automatically, by abstracting away the
// underlying transformations (in this case, eta-expansion)
$as_strings = papply('array_map', 'strval');

// Generic, re-usable partial-application function
function papply() {
  $args = func_get_args();
  return function() use ($args) {
    return call_user_func_array('call_user_func',
                                array_merge($args, func_get_args()));
  };
}
```

One remaining problem with this historical baggage is that we now have to deal with two kinds of function: "traditional" functions, which live in their own special, global namespace; and function values which are regular variables. Ideally we'd just stick to function values, but the history of global functions in PHP has strongly influenced the architecture of many projects, making it difficult to pass properly-scoped function values between their point of definition and their point of use.

We can easily go the other way, thanks to a hack in the PHP interpreter which lets us treat strings as functions if they contain the name of a global function. Going the other way is more cumbersome.

The simplest way is to write global wrappers which just defer to a function value. For example:

```php
function foo($x) {
  return call_user_func(function($y) { return $y * $y; }, $x);
}
```

The difficulty with this approach is that global functions don't have proper (lexical) scope, so they have no access to the scope in which they're defined. This makes life complicated when we're trying to wrap existing values, unlike the above where we're defining a new function value inside the wrapper's scope.

Obviously, the best way to inject a value into a function's scope is as an argument, but it defeats the point of a wrapper if we need to supply it with the supposedly-wrapped value every time we call it.

One way around this is to use PHP's peculiar, and confusingly named, "static variables". These are like an impoverished version of a closure's environment; they're function-local, but retain their value between invocations. We can use these to write wrappers which act in two 'phases': first we pass the function value as an argument, which gets assigned to a static variable and is therefore available to future invocations. These future invocations can take their arguments as normal, deferring to the static variable. Operationally, this is similar to an Erlang actor changing its behaviour via a tail call, or how a Smalltalk object can use `become` to change its class (except we incur a small run-time cost):

```php
function foo($x) {
  static $f = NULL;          // $f will persist across calls
  if (is_null($f)) $f = $x;  // If we haven't assigned $f, do so
  else return $f($x);        // Otherwise, defer to $f
}
foo(function($y) { return $y * $y; });  // Pass a value from outside
```

Of course, a wrapper like this is more fragile than necessary, since it's too closely coupled to the interface of `$f`. We can generalise it to pass on any number of arguments:

```php
function foo() {
  static $f = NULL;
  if (is_null($f)) $f = current(func_get_args());
  else return call_user_func_array($f, func_get_args());
}
foo(function($y) { return $y * $y; });
```

In fact, this new, generic approach can be wrapped up in a macro (which takes its name from the [LISP equivalent] [1]):

[1]: http://en.wikipedia.org/wiki/Defun

```php
function defun($name, $body) {
  // Regex taken from http://www.php.net/manual/en/functions.user-defined.php
  $name_regex = '/[a-zA-Z_\\x7f-\\xff][a-zA-Z0-9_\\x7f-\\xff]*/';
  array_map('except', array_keys(array_filter([
    "Invalid name for $name" => !preg_match($name_regex, $name),
    "Cannot redeclare $name" => function_exists($name),
    "Invalid body for $name" => !is_callable($body, TRUE)])));

  // Declare $name globally, storing the body in an assign-once static variable
  eval("function {$name}() {
          static \\$f = NULL;
          if (is_null(\\$f)) \\$f = current(func_get_args());
          else return call_user_func_array(\\$f, func_get_args());
        }");
  $name($body);  // Populate $f with our $body
}

// Declaring our "foo" function is then as simple as
defun('foo', function($y) { return $y * $y; });
```

Here's the "except" function I've used a helper function to make validation easier. You can replace this with a better method of error handling than exceptions if you want (eg. a `Maybe`):

```php
function except($msg) { throw new Exception($msg); }
```

With this infrastructure in place, we can not only use global-level functions as values (by using strings containing their names), but we can also use values as global-level functions (by passing them to defun).

This should be useful for those times when you find yourself wanting a global-level function which acts like a particular value, and don't want the boilerplate, bloat, bugs and frustration of coming up with a matching manual definition.

Happy hacking!
