---
title: Mapping associative arrays in PHP
---
Trying to solve the same problem, I came across [this Stack Overflow question](http://stackoverflow.com/questions/5868457/php-can-i-get-the-index-in-an-array-map-function) about accessing keys in PHP's `array_map`{.php}. The accepted answer uses `array_keys`{.php}, but this is unsatisfactory since it requires giving the `array`{.php} a name (I wouldn't have been Googling for a solution if it was so easy!).

Turns out there's no great solution, so I added an answer listing a few alternatives. I'm shamelessly re-posting it here:

## The Problem ##

When mapping an anonymous `function`{.php} over an anonymous `array`{.php}, there is no way to access the keys:

```php
array_map(
    function($val) use ($foo) { /* ... */ },
    array(key1 => val1,
          key2 => val2,
          /* ... */));
```

`array_reduce`{.php} doesn't get access to the keys either. `array_walk`{.php} can access keys, but the `array`{.php} is passed by reference, which we can't do with anonymous values.

## Solutions ##

### Array of pairs ###

This is bad, since we're changing the original `array`{.php}, which might require pre-processing. Also, the boilerplate `array()`{.php} calls increase linearly with the length of the `array`{.php}`:

```php
array_map(
    function($pair) use ($foo) {
        list($key, $val) = $pair;
        /* ... */
    },
    array(array(key1, val1),
          array(key2, val2),
          /* ... */));
```

### Temporary variable ###

We don't need to pre-process the `array`{.php}, and the boilerplate is constant, but we can accidentaly clobber an existing variable:

```php
$i_hope_this_does_not_conflict = array(key1 => val1,
                                       key2 => val2,
                                       /* ... */);
array_map(
    function($key, $val) use ($foo) { /* ... */ },
    array_keys($i_hope_this_does_not_conflict),
    $i_hope_this_does_not_conflict);
unset($i_hope_this_does_not_conflict);
```

### One-shot `function`{.php} ###

We can use a `function`{.php} to restrict the scope of our temporary variable and prevent clobbering existing names. One downside is that we need to add an extra `use` clause for the values we'll need:

```php
call_user_func(
    function($arr) use ($foo) {
        return array_map(function($key, $val) use ($foo) { /* ... */ },
                         array_keys($arr),
                         $arr);
    },
    array(key1 => val1,
          key2 => val2,
          /* ... */));
```

### Multi-argument one-shot `function`{.php} ###

We define the `function`{.php} we're mapping in the original scope to prevent the `use`{.php} boilerplate:

```php
call_user_func(
    function($f, $arr) {
        return array_map($f, array_keys($arr), $arr);
    },
    function($key, $val) use ($foo) { /* ... */ },
    array(key1 => val1,
          key2 => val2,
          /* ... */));
```

### New `function`{.php} ###

The interesting thing to note is that our last one-shot `function`{.php} has a nice, generic signature and looks a lot like `array_map`. We might want to give this a name so we can re-use it:

```php
function array_mapk($f, $arr) {
    return array_map($f, array_keys($arr), $arr);
}
```

Our application code then gets back to where we started, but in the process we've gained a useful `function`{.php} for our library:

```php
array_mapk(
    function($key, $val) use ($foo) { /* ... */ },
    array(key1 => val1,
          key2 => val2,
          /* ... */));
```
