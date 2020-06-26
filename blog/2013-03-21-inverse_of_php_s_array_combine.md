---
title: Inverse of PHP's array_combine
---
Another shameless re-post of [an answer I gave on Stack Overflow](http://stackoverflow.com/questions/6234696/php-splitting-an-array-into-two-arrays-keys-array-and-values-array/15554317#15554317). This time it's about a function to split an `array`{.php} into its keys and its values, ie. an inverse of `array_combine`{.php}.

Unfortunately there is no built-in inverse of `array_combine`{.php}. There is also no way to define one, since `array_combine`{.php} expects multiple parameters and we can't return multiple values from a function.

We can construct an alternative to `array_combine`{.php} which takes a single argument: the `array`{.php} of keys and the `array`{.php} of values wrapped up together in another `array`{.php}. This transformation is called "uncurrying" and is performed by `call_user_func_array`{.php}:

```php
$array_comb  = function($arr) { return call_user_func_array('array_combine', $arr); };
```

This alternative function does have an inverse:

```php
$array_split = function($arr) { return array(array_keys($arr), array_values($arr)); };
```

If we define function composition:

```php
$compose  = function($f, $g) {
    return function($x) use ($f, $g) { return $f($g($x)); };
};
```

Then the following functions are all (extensionally) equal, ie. they all return their argument unchanged:

```php
$identity      = function($x) { return $x; };
$left_inverse  = $compose($array_split, $array_comb);  // Split then combine
$right_inverse = $compose($array_comb, $array_split);  // Combine then split
```

Note that they accept different argument types though:

 - `$identity`{.php} will work on anything.
 - `$left_inverse`{.php} will work on any `array`{.php}.
 - `$right_inverse`{.php} will work on `array`{.php}s-of-`array`{.php}s, where the outer `array`{.php} contains 2 elements, both inner `array`{.php}s are of equal length and the first inner `array`{.php} only contains `int`{.php}egers and `string`{.php}s.
