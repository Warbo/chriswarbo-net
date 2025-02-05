---
title: PHP Reference Silliness
---
Today I needed to pop arrays in a loop in PHP, starting with one and falling back to another when it runs out. A naïve implementation might do this:

```php
if (count($bar)) {
  $foo = array_pop($bar);
}
else {
  $foo = array_pop($baz);
}
```

However, I always cringe when I see code like this. Why branch on a whole statement when you could just branch on the expression you care about?

```php
$foo = count($bar)? array_pop($bar) : array_pop($baz);
```

Of course, this is still redundant, since we're calling `array_pop` twice. Let's shrink the branch again:

```php
$foo = array_pop(count($bar)? $bar : $foo);
```

Aaaaaaaand... PHP dies. It won't let us pass-by-reference with a calculated expression like this ternary statement. Sad times. We can sometimes work around PHP's stupid pass-by-reference crap using `call_user_func`, but not this time. However, we **can** call-by-reference on elements inside an array, and we **can** look up those elements using a calculated expression. This means we can swap `$bar` and `$baz` for `$a = array($bar, $baz)` and use the following:

```php
$foo = array_pop($a[count($a[0]) > 0]);
```

In fact, while writing this up, I found another refactoring in the generation of `$bar` and `$baz` which lets me do away with their distinction by the time we get this far, so I can just `array_merge` them together and pop that until it's empty :)
