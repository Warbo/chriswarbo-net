---
title: Plumb Implementations
---
There are Plumb implementations for (at least) the following languages. If you know of another, [please tell me!](/contact.html)

- [PHP](/git/php-plumb)
    - Uses `__()`{.php} for grouping syntax
    - Available on [Packagist](https://packagist.org/packages/warbo/plumb)
    - Curries PHP functions automatically, and `plumb`{.php} itself is curried
    - Treats operators as functions, eg. `'+'`{.php} acts like `curry(function($x, $y) { return $x + $y; })`{.php}
- [Python](/git/python-plumb)
    - Partial support for keyword arguments
    - Curries Python functions automatically, and `plumb`{.python} itself is curried
- [Javascript](/git/js-plumb)
    - Uses `_()`{.javascript} for grouping syntax
    - Curries Javascript functions automatically, and `plumb`{.javascript} itself is curried
