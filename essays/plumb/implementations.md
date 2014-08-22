---
title: Plumb Implementations
---
There are Plumb implementations for (at least) the following languages. If you know of another, [please tell me!](/contact.html)

- [PHP](https://www.gitorious.org/php-plumb/php-plumb): Note that this implementation uses `<? __()`{.php} for grouping syntax.
    - Available on [Packagist](https://packagist.org/packages/warbo/plumb)
    - Curries PHP functions automatically, and `<? plumb`{.php} itself is curried
    - Treats operators as functions, eg. `<? '+'`{.php} acts like `<? curry(function($x, $y) { return $x + $y; })`{.php}
- [Python](https://www.gitorious.org/python-plumb/python-plumb)
    - Partial support for keyword arguments
    - TODO: Add to PyPI
- [Javascript](https://gitorious.org/js-plumb)
