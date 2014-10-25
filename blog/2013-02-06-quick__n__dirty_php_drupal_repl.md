---
title: Quick 'n' Dirty PHP/Drupal REPL
---
In my new job at [WANdisco] [1] I've
been stuck trying to understand Drupal for a few weeks (which you may
have gathered from my Identi.ca feed!).

[1]: http://www.wandisco.com

I've got Xdebug and Emacs integrated quite nicely via GEBEN, which is
pretty cool, but running **everything** through a debugger can be
frustrating at times. Sometimes I just want to evaluate an expression,
without loading a browser page, without new Emacs windows appearing,
etc. It's sometimes tempting to use print_f debugging, but I know that
it will come back to bite me if I do.

Instead, I've thrown together the following read-eval-print-loop
(REPL) for Drupal. Run it using `drush php-script repl.php`,
preferably as your Web-server user (www-data in my case). It took
approximately 30 seconds to write, so don't expect it to handle edge
cases gracefully!

```php
<?php

// VERY simple read-eval-print-loop (REPL) for PHP.
// Useful for running with "drush php-script repl.php"
echo "\\nphp> ";
while (($line = fgets(STDIN)) !== FALSE) {
    try {
        var_dump(eval($line));
    } catch (Exception $e) {
        echo "\\nUncaught Exception: " . var_export($e, TRUE);
    }
    echo "\\nphp> ";
}
```

Run it and you'll get a command-line `php>`, where you can run
lines of PHP. Note that you need to put complete lines, ie. you need
to include the semicolon, and code which normally straddles several
lines (eg. `if`, `for`, `while`, etc.) may need to be squashed on to one.

Use of `echo`, `var_dump`, etc. is the same as any other commandline PHP.
I've wrapped return values in `var_dump` as a shortcut, since I normally
`var_dump` everything I'm looking at (eg. `echo FALSE;` gives an empty
string, `var_dump(FALSE);` gives the much more useful `(bool) false`).

I'll probably add to this over time, so I may stick it in my Wiki :)

Hope it helps someone!
