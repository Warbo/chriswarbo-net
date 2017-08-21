---
title: A Personal Opinion On PHP
---

(I found this sat in my 'unfinished' directory, as an introduction to PHP
programming for [ocPortal](http://ocportal.com) users. I still agree with the
content so I thought I'd post it; although it does basically trail off!)

[ocPortal](http://ocportal.com) is written in a mixture of programming
languages, including standard Web languages such as XHTML, CSS, SQL and
Javascript; its own internal languages Comcode and Tempcode; and the PHP
language to tie it all together. It is important to remember, if you plan to
learn more about programming in general, that programming languages are
designed, created and used by people. This means that:

 - They are always going to be flawed in some way, since language design is an
   on-going area of research
 - The way one language may approach a problem is not the only way, and
   certainly not a consequence of something inherent to computers (more usually
   a language's features are determined as a balance between what is considered
   important by its designers against how difficult it would be to make those
   things work)

This makes it incredibly important for those who wish to understand programming
to learn a few varied languages, rather than focusing on only one and "becoming
an expert" (you won't, you'll just be blinkering yourself to other people's
insights). The more languages you know, the more approaches to problems you'll
know, and the more prepared you'll be when tackling problems in any language.

Here we're learning programming in the context of PHP, but it is important that
you do not see the techniques you learn as being "PHP-centric". I say this
because I, along with many others, am not a fan of PHP. It is available on every
Web server in the world, which is why it's a great choice for building ocPortal,
but that is the only good point about the language. I will mention here what my
major issues are with PHP, so that you can have them in mind as you come across
them.

Firstly, PHP is obsessed with syntax. The syntax of a language is the system of
rules that determine what text is valid PHP and what isn't, regardless of any
intended or actual meaning of the code. It seems that whenever PHP gets a new
feature, it is also given an entirely new language construct in order to express
that feature in, rather than reusing existing syntax. For example
`foreach ($a as $b=>$c)` is a very confusing collection of symbols. It doesn't
actually need the `=>` syntax since it could have used instead
`foreach ($a as array($b,$c))` (note that the existing `list($b,$c)` wouldn't
do, as it would give ambiguity when looping over arrays of pairs) and could be
implemented internally via:

    $temp = $a;
    foreach (zip(array_keys($temp),array_values($temp)) as list($b,$c))...

Likewise there is the obvious failing that variables and functions are
referenced via different syntax, `foo` for functions and `$foo` for variables,
except of course when they're in an object in which case there's a third syntax
`$bar->foo` which is used for both variables and functions. Classes introduce a
fourth syntax for references, `bar::foo` which is once again used for functions
and variables. Arrays have their own syntax for references too, as `bar[foo]`,
bringing the count up to 5, and while we're on the subject of arrays it's worth
noting that their constructor `array(foo,bar,baz)` is nice and symmetric with
the function calling syntax of comma-separated arguments in parentheses, but
then they go and ruin it by allowing `array(foo=>bar,baz=>foobar)` whilst
simultaneously disallowing keyword argument function calls such as
`foo(bar=>baz,foobar=>bazbaz)`.

To make things extra confusing, variables can store functions, making `$foo`
valid for functions in some cases (6 ways of referencing) and the standard way
to reference a function when passing it as an argument is by type-mangling a
string, which makes `'foo'`, `"foo"` and `<<< BAR
foo BAR;` all standard, required ways to reference functions in various
contexts. That's 9 distinct ways of referencing things which are completely
context-dependent. Thanks PHP. THP.

To add insult to injury with all this, the naming conventions, argument order
and expected argument values for the standard library have absolutely no
consistency.
