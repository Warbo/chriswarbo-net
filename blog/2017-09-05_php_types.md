---
title: PHP Types
---

This is short ramble about PHP's *type system* and the
*meaning* of our programs.

## Semantics

A programming system can usually be considered as 2 different parts; syntax and
semantics. *Syntax* is the particular way that things need to be written in
order for them to be "well formed" or "syntactically correct"; in other words,
syntax separates valid programs from invalid programs based on how they're
written (for example `my$ =;` is invalid PHP). *Semantics* on the
other hand, separates valid and invalid programs based on their *behaviour*
or *meaning*. This is much trickier, since misbehaving programs are much
harder to spot than incorrectly written ones, and in order to spot "bad"
behaviour we need to know what "good" behaviour is, and even what we mean by
"behaviour".

In PHP, the "meaning" of the code we write
is that the computer will start at the first line of the first file it is given
and treat each statement it finds as an *instruction* to carry out (joined
together by semicolons which mean "and then"), until it either runs out of
instructions or is told to stop. These instructions can modify
the *state* of the computer. Computer Scientists would call this a form of
"operational semantics"; the meaning of the code depends on some physical
operation being performed, in this case the meaning of the code is the changes
it makes to the state of the computer (its memory).

This simple definition allows us to spot that code like this:

```php
$x = 0;
$x = $x;
```

is 'wrong' because we know that the second line will never change the state, and
thus is 'meaningless' from the point of view of PHP. In this case the mistake is
harmless, since by definition it doesn't change the behaviour. There are much
harder problems to deal with though, where the incorrect instruction depends on
the state of the program when it is run, and we can use much more sophisticated
techniques to find them.

## Types

Understanding more about the semantics of PHP values, and in particular its
*type system* allows us to spot and prevent many mistakes in the behaviour of
our programs.

The type of some value, in a very non-rigourous sense, depends on both the way
we represent that value and what we can do with it. Computers are built out of
circuits which carry electricity, but the design of these circuits causes the
electric signals to behave like numbers. Thus the most basic kind of thing, or
*type of value*, that a computer can handle is (whole) numbers, since
they're built into the hardware of the machine.

In a similar way to using circuits that behave like numbers, we can make numbers
behave like other types of things, if handled correctly. For example we can
represent fractions with whole numbers by using scientific notation, such as
'15×10^-4^' to mean '0.0015'. We can represent letters and other characters as
numbers if we follow a scheme (an *encoding*) like '01 is A, 02 is B, 03 is
C...', and we can use sequences of these characters to represent more complex
types of thing, like PHP code and HTML pages, by inventing syntax to write them
in.

Because all of these values are, ultimately, numbers (and then electric signals
in the computer), the computer itself doesn't know if a value is meant to
represent something of another type or not. Thus it is important to know what
type of values we are dealing with in our programs if we're to handle them
correctly. For example, we may try to write out the string `"CAR"` by saying:

``` php
echo "C" + "A" + "R";
```

However, if PHP is using the text representation described above (it doesn't,
but this won't affect the examples) then this will actually be the same as an
instruction to do:

```php
echo 03 + 01 + 18;
```

This is obviously not what we were expecting, since it's the number
`22`. There's been a mistake in our use of types here. However, it gets
worse. We now have the number 22, but we were expecting a string so we're going
to make another type error as we treat `22` as a string, which will end up being
a `"V"`. As far as the computer is concerned, `"V"` is the result we wanted,
since that's what we asked for, but from our point of view this data has become
*corrupted*. This is why semantics is difficult to get right.

Luckily, PHP doesn't blindly assume that we're treating types
correctly. Instead, in PHP each value contains information about what type it
is, and if we try to do things which don't make sense for that type of value
(such as summing strings like above) then it will give us an error message. Note
that PHP will sometimes try to guess what you might have meant, but it is
dangerous to rely on this compared to specifying exactly what you mean.

If we want to fix the above example, and attach one string to the end of another
(as opposed to summing them), then we can use the *concatenation operator*, a
full stop `.`, rather than the *addition operator*, the plus `+`. This means we
can write our example correctly as:

```php
echo "C" . "A" . "R";
```

This won't give a type error, and it won't corrupt our values. The downside, of
course, is that we can't freely mix strings and numbers, since they're different
types. However, it's quite easy to convert numbers into strings (just write them
out in decimal), and the `strval` function will do this for us. For example, if
we want to write out the number of cars, which is stored in a variable called
`$car_num`, then we can't say:

```php
echo "There are " . $car_num . " cars";
```

Since this is a type error (we're trying to attach a number to some strings). We
can write it correctly as:

```php
echo "There are " . strval($car_num) . " cars";
```

Of course, we may not know what our variable contains (the state may change it),
in which case using `strval` is a good idea if the variable might not be a
string. There are similar functions for integers (whole numbers) and "floating
point" numbers (fractions) called `intval` and `floatval`, although keep in mind
that most things can be turned into strings, but only properly formatted strings
can be turned into numbers correctly; for example `floatval('-4.3')` will work,
but `floatval('minus four point three')` won't.

## Conclusion

Although types are very useful for finding programming mistakes, like most of
PHP its type system is limited to just the handful that have been hard-coded
into the implementation by its developers. For example, we're not allowed to
define our own types (eg. angles) or operators (eg. vector product), there are
no higher-level types (types of types, eg. coordinates) and we can't define our
own encodings (eg. a HTML type). We can overcome some of these difficulties by
using PHP's primitive "object" system, which I may talk about in a future post.
