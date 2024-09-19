---
title: "Ivory: Void"
---

> | The torture at last
> | Will be gone with the past
> | Infinity. Where will I go?
— <cite>Blind Guardian, *Tanelorn (Into the Void)*</cite>

The simplest number system appears at the very top of Ivory's numerical tower:
`void`. In fact, it is *so* simple that it doesn't contain *any* values at all!

```
;; Given any value 'n', it's not an element of 'void'
(define (void? n) #f)
```

`void` is an
[empty type](http://www.chriswarbo.net/blog/2020-02-09-bottom.html), equivalent
to
[`Nothing` in Typed Racket](https://docs.racket-lang.org/ts-reference/type-ref.html#%28form._%28%28lib._typed-racket%2Fbase-env%2Fbase-types..rkt%29._.Nothing%29%29).
It is an
[initial object](https://en.wikipedia.org/wiki/Initial_and_terminal_objects),
meaning we can transform an element of `void` into an element of *anything*: we
just do a `cast`, safe in the knowledge that it can never go wrong, since it can
never actually be executed (due to there being no elements of `void` to call it
on)!

Nevertheless, we can use such casts to implement arithmetic operations for
`void` (like `+`, `×`, `/`, `max`, `min`, `gcd`, `lcm`, etc.) which vacuously
obey their associated laws.

It's rare to need `void`, but it can be handy on occasion. Its dual is a unit
type, like [`zero`](zero_one_many.html).
