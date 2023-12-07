---
title: Numbers In Scheme
packages: ['racketWithRackCheck']
---

```{pipe="bash $setup num.rkt"}
```

```{pipe="./hide"}
#lang racket
(module+ test (require rackunit rackcheck-lib))
```

<figure>

```
    ┌─┐ ┌──┐ ┌─┐
    │ └─┘  └─┘ │
    │  number  │
    ├──────────┤
    │ complex  │
    ├──────────┤
    │   real   │
    ├──────────┤
    │ rational │
    ├──────────┤
    │ integer  │
────┴──────────┴────
```

 <figcaption>
  Scheme's standard numerical tower.
 </figcaption>
</figure>

Scheme uses the asterisk `*`{.scheme} for multiplication, since it's easy to
type on a standard US keyboard. I prefer the usual `×`{.scheme} symbol, for
clarity. Hence we'll define `×`{.scheme} to be `*`{.scheme}, so that both
symbols will mean multiplication:

```{.scheme pipe="./show"}
(define × *)
```

```{pipe="./hide"}
(module+ test
  (define gen:integer
    (gen:let ([magnitude gen:natural]
             [sign       gen:boolean])
      (* (if sign 1 -1) magnitude)))

  (define gen:rational
    (gen:let ([numerator   gen:integer]
              [denominator gen:integer])
      (if (zero? denominator)
        numerator
        (/ numerator denominator))))

  (test-equal? "Empty product is 1" (×) 1)

  (check-property
    (property ×-is-extensionally-equal-to-*
      ([nums (gen:list gen:rational)])
      (test-equal? "× acts like *" (apply × nums) (apply * nums))))
)

```

Numbers in Scheme can be `exact`{.scheme} or `inexact`{.scheme}. That's mostly
irrelevant for what we're doing, so in this post we'll stick to `exact`{.scheme}
numbers. These are arranged in a "numerical tower", where each level is a
super-set of the ones below, including:

 - `number`{.scheme}: This is the top level, containing every numeric value.
 - `complex`{.scheme}: These numbers use a clever trick for representing square
   roots of negative numbers. If you've encountered it before, great; if not,
   don't worry because we'll be replacing it with a more powerful trick!
 - `real`{.scheme}: This supposedly includes the whole "number line", but is
   actually rather silly since almost all of the "real numbers" can't be
   represented.
 - `rational`{.scheme}: Includes all fractions, positive and negative.
 - `integer`{.scheme}: Only whole numbers, positive and negative.

These are cumulative, so an `integer`{.scheme} like `-42`{.scheme} is also a
`rational`{.scheme} (e.g. you can think of it like `-42/1`{.scheme}), a `real`
and a `complex` (like `-42+0i`{.scheme}, if you know what that means). We'll use
insights from GA to alter, extend and replace this tower!

```{pipe="./hide"}
(module+ test
  (test-case
    "Check the parts of Scheme's tower that we'll use"

    (test-pred "Integer -1 exists"   integer?  -1)
    (test-pred "Rational 1/2 exists" rational? 1/2)
    (test-pred "Numbers 1+1i exists" number?   1+1i)

    (test-false "Rational 1/2 isn't integer" (integer?  1/2))
    (test-false "Number 1+1i isn't rational" (rational? 1+1i))

    (check-property
      (property integer?-implies-rational? ([n gen:integer])
        (check-pred integer?  n)
        (check-pred rational? n)))

    (check-property
      (property rational?-implies-number? ([n gen:rational])
        (check-pred rational? n)
        (check-pred number?   n)))
))
```

### Numbers in Racket ###

<figure>

```
    ┌─┐ ┌──┐ ┌─┐
    │ └─┘  └─┘ │
    │  number  │
    │ c̶o̶m̶p̶l̶e̶x̶  │
    ├──────────┤
    │   r̶e̶a̶l̶   │
    │ rational │
    ├──────────┤
    │ integer  │
────┼──────────┼────
    │ natural  │
    ├──────────┤
    │   zero   │
    └──────────┘
```

 <figcaption>
  Racket's numerical tower (somewhat simplified). We'll ignore
  `complex`{.scheme} (which is just another name for `number`{.scheme}). We'll
  also ignore `real`{.scheme}, since it only differs from `rational`{.scheme}
  when using `inexact`{.scheme} numbers (which we're ignoring).
 </figcaption>
</figure>

Racket already [extends Scheme's standard numerical
tower](https://docs.racket-lang.org/reference/numbers.html#%28tech._number%29),
and pins-down some details that Scheme leaves open. In particular:

 - Scheme *allows* levels that are higher than `complex`{.scheme}, but Racket
   doesn't provide any.
 - Racket's top-level `number`{.scheme} adds nothing else to `complex`{.scheme}
   (they're literally just synonyms!)
 - The only `exact`{.scheme} numbers in Racket's `real`{.scheme} level are
   `rational`{.scheme}. Hence, as far as we're concerned, `real`{.scheme} is
   just a different (more confusing) name for `rational`{.scheme}.

Racket *does* add some "basement levels", which are strict subsets of
`integer`{.scheme}:

 - `natural`{.scheme} is a sub-set of `integer`{.scheme} without negatives. It
   is closed under `+`{.scheme}, `×`{.scheme}, `gcd`{.scheme}, `lcm`{.scheme},
   `max`{.scheme}, `min`{.scheme}, etc.; as well as `quotient`{.scheme} and
   `remainder`{.scheme} excluding the divisor `0`{.scheme}, and `expt`{.scheme}
   excluding `(expt 0 0)`{.scheme}.
 - `zero`{.scheme} is a sub-set of `natural`{.scheme} containing only
   `0`{.scheme}. It is closed under `+`{.scheme}, `×`{.scheme}, `gcd`{.scheme},
   `lcm`{.scheme}, `max`{.scheme}, `min`{.scheme}, etc.

```{pipe="./hide"}
(module+ test
  (test-case
    "Check the Racket-specific levels that we'll use"

    (test-pred  "0 is zero"         zero?     0 )
    (test-pred  "0 is natural"      natural?  0 )
    (test-pred  "1 is natural"      natural?  1 )
    (test-pred  "-1 is integer"     integer? -1 )
    (test-false "1 isn't zero"     (zero?     1))
    (test-false "-1 isn't natural" (natural? -1))

    (check-property
      (property natural?-implies-integer? ([n gen:natural])
        (check-pred natural? n)
        (check-pred integer? n)))

    (check-property
      (property natural-is-closed ([x gen:natural] [y gen:natural])
        (test-pred   "+ is closed" natural? (+   x y))
        (test-pred   "× is closed" natural? (×   x y))
        (test-pred "gcd is closed" natural? (gcd x y))
        (test-pred "lcm is closed" natural? (lcm x y))
        (test-pred "max is closed" natural? (max x y))
        (test-pred "min is closed" natural? (min x y))
        (when (> y 0)
          (test-pred  "quotient is closed" natural? (quotient  x y))
          (test-pred "remainder is closed" natural? (remainder x y))
          (test-pred      "expt is closed" natural? (expt x y)))
        (when (> x 0)
          (test-pred      "expt is closed" natural? (expt x y)))))

    (test-case "zero is closed"
      (test-pred   "+ is closed" zero? (+   0 0))
      (test-pred   "× is closed" zero? (×   0 0))
      (test-pred "gcd is closed" zero? (gcd 0 0))
      (test-pred "lcm is closed" zero? (lcm 0 0))
      (test-pred "max is closed" zero? (max 0 0))
      (test-pred "min is closed" zero? (min 0 0)))
))
```

Even more fine-grained structure is [described in the Typed Racket
documentation](https://docs.racket-lang.org/ts-reference/type-ref.html#%28part._.Numeric_.Types%29)
(e.g. `byte`{.scheme} is a subset of `natural`{.scheme} between `0`{.scheme} and
`255`{.scheme}) but we won't bother with such size-based distinctions.

## Code ##

Here's a URI containing all of the Racket code generated by this post:

```{.unwrap pipe="sh | pandoc -t json"}
# Use a data URL. These default to US-ASCII encoding, so we need to
# specify UTF8 for our unicode symbols.
printf '<a id="racket" download="num.rkt" href="%s' \
       'data:text/plain;charset=utf-8;base64,'

# The contents needs to be URL-encoded. That requires extra dependencies, which
# I'd rather avoid. Instead, we can use GNU coreutils to Base64-encode it, which
# will avoid the need for a separate URL-encoding step.
base64 -w0 < num.rkt
printf '">DOWNLOAD RACKET CODE</a>'
```

<details class="odd">
 <summary>Test results</summary>

```{pipe="./tests"}
```

</details>
