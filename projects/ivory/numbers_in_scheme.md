---
title: "Ivory: Numbers In Scheme"
packages: ['racketWithRackCheck']
---

```{pipe="sh"}
bash $setup num.rkt
```

```{pipe="./hide"}
#lang racket
(provide × normalise:real reduce step)
(module+ test
  (require rackunit rackcheck-lib)
  (provide gen:integer gen:rational prop))
```

<figure>

```
    ┌─┐ ┌──┐ ┌─┐
    │ └─┘  └─┘ │
    │ integer  │
    ├──────────┤
    │ rational │
    ├──────────┤
    │   real   │
    ├──────────┤
    │ complex  │
    ├──────────┤
    │  number  │
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
(define (normalise:real normalise ≤ n) n)

(define (step norms ≤ n)
  (for/fold ([result  n]
             [changed #f])
            ([norm norms]
             #:unless changed)
    (norm (curry step norms ≤) result)))

(define (reduce step n)
  (let-values ([(result changed) (step n)])
    (if changed (reduce step result) result)))

(module+ test
  (require (for-syntax syntax/parse))

  (define-syntax (prop stx)
    (define-syntax-class free
      #:description "free variable"
      (pattern (var:id gen:expr)))

    (syntax-parse stx
      [(_ name:id (v:free ...) body ...+)
       #:fail-when (check-duplicate-identifier
                     (syntax->list #'(v.var ...)))
                   "duplicate variable name"
       #'(check-property (property name ([v.var v.gen] ...) body ...))]))

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

  (prop ×-matches-* ([nums (gen:list gen:rational)])
    (check-equal? (apply × nums) (apply * nums) "× acts like *"))
)
```

Numbers in Scheme can be `exact`{.scheme} or `inexact`{.scheme}. Ivory is only
concerned with `exact`{.scheme} numbers, so we'll ignore the latter. Scheme
arranges its types in a "numerical tower", where each level is a super-set of
the ones above, including:

 - `integer`{.scheme}: Whole numbers, positive and negative.
 - `rational`{.scheme}: All fractions, positive and negative.
 - `real`{.scheme}: This supposedly includes the whole "number line", but is
   actually rather silly since almost all of the "real numbers" can't be
   represented.
 - `complex`{.scheme}: This uses a clever trick to represent square roots of
   negative numbers. If you've encountered it before, great; if not, don't worry
   since Ivory does not include this level (we instead define a mezzanine inside
   the `geometric`{.scheme} level)
 - `number`{.scheme}: This is the top level, containing every numeric value.

These are cumulative, so an `integer`{.scheme} like `-42`{.scheme} is also a
`rational`{.scheme} (e.g. you can think of it like `-42/1`{.scheme}), a `real`
and a `complex` (like `-42+0i`{.scheme}, if you know what that means).

This basic framework is the inspiration for Ivory.

```{pipe="./hide"}
(module+ test
  (test-case
    "Check the parts of Scheme's tower that we'll use"

    (test-pred "Integer -1 exists"   integer?  -1)
    (test-pred "Rational 1/2 exists" rational? 1/2)
    (test-pred "Numbers 1+1i exists" number?   1+1i)

    (test-false "Rational 1/2 isn't integer" (integer?  1/2))
    (test-false "Number 1+1i isn't rational" (rational? 1+1i)))

  (prop integer?-implies-rational? ([n gen:integer])
    (check-pred integer?  n)
    (check-pred rational? n))

  (prop rational?-implies-number? ([n gen:rational])
    (check-pred rational? n)
    (check-pred number?   n))
)
```

### Numbers in Racket ###

<figure>

```
        ┌─┐ ┌──┐ ┌─┐
        │ └─┘  └─┘ │
        │   zero   │
       ┌┴──────────┴┐
       │  natural   │
      ┌┴────────────┴┐
      │   integer    │
     ┌┴──────────────┴┐
     │    rational    │
     ├────────────────┤
     │      real      │
    ┌┴────────────────┴┐
    │     complex      │
    ├──────────────────┤
    │      number      │
────┴──────────────────┴────
```

 <figcaption>
  Racket's numerical tower (somewhat simplified): `complex`{.scheme} is another
  name for `number`{.scheme}, and `real`{.scheme} is another name for
  `rational`{.scheme} (for `exact`{.scheme} numbers, at least).
 </figcaption>
</figure>

Racket already [extends Scheme's standard numerical
tower](https://docs.racket-lang.org/reference/numbers.html#%28tech._number%29),
and pins-down some details that Scheme leaves open. In particular:

 - Scheme *allows* levels between `complex`{.scheme} and `number`{.scheme}, but
   Racket doesn't provide any.
 - Racket's foundational `number`{.scheme} type adds nothing else to
   `complex`{.scheme} (they're just synonyms)
 - The only `exact`{.scheme} numbers in Racket's `real`{.scheme} level are
   `rational`{.scheme}. Hence, as far as Ivory is concerned, those levels are
   the same.

Racket *does* add some "attic levels", which are strict subsets of
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
    (test-false "-1 isn't natural" (natural? -1)))

  (prop natural?-implies-integer? ([n gen:natural])
    (check-pred natural? n)
    (check-pred integer? n))

  (prop natural-is-closed ([x gen:natural] [y gen:natural])
    (check-pred  natural? (+   x y)   "+ is closed")
    (check-pred  natural? (×   x y)   "× is closed")
    (check-pred  natural? (gcd x y) "gcd is closed")
    (check-pred  natural? (lcm x y) "lcm is closed")
    (check-pred  natural? (max x y) "max is closed")
    (check-pred  natural? (min x y) "min is closed")
    (when (> y 0)
      (check-pred natural? (quotient  x y)  "quotient is closed")
      (check-pred natural? (remainder x y) "remainder is closed")
      (check-pred natural? (expt      x y)      "expt is closed"))
    (when (> x 0)
      (check-pred natural? (expt      x y) "expt is closed")))

  (test-case "zero is closed"
    (test-pred   "+ is closed" zero? (+   0 0))
    (test-pred   "× is closed" zero? (×   0 0))
    (test-pred "gcd is closed" zero? (gcd 0 0))
    (test-pred "lcm is closed" zero? (lcm 0 0))
    (test-pred "max is closed" zero? (max 0 0))
    (test-pred "min is closed" zero? (min 0 0)))
)
```

Even more fine-grained structure is [described in the Typed Racket
documentation](https://docs.racket-lang.org/ts-reference/type-ref.html#%28part._.Numeric_.Types%29)
(e.g. `byte`{.scheme} is a subset of `natural`{.scheme} between `0`{.scheme} and
`255`{.scheme}) but Ivory does not make such size-based distinctions.

## Code ##

This post defines Racket code, including a RackCheck test suite to check the
claims we make on this page. Here's a URI containing all of the generated code:

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
