---
title: "Ivory: Geometric Units"
packages: ['racketWithRackCheck']
---

```{pipe="sh"}
bash $setup geo-units.rkt
```

```{pipe="./hide"}
#lang racket
(provide unit-ga?
         unit-flavour
         unit-index
         unit<?
         subscripts-to-digits
         parse-unit)
(module+ test (require rackunit rackcheck-lib))
```

In this page we will leave the ordinary number line, and begin to introduce the
`geometric` level of the Ivory tower. Our starting point is the equation:

$$a^2 + 1 = 0$$

Or, as an s-expression (where [the `sqr`
function](https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28lib._racket%2Fmath..rkt%29._sqr%29%29)
multiplies a number by itself):

```scheme
(= (+ (sqr a) 1)
   0)
```

This looks simple enough, but let's take a moment to consider its broader
meaning. In particular, the sum includes one quantity that's *squared*
`(sqr a)`{.scheme} and one quantity that *isn't squared* `1`{.scheme}. This
feels "off", for a couple of reasons:

 - As a physicist: the variable `a`{.scheme} must have different units than the
   constants `1`{.scheme} and `0`{.scheme} in order to be dimensionally
   consistent. For example, if `a` were a length then the constants must be
   areas.
 - As a computer scientist: this feels like an ill-typed expression, like we're
   [mixing up encodings of semantically-distinct
   quantities](https://wiki.c2.com/?StringlyTyped).

Sure, those fears *might* be unfounded; but we can put ourselves at ease by
re-stating the equation entirely with squared terms. This requires introducing a
couple of extra variables, which we'll call `b`{.scheme} and `c`{.scheme}:

```scheme
(= (+ (sqr a) (sqr b))
   (sqr c))
```

Now let's solve this equation:

 - We know that `(= (sqr b) 1)`{.scheme} and `(= (sqr c) 0)`{.scheme}, by
   definition.
 - We can rearrange the equation to find that `(= (sqr a) (- (sqr b)))`{.scheme}
   and hence `(= (sqr a) -1)`{.scheme}

You may be tempted to "square root" these and say that `(= b 1)`{.scheme},
`(= c 0)`{.scheme} and (if you're familiar with complex numbers)
`(= a i)`{.scheme}; however, those are just *some* of the possible solutions to
these equations. Not only are there negative solutions too, but Geometric
Algebra provides even more by extending our arithmetic to include *extra
numbers*! These come in three flavours, one for each of our variables (apologies
for the intimidating names, which actually pre-date Geometric Algebra!):

 - We'll call solutions to `(= (sqr b) 1)`{.scheme} (other than `1`{.scheme} and
   `-1`{.scheme}) [hyperbolic
   units](https://en.wikipedia.org/wiki/Split-complex_number) and write them as
   `h₀`{.scheme}, `h₁`{.scheme}, `h₂`{.scheme}, etc.
 - We'll call solutions to `(= (sqr c) 0)`{.scheme} (other than `0`{.scheme})
   [dual units](https://en.wikipedia.org/wiki/Dual_numbers) and write them as
   `d₀`{.scheme}, `d₁`{.scheme}, `d₂`{.scheme}, etc.
 - We'll call solutions to `(= (sqr a) -1)`{.scheme} [imaginary
   units](https://en.wikipedia.org/wiki/Imaginary_number) and write them as
   `i₀`{.scheme}, `i₁`{.scheme}, `i₂`{.scheme}, etc.

Practical applications of GA will only use a few of these units, but I want my
code to support arbitrarily-many. Each of these units is a perfectly legitimate
`number`{.scheme}, but they are *not* part of `rational`{.scheme}; hence they
must occur at a higher level of our numerical tower. We'll define a new level
called `geometric`{.scheme} to contain all of them. I'll be referring to them as
GA/`geometric`{.scheme}/non-`rational`{.scheme} units". Note that we *cannot*
call them "irrational", since [that already means something
else](https://en.wikipedia.org/wiki/Irrational_number)!

<figure>

```
    ┌─┐ ┌──┐ ┌─┐
    │ └─┘  └─┘ │
    │  number  │
    │geometric |
    ├──────────┤
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
  Numerical tower with a `geometric`{.scheme} level at the top
 </figcaption>
</figure>

These non-`rational`{.scheme} numbers do not appear on the familiar [number
line](https://en.wikipedia.org/wiki/Number_line). We'll give their geometric
interpretation later. For now we'll just treat them as symbolic constants, the
same way we treat
[τ](https://tauday.com/tau-manifesto),
[𝑒](https://en.wikipedia.org/wiki/E_(mathematical_constant)),
[ϕ](https://en.wikipedia.org/wiki/Golden_ratio), etc.

### Representing Geometric Units In Scheme ###

This is pretty simple, since each unit contains two pieces of information: the
flavour and the index. We'll represent the flavour using a symbol: either
`h`{.scheme} or `d`{.scheme} or `i`{.scheme}. The index will just be a
`number`{.scheme} (we'll be sticking to `natural`{.scheme} indexes, but won't
enforce it). We'll combine these into a *pair*, by either giving them as inputs
to the `cons`{.scheme} operation, like `(cons 'd 0)`{.scheme} for `d₀`{.scheme};
or with a quotation, like `'(i . 2)`{.scheme} for `i₂`{.scheme} (where the `.`
makes this a pair, rather than a list).

It *looks like* we're calling functions named `d`{.scheme}, `h`{.scheme} and
`i`{.scheme} with a `number`{.scheme} as input; but for something to be a name,
there must be some underlying definition that it's referring to. In this case we
have no definitions (or, if you prefer, symbols are merely names for
themselves). These are ["uninterpreted
functions"](https://en.wikipedia.org/wiki/Uninterpreted_function), meaning
Racket will just pass around these expressions as-is.

It may feel like cheating to claim these values are "incorporated deeply" into
the language, compared to "usual" numbers. Admittedly the `natural`{.scheme}
type is a special case (due to its place-value notation), but it turns out that
*all* of Scheme's standard numerical tower relies on this "uninterpreted
function" trick!

Consider `integer`{.scheme}: this includes both `natural`{.scheme} numbers and
their negatives. The latter are represented by prefixing the former with a
`-`{.scheme} symbol, representing negation, which is left uninterpreted. The
higher levels, `rational`{.scheme} and `complex`{.scheme}, use uninterpreted
functions with two inputs (numerator & denominator, for `rational`{.scheme};
"real" & "imaginary" for `complex`{.scheme}).

In any case, here are some Scheme functions for manipulating these GA units:

```{pipe="./hide"}
(module+ test
  (define gen:flavour
    (gen:one-of '(d h i)))

  (define gen:unit-ga
    (gen:let ([f gen:flavour]
              [i gen:natural])
      (cons f i))))
```

```{.scheme pipe="./show"}
;; Predicates for spotting if a value is a GA unit (i.e. an appropriate pair)

(define/match (unit-d? n)
  [((cons 'd index)) (number? index)]
  [(_) #f])

(define/match (unit-h? n)
  [((cons 'h index)) (number? index)]
  [(_) #f])

(define/match (unit-i? n)
  [((cons 'i index)) (number? index)]
  [(_) #f])

(define unit-ga? (disjoin unit-h? unit-d? unit-i?))

;; Functions to access the flavour and index of a GA unit. The 'car'/'cdr'
;; functions return the first/second element of a pair.
(define unit-flavour car)
(define unit-index cdr)

;; Helper for sorting units alphabetically
(define (unit<? x y)
  (let ([fx (unit-flavour x)]
        [fy (unit-flavour y)]
        [ix (unit-index  x)]
        [iy (unit-index  y)])
    (or (symbol<? fx fy)
        (and (equal? fx fy) (< iy ix)))))
```

#### Parsing Geometric Units ####

Now we have a *representation* for `geometric` units, as a pair of flavour and
index, we can extend Racket's parser to understand the subscript form. There are
a few ways to do this: we'll use a custom
[`readtable`{.scheme}](https://docs.racket-lang.org/guide/hash-reader.html#%28part._readtable%29),
since that lets us target our changes to only affect numeric literals. This
works by defining a custom `read`{.scheme} function to consume characters of our
desired syntax from a `port`, and return a datum/syntax-object representing our
desired result; a custom readtable can then dispatch to that function when
appropriate characters are encountered in some code.

We'll leave the custom readtable until the end of our implementation, but we can
define a function to read GA units here, which will be invoked when the input
contains a `d`{.scheme}, `h`{.scheme} or `i`{.scheme}. This functionshould
consume any following subscript digits, and construct a pair to represent the
corresponding GA unit. If no subscript digits are found, it should fall-back to
the normal reader:

```{.scheme pipe="./show"}
(define (read-unit src in)
  ;; Look for flavour and index, using regexp capture groups
  (match (regexp-match #px"^([dhi])(₀|([₁₂₃₄₅₆₇₈₉][₀₁₂₃₄₅₆₇₈₉]*))" in)
    ;; No match: fall back to normal parsing rules (e.g. for symbols)
    [#f (read-syntax/recursive src in #f #f)]

    ;; Found a match: s is entire match, groups are captured substrings
    [(list s flavour-group index-group)
     (cons
       (string->symbol flavour-group)
       (string->number (for/fold ([digits index-group])
                                   ([i    "0123456789"]
                                    [char "₀₁₂₃₄₅₆₇₈₉"])
                           (string-replace digits char i))))])
```

```{pipe="./hide"}
(module+ test
  (define (string->unit s) (read-unit #f (open-input-string s)))

  (test-equal? "Parse dual 0"          (string->unit "d₀") '(d . 0))
  (test-equal? "Parse hyperbolic 0"    (string->unit "h₀") '(h . 0))
  (test-equal? "Parse imaginary 0"     (string->unit "i₀") '(i . 0))
  (test-equal? "Non-unit 0 left alone" (string->unit "a₀") 'a₀)

  (test-equal? "Parse dual 1"          (string->unit "d₁") '(d . 1))
  (test-equal? "Parse hyperbolic 1"    (string->unit "h₁") '(h . 1))
  (test-equal? "Parse imaginary 1"     (string->unit "i₁") '(i . 1))
  (test-equal? "Non-unit 1 left alone" (string->unit "a₁") 'a₁)

  (test-equal? "Parse dual 10"          (string->unit "d₁₀") '(d . 10))
  (test-equal? "Parse hyperbolic 10"    (string->unit "h₁₀") '(h . 10))
  (test-equal? "Parse imaginary 10"     (string->unit "i₁₀") '(i . 10))
  (test-equal? "Non-unit 10 left alone" (string->unit "a₁₀") 'a₁₀)

  (test-equal? "Invalid dual index left alone"       (string->unit "d₀₁") 'd₀₁)
  (test-equal? "Invalid hyperbolic index left alone" (string->unit "h₀₁") 'h₀₁)
  (test-equal? "Invalid imaginary index left alone"  (string->unit "i₀₁") 'i₀₁)
  (test-equal? "Invalid non-unit left alone"         (string->unit "a₀₁") 'a₀₁)

  (test-equal? "Dual without index is symbol"       (string->unit "d") 'd)
  (test-equal? "Hyperbolic without index is symbol" (string->unit "h") 'h)
  (test-equal? "Imaginary without index is symbol"  (string->unit "i") 'i)
)
```

## Code ##

```{.unwrap pipe="./dump geo-units.rkt"}
```

<details class="odd">
 <summary>Test results</summary>

```{pipe="./tests"}
```

</details>
</div>
