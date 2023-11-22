---
title: A Numerical Tower For Geometric Algebra
packages: ['racketWithRackCheck']
---

FIXME: Complex numbers are commutative so i = h0h1, it does not work to have just i0
FIXME: Quaternions also need combinations of h
FIXME: What is the meaning of i0 etc? They're anticommutative...
FIXME: A d point can be thought of as an ideal point at infinity
FIXME: Can we extend an n-dimensional GA or PGA to a CGA? Seems to require an
    extra h, but which to choose if we're already using those for our direction
    vectors? Seems naff to reserve h0 or something since GA pays without needingit

<!-- Unicode for copy/pasting:
×
₀₁₂₃₄₅₆₇₈₉
-->

```{pipe="sh > /dev/null"}
# Commands to use in our pipe attributes: both append code to the end of geo.rkt
# but one also shows it on the page and the other doesn't.
{
  echo '#!'"$(command -v bash)"
  echo 'tee -a geo.rkt'
  echo 'echo >> geo.rkt'
  echo 'echo >> geo.rkt'
} > show

{
  echo '#!'"$(command -v bash)"
  echo 'cat >> geo.rkt'
  echo 'echo >> geo.rkt'
  echo 'echo >> geo.rkt'
} > hide

chmod +x show hide
```

**Note:** In this post I will avoid my preferred [overbar
notation](/projects/units/negative_bar_notation.html), and instead write
negatives using a "minus sign" (like $-123$) for consistency with Scheme/Racket
notation.

## Introduction ##

I've recently taken an interest in [geometric algebra](https://bivector.net)
("GA", not to be confused with [algebraic
geometry](https://en.wikipedia.org/wiki/Algebraic_geometry)!), which extends the
usual "number line" in a way that elegantly models geometric ideas such as
circles, volumes, rotations, etc. I don't want to motivate or advocate for GA
itself, since there are plenty of [much better resources](https://bivector.net)
out there. Hence I'm going to focus on explanation and implementation of some
basic foundations of GA, since that's a good way to check my own understanding
of the subject!

In this post I won't assume any prior knowledge of GA or related structures, but
I will assume *some* familiarity with high-school algebra, like how to multiply
groups of terms, e.g.

$$(2 + x)(5 + 3y) = 10 + 5x + 6y + 3xy$$

I will also be using a lot of
[s-expressions](/blog/2017-08-29-s_expressions.html), AKA Lisp syntax or prefix
notation. This may be unfamiliar, but is pretty simple and removes ambiguities
like precedence: operations are written `(in parentheses)`, with the operation's
name/symbol first and its inputs after. For example, the above equation could be
written as follows:

```scheme
(= (× (+ 2 x) (+ 5 (× 3 y)))
   (+ 10 (× 5 x) (× 6 y) (× 3 x y)))
```

Rather than writing a bunch of standalone functions/classes, I wanted to think
holistically about how GA can be incorporated more deeply into a language. For a
project like this I prefer a language that's elegant, logical, principled and
consistent; rather than popular or fast. This makes Scheme a good choice; and
I'll focus on Racket since that's the version I'm most familiar with. Note that
I won't be paying much attention to efficiency (existing libraries address that
better than I could!)

This page is [active code](/projects/activecode) which generates a working
Racket library. I won't show *all* of the code on screen, so scroll to the
bottom for a link containing the full source, as well as the "view source" link
for this page's Markdown. Here's some preamble to get us going:

```{.scheme pipe="./show"}
#lang racket
;; We'll document our definitions using Scribble
(require scribble/srcdoc
         (for-doc racket/base scribble/manual))

;; We'll also define a suite of property-tests as we go, which the HTML export
;; will check to keep us honest!
(module+ test (require rackunit rackcheck-lib))
```

I also prefer to write multiplication with the usual `×` symbol, but Scheme uses
the asterisk `*` instead (since that's easier to type). We can fix this with a
definition:

```{pipe="cat >> geo.rkt"}
(provide
  (proc-doc/names
    × (-> number? ... number?) (z ...)
  ("Returns the product of all the given numbers, or the multiplicative unit "
   (racket 1)
   " if no numbers are given. This is an alias for Racket's "
   (racket *)
   ".")))
```


## Numbers in Scheme ##

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

Scheme uses the asterisk `*` for multiplication, since it's easy to type on a
standard US keyboard. I prefer the usual `×` symbol, for clarity. Hence we'll
define `×` to be `*`, so that both symbols will mean multiplication:

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

Numbers in Scheme can be `exact` or `inexact`. That's mostly irrelevant for
what we're doing, so in this post we'll stick to `exact` numbers. These are
arranged in a "numerical tower", where each level is a super-set of the ones
below, including:

 - `number`: This is the top level, containing every numeric value.
 - `complex`: These numbers use a clever trick for representing square roots of
   negative numbers. If you've encountered it before, great; if not, don't worry
   because we'll be replacing it with a more powerful trick!
 - `real`: This supposedly includes the whole "number line", but is actually
   rather silly since almost all of the "real numbers" can't be represented.
 - `rational`: Includes all fractions, positive and negative.
 - `integer`: Only whole numbers, positive and negative.

These are cumulative, so an `integer` like `-42` is also a `rational` (e.g. you
can think of it like `-42/1`), a `real` (like `-42.0`) and a `complex` (like
`-42+0i`, if you know what that means). We'll use insights from GA to alter,
extend and replace this tower!

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

 <figcaption>Racket's numerical tower (somewhat simplified)</figcaption>
</figure>

Racket already [extends Scheme's standard numerical
tower](https://docs.racket-lang.org/reference/numbers.html#%28tech._number%29),
and specifies some extra details of its implementation. In particular, Scheme
*allows* levels that are higher than `complex`, but Racket doesn't provide any;
so its `number` level adds nothing else to `complex`. We'll ignore the `complex`
level for now, since it turns out we can redefine it from GA primitives later.


Racket *does* add some "basement levels", which are strict subsets of `integer`:

 - `natural` is a sub-set of `integer` without negatives. It is closed
   under `+`, `×`, `gcd`, `lcm`, `max`, `min`, etc.; as well as `quotient` and
   `remainder` excluding the divisor `0`, and `expt` excluding `(expt 0 0)`.
 - `zero` is a sub-set of `natural` containing only `0`. It is closed under
   `+`, `×`, `gcd`, `lcm`, `max`, `min`, etc.

```{pipe="./hide"}
(module+ test
  (test-case
    "Check the Racket-specific levels that we'll use"
))
```

Even more fine-grained structure is [described in the Typed Racket
documentation](https://docs.racket-lang.org/ts-reference/type-ref.html#%28part._.Numeric_.Types%29)
(e.g. `byte`, a subset of `natural` between `0` and `255`) but we won't bother
with such size-based distinctions here.

Scheme also allows numbers to be `inexact`, which Racket implements with IEEE754
floating-point encodings. Exactness is unrelated to what we're doing, so in this
post we'll ignore `inexact` numbers, floating point numbers, and anything to do
with IEEE754 (so no negative zero, infinities or NaNs). Since those are the only
`real` values which aren't `rational`, we can also combine those two levels of
the tower!

## Geometric Numbers ##

I don't want to give a full account of GA, but a nice starting point for our
numerical tower is to consider the equation:

$$x^2 + 1 = 0$$

Or, as an s-expression (where [the `sqr`
function](https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28lib._racket%2Fmath..rkt%29._sqr%29%29)
multiplies a number by itself):

```scheme
(= (+ (sqr x) 1)
   0)
```

This looks simple enough, but let's take a moment to consider its broader
meaning. In particular, the sum includes one quantity that's *squared*
`(sqr x)` and one quantity that *isn't squared* `1`. This feels "off", for a
couple of reasons:

 - As a physicist: the variable `x` must have different units than the constants
   `1` and `0` in order to be dimensionally consistent. For example, if `x` were
   a length then the constants must be areas.
 - As a computer scientist: this feels like an ill-typed expression, like we're
   mixing up encodings of semantically-distinct quantities.

Sure, those fears *might* be unfounded; but we can put ourselves at ease by
re-stating the equation entirely with squared terms. This requires introducing a
couple of extra variables, which we'll call `y` and `z`:

``` scheme
(= (+ (sqr x) (sqr y))
   (sqr z))
```

Where `(= (sqr y) 1)` and `(= (sqr z) 0)` by definition. We can also
rearrange this equation to find that `(= (sqr x) (- (sqr y)))` and hence
`(= (sqr x) -1)`.

You may be tempted to "square root" these and say that `(= y 1)`, `(= z 0)` and
(if you're familiar with complex numbers) `(= x i)`; however, those are just
*some* of the solutions to these equations. Geometric Algebra provides more
solutions, by extending our arithmetic to include *extra numbers*! These come in
three flavours, and were initially developed using various standalone theories;
we'll unify them into the larger framework of GA, but will keep their original
names for posterity (again, don't worry if you've not seen these before):

 - We'll call solutions to `(= (sqr y) 1)` (other than 1 and -1) [hyperbolic
   units](https://en.wikipedia.org/wiki/Split-complex_number) and write them as
   `h₀`, `h₁`, `h₂`, etc.
 - We'll call solutions to `(= (sqr z) 0)` (other than 0) [dual
   units](https://en.wikipedia.org/wiki/Dual_numbers) and write them as `d₀`,
   `d₁`, `d₂`, etc.
 - We'll call solutions to `(= (sqr x) -1)` [imaginary
   units](https://en.wikipedia.org/wiki/Imaginary_number) and write them as
   `i₀`, `i₁`, `i₂`, etc.

Practical applications of GA will only use a few of these units, but I want my
code to support arbitrarily-many. Each of these units is a perfectly legitimate
`number`, but they are *not* `rational`; hence they must occur at a higher level
of our numerical tower. We'll define a new level called `geometric` to contain
all of them.

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

 <figcaption>Numerical tower with a `geometric` level at the top</figcaption>
</figure>

These non-`rational` numbers do not appear on the familiar [number
line](https://en.wikipedia.org/wiki/Number_line). We'll give their geometric
interpretation later, with a section explaining each flavour. For now we'll just
treat them as symbolic constants, just like we treat
[τ](https://tauday.com/tau-manifesto),
[𝑒](https://en.wikipedia.org/wiki/E_(mathematical_constant)),
[ϕ](https://en.wikipedia.org/wiki/Golden_ratio), etc.

<!--

### What About This `complex` Thing?  ###

Scheme's existing tower defines a level called `complex`, which contains the
`rational` numbers and a single imaginary unit `i` (AKA `i₀`), as well as every
sum and product of those numbers (i.e. it is closed under `+` and `×`).

If you've never encountered `complex` numbers before, they have two important
properties which will be relevant to our more-general framework of GA. Firstly
every `complex` number, no matter how much we mix and nest sums and products,
will always reduce down to a *single* sum, of the form `(+ A (× B i))`, where
`A` and `B` are `rational` (and potentially `zero`). A `complex` number will
never require *more* parts, like `(× C i i)`, `(× D i i i)`, etc. since we know
that `(= (× i i) -1)` (from the above definition of imaginary units), so all
higher powers of `i` will reduce down to the `(+ A (× B i))` form. Secondly
there is no meaningful way to further reduce this sum, so a `complex` number is
always made of two "parts"; despite being a *single* number!

Racket's notation for `complex` numbers is hence `A+Bi` (with no spaces); or
using `-` instead of `+` when `B` is negative.

The problem with a dedicated `complex` level is that it gives preferential
treatment to imaginary units relative to dual and hyperbolic units. Some might
find this desirable, but I've decided to extend my `geometric` level to
encompass `complex`, which makes the tower simpler and more consistent:

<figure>

```
     number
    ┌─┐ ┌──┐ ┌─┐
    │ └─┘  └─┘ │
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

 <figcaption>Our final numerical tower, with no distinct level for `complex`
 numbers.</figcaption>
</figure>

### Aside: The Many Structures Found Inside `geometric` ###

<figure>

```
     geometric
   ═════════════════════
  ║dual-quaternion║ hb │
  ├────╥──────────╢ yo │
  │    ║quaternion║ pl │
  │dual╟──────────╢ ei │
  │    ║ complex  ║ rc │
  ├────╨──────────╨────┤
   ╲  rational        ┌╯
    ├──────────┬──────╯
    │ integer  │
────┼──────────┼────────────
    │ natural  │
    ├──────────┤
    │   zero   │
    └──────────┘
```

 <figcaption>The `hyperbolic` numbers contain `rational`, and are contained in
 `geometric`; but are distinct from `dual`, `complex` and their extensions
 </figcaption>
</figure>
</details>

Whilst `complex` is certainly a useful type of number, the reason I don't want
it as a level above `rational` is there are other numbers above `rational`,
which are neither above or below `complex`.

The and vice "sibling" other non-`rational` units form
perfectly there are two numbers As mentioned above, these non-`rational` units have appeared in various theories
over the course of several centuries. You don't need to know or care about these
different algebras, since they crop up naturally as patterns in GA, but since a
numerical tower is all about representing such nested structures it seems
prudent to define them for those who care!

#### Complex Numbers ###

If we extend the `rational` numbers with a single imaginary unit, say `i₀`, we
get a self-contained numerical system called the complex numbers. This has found
uses which is
useful in 2D geometry, wave mechanics, electrical engineering, etc. Indeed, this
already exists in the standard Scheme tower, as the `complex` level!

#### Quaternions And Hyperimaginary Numbers ####

Extending `complex` with another imaginary unit doesn't give a useful theory,
but having *three* imaginary units (`i₀`, `i₁` and `i₂`) gives another useful
system called the quaternions; which is especially useful for describing 3D
rotations. The numerical tower in [Kawa
Scheme](https://www.gnu.org/software/kawa/Quaternions.html) has a `quaternion`
level above `complex`, so we'll do the same!

There is actually an infinite family of such "hypercomplex" theories, each with
twice as many units as the last (when counting all the imaginary units *and* the
unique `rational` unit `1`); I'll call these *hyperimaginary*, to distinguish
them from the other flavours. The
[octonions](https://en.wikipedia.org/wiki/Octonion) have seven imaginary units,
the sedeneons have fifteen, and so on. However, the further we go, the less
useful those theories become, as they follow fewer (and weaker) algebraic
rules. In particular, everything past the quaternions violates associativity,
which I don't consider "numeric" enough to live in our tower!

<figure>

```
       number
    ┌─┐ ┌──┐ ┌─┐
    │ └─┘  └─┘ │
    │geometric |
    ├──────────┤
    │quaternion│
    ├──────────┤
    │ complex  │
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

 <figcaption>The `quaternion` level contains `complex`, and is contained by
 `geometric`</figcaption>
</figure>

#### Dual Numbers ####

If we extend `rational` with a single *dual* unit, say `d₀`, we get the system
of [dual numbers](), which is useful for e.g. automatic differentiation. Dual
numbers don't include an imaginary unit, and `complex` numbers don't include a
dual unit, so neither is a sub-set of the other. Hence they'll need to live
side-by-side in our tower!

<figure>

```
     number
    ┌─┐ ┌──┐ ┌─┐
    │ └─┘  └─┘ │
    │geometric |
  ┌─┴──╥──╳───╳┴──┐
  │    ║quaternion│
  │dual╟──────────┤
  │    ║ complex  │
  └┬┬╳─╨───────┬┬─┘
   \│ rational ├╯
    ├──────────┤
    │ integer  │
────┼──────────┼────
    │ natural  │
    ├──────────┤
    │   zero   │
    └──────────┘
```

 <figcaption>Adding `dual` numbers requires bodging our tower, so multiple
 "levels" can occur at the same height!</figcaption>
</figure>

#### Dual Quaternions ####

The combination of `quaternion` and `dual` forms a useful theory called the
[dual quaternions](https://en.wikipedia.org/wiki/Dual_quaternion), which are
used to describe rotation and translation in 3D space. Thankfully there's a
perfect spot for it in our unfortunately-wonky tower:

<figure>

```
       number
   ┌─┐ ┌──┐ ┌─┐ ╔╦╕
   │ └─┘  └─┘ │ ╢╟├╯
   │geometric ╞╧╣╚╡
  ╭┴──────────┴─╨─┤
  │dual-quaternion│
  ├────╥──────────┤
  │    ║quaternion│
  │dual╟──────────┤
  │    ║ complex  │
  └┬┬╳─╨───────┬┬─┘
   \│ rational ├╯
    ├──────────┤
    │ integer  │
────┼──────────┼────
    │ natural  │
    ├──────────┤
    │   zero   │
    └──────────┘
```

 <figcaption>The `dual-quaternion` level combines values from both `dual` and
 `quaternion`</figcaption>
</figure>

#### Hyperbolic Numbers ####

We've got one flavour of unit left, the hyperbolics, and you may have guessed
that we can *also* extend `rational` with one of those, say `h₀`, to get
[the hyperbolic numbers](https://en.wikipedia.org/wiki/Split-complex_number).

-->

## Geometric Arithmetic ##

Adding and multiplying `geometric` numbers always results in another `geometric`
number (i.e. `geometric` is closed under `+` and `×`).  For example:

 - Adding a unit to itself gives a `rational` multiple, e.g. `(= (+ h₀ h₀) 2h₀)`
 - To multiply units we combine their `rational` parts and juxtapose their
   symbols, e.g. `(= (× 2d₀ 3h₁) 6d₀h₁)`
 - A unit multiplied by itself can be simplified further, according to the
   definitions above, e.g. `(= (× 3i₀ 7i₀) 21i₀i₀ -21)`
 - Addition cannot meaningfully combine *different* units, so a general
   `geometric` number is represented as a "sum of parts" (which is nevertheless
   a single number!), e.g. `(= (+ 7 -3i₀ 5h₀ i₀ h₁ -2d₀d₁ 2i₀) 7+5h₀+h₁-2d₀d₁)`
 - Addition and multiplication of general `geometric` numbers proceeds as if
   their "sum of parts" were ordinary terms grouped using parentheses (like the
   example in the introduction), e.g. `(= (× 2+d₂ 5+3h₀) 10+5d₂+6h₀+3d₂h₀)`

### The Geometric Product Anti-Commutes ###

The most important feature of Geometric Algebra is that the GA units
*anti-commute* when multiplied together. This means the order of units appearing
in a product is significant: a combination like `h₀i₀` is the *negative* of the
combination `i₀h₀`. This extends to larger combinations too, with the result
changing sign whenever a pair of *neighbouring* units are swapped:

```scheme
(= (× i₀ h₁ d₀ i₀ h₁)
   i₀h₁d₀i₀h₁         ;; Shorthand for this product, in the same order
   -i₀h₁i₀d₀h₁        ;; Swap d₀i₀ to -i₀d₀
   i₀i₀h₁d₀h₁         ;; Swap -h₁i₀ to i₀h₁
   -h₁d₀h₁            ;; Since (= i₀i₀ -1), by definition of imaginary units
   d₀h₁h₁             ;; Swap -h₁d₀ to d₀h₁
   d₀)                ;; Since (= h₁h₁ 1), by definition of hyperbolic units
```

This "geometric product" is just an extension of ordinary multiplication, since
`rational` numbers commute in the usual way, with each other and with GA units.
From now on, we will always convert `geometric` numbers into alphabetical order
of their sums and products (this will be their canonical form, similar to how we
always convert fractions to their lowest form).

## Implementing `geometric` ##

### Encoding Numbers ###

We'll represent each flavour of GA unit using a Racket `struct`, with the index
as a field:

```{.scheme pipe="./show"}
```

Each "part" of a `geometric` number will be list of `rational` or GA unit
elements, and a `geometric` number contains a list of its parts:

```{pipe="./hide"}
;; Helper functions
```

TODO: Add contracts, printers, readers, etc.

We'll write a `canonical` function, which will iterate through the parts of a
`geometric` number to ensure:

 - The units within each part are in alphabetical order (negating as needed)
 - Any repeated (i.e. squared) units within a part are replaced according to
   their definition (with -1, 0 or 1)
 - All `rational` elements within a part are multiplied into a single `rational`
 - The parts of the `geometric` number are listed in alphabetical order
 - Any parts with the same list of units are combined (adding their `rational`
   entries)
 - If the result contains only a `rational`, that is returned without being
   wrapped in a `geometric` struct

;; This function is very slow, since it will shuttle back-and-forth over the
;; list (given in zipper form).
(define/match (canonical-part-acc elems acc)
  ;; Base case: no more elements to process, reverse our accumulated units
  [('() acc) (reverse acc)]

  ;; Short-circuit: if any element is zero, this whole thing is zero
  [((cons 0 _) _) '(0)]

  ;; A pair of identical dual units makes this whole thing zero
  [((cons (dual a) _) (cons (dual b) _)) #:when (= a b) '(0)]

  ;; A pair of rationals get multiplied together
  [((cons (? rational? a) us) (cons (? rational? b) accs))
   (canonical-part-acc (cons (× a b) us) accs)]

  ;; Rationals move before geometric units (no negative, as they commute)

  [((cons (? rational? u) us)
    (cons (and acc (or (hyperbolic _) (dual _) (imaginary _))) accs))
  (canonical-part-acc (cons u (cons acc us)) accs)]

  ;; A pair of identical hyperbolic units can be replaced with 1. We can't just
  ;; remove them entirely, since we need to ensure at least one element remains!
  [((cons (hyperbolic a) us) (cons (hyperbolic b) accs))
   #:when (= a b)
   (canonical-part-acc (cons 1 us) accs)]

  ;; A pair of identical imaginary units can be replaced with -1.
  [((cons (imaginary a) us) (cons (imaginary b) accs))
   #:when (= a b)
   (canonical-part-acc (cons -1 us) accs)]

  ;; If indexes appear in descending order, swap them and introduce a negative

  [((cons (and u (hyperbolic a)) us) (cons (and acc (hyperbolic b)) accs))
   #:when (< a b)
   (canonical-part-acc (cons -1 (cons u (cons acc us))) accs)]

  [((cons (and u (dual a)) us) (cons (and acc (dual b)) accs))
   #:when (< a b)
   (canonical-part-acc (cons -1 (cons u (cons acc us))) accs)]

  [((cons (and u (imaginary a)) us) (cons (and acc (imaginary b)) accs))
   #:when (< a b)
   (canonical-part-acc (cons -1 (cons u (cons acc us))) accs)]

  ;; If flavours aren't in alphabetical order, swap them and negate

  [((cons (and u (dual _)) us)
    (cons (and acc (or (hyperbolic _) (imaginary _))) accs))
   (canonical-part-acc (cons -1 (cons u (cons acc us))) accs)]

  [((cons (and u (hyperbolic _)) us) (cons (and acc (imaginary _)) accs))
   (canonical-part-acc (cons -1 (cons u (cons acc us))) accs)]

  ;; If none of the above cases match, use this unit as-is
  [((cons u us) accs) (canonical-part-acc us (cons u accs))])

(define (canonical-part p)
  ;; Always include a 1 multiplier, to ensure the result will start with a
  ;; single rational. The overhead is low if the part is already canonical,
  ;; since the existing rational multiplier will be the first element.
  (canonical-part-acc p '(1)))

(define/match (compare-parts a b)
  ;; If both run out at once, they're equal
  [('() '()) 'EQ]

  ;; Shorter is less than
  [((cons _ _) '()) 'GT]
  [('() (cons _ _)) 'LT]

  ;; Skip over equal units
  [((cons (? rational?) a) (cons (? rational?) b)) (compare-parts a b)]
  [((cons (dual a) as) (cons (dual b) bs)) #:when (= a b) (compare-parts as bs)]
  [((cons (hyperbolic a) as) (cons (hyperbolic b) bs)) #:when (= a b) (compare-parts as bs)]
  [((cons (imaginary a) as) (cons (imaginary b) bs)) #:when (= a b) (compare-parts as bs)]

  ;; Compare indices (equal are handled above)
  [((cons (dual       a) _) (cons (dual       b) _)) (if (< a b) 'LT 'GT)]
  [((cons (hyperbolic a) _) (cons (hyperbolic b) _)) (if (< a b) 'LT 'GT)]
  [((cons (imaginary  a) _) (cons (imaginary  b) _)) (if (< a b) 'LT 'GT)]

  ;; Compare flavours
  [((cons (dual _) _) (cons (or (hyperbolic _) (imaginary _)) _)) 'LT]
  [((cons (or (hyperbolic _) (imaginary _)) _) (cons (dual _) _)) 'GT]
  [((cons (hyperbolic _) _) (cons (imaginary _) _)) 'LT]
  [((cons (imaginary _) _) (cons (hyperbolic _) _)) 'GT]

  ;; Should never reach here
)

(define/match (canonical-parts parts acc)
  ;; Base case: reverse the elements we accumulated
  [('() acc) (reverse acc)]

  ;; Wrap purely rational parts into multipliers of no units
  [((cons (? rational? x) parts) acc)
   (canonical-parts (cons (list x) parts) acc)]

  ;; Combine neighbouring rational parts
  [((cons (or (? rational? x) (list (? rational? x))) parts)
    (cons (or (? rational? y) (list (? rational? y))) acc))
   (canonical-parts (cons (+ x y) parts) acc)]

  ;; Move rational parts before those with geometric units
  [((cons (or (? rational? x) (list (? rational? x))) parts)
    (cons (and acc (not (? rational?) (list (? rational?)))) accs))
   (canonical-parts (cons x (cons acc parts)) accs)]

  ;; When neighbouring unit lists are the same, keep one and combine their
  ;; rational multipliers
  [((cons (and part (cons m1 u)) parts) (cons (and acc (cons m2 _)) accs))
   (match (compare-parts part acc)
    ['EQ (canonical-parts (cons (cons (× m1 m2) u) parts) accs)]

    ;; Otherwise arrange them alphabetically and recurse
    ['LT (canonical-parts (cons part (cons acc parts)) accs)]
    ['GT (canonical-parts parts (cons part (cons acc accs)))]
    )]

  ;; Otherwise accept this part and move on
  [((cons part parts) acc) (canonical-parts parts (cons part acc))])

```{.scheme pipe="./show"}
;; Converts the given number? to its canonical form
(define/match (canonical n)
  [(geometric parts)
   (match (canonical-parts (map canonical-part parts) '())
     ;; An empty sum (of parts) is defined to be zero
     ['() 0]

     ;; If the units annihilate to leave a rational, return it as-is
     [(list (? rational? n)) n]

     ;; Otherwise wrap up the resulting parts in a new geometric number
     [parts (geometric parts)])]

  ;; Return anything else unchanged (e.g. rationals)
  [n n])
```

with their ration are always in canonical form, we'll write a
`canonical` function to rearrange them as necessary will Addition is trivial, since we just append the parts lists


<!--

them gives us are extra numbers have structure which you might have encountered elsewhere
(if not, don't worry!).

 - Adding a `rational` to an imaginary, like `2+3i₀`, give us [complex
   numbers](https://en.wikipedia.org/wiki/Complex_number) (usually written like
   $$2+3i$$)
 - Adding a `rational` to a dual, like `2+3d₀`, gives us [dual numbers](https://en.wikipedia.org/wiki/Dual_numbers)
 : $$A + B𝜀$$ becomes `(+ A (× B d₀))` (+ A (× B i₀))
 - Quaternions: $$A + Bi + Cj + Dk$$ becomes
   `(+ A (× B i₀) (× C i₁) (× D i₂))`
 - Dual quaternions: $$A + Bi + Cj + Dk + E𝜀 + F𝜀i + G𝜀j + H𝜀k$$ becomes
   `(+ A (× B i₀) (× C i₁) (× D i₂) (× d₀ (+ E (× F i₀) (× G i₁) (× H i₂))))`

From now on we can ignore those `x`, `y` and `z`
-->

## Final Thoughts ##

Here's a URI containing all of the Racket code generated by this post:

```{.unwrap pipe="sh | pandoc -t json"}
# Use a data URL. These default to US-ASCII encoding, so we need to
# specify UTF8 for our unicode symbols.
printf '<a download="geo.rkt" href="data:text/plain;charset=utf-8;base64,'

# The contents needs to be URL-encoded. That requires extra dependencies, which
# I'd rather avoid. Instead, we can use GNU coreutils to Base64-encode it, which
# will avoid the need for a separate URL-encoding step.
base64 -w0 < geo.rkt
printf '">DOWNLOAD RACKET CODE</a>'
```

```{pipe="sh"}
raco test geo.rkt
```
