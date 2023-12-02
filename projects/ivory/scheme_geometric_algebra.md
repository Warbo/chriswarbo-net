---
title: A Numerical Tower For Geometric Algebra
packages: ['racketWithRackCheck']
---

<!--
FIXME: Can we extend an n-dimensional GA or PGA to a CGA? Seems to require an
    extra h, but which to choose if we're already using those for our direction
    vectors? Seems naff to reserve h0 or something since GA pays without needing
    it
TODO: Reader macros
TODO: Use provide, etc. to replace number?, zero?, +, etc.
TODO: Applications: include subalgebras corresponding to existing things, e.g.
  complex numbers, quaternions, etc. (hence we gain their applications too)
TODO: Using `geometric` to implement (vanilla) GA, PGA, CGA, etc.
-->

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

<nav id="toc"></nav>
<script type="text/javascript" src="/js/table-of-contents.js"></script>
<div data-content>

## Introduction ##

I've recently taken an interest in [geometric algebra](https://bivector.net)
("GA", not to be confused with [algebraic
geometry](https://en.wikipedia.org/wiki/Algebraic_geometry)!), which goes beyond
the usual ["number line"](https://en.wikipedia.org/wiki/Number_line) in a way
that elegantly models geometric ideas such as circles, volumes, rotations,
etc. I don't want to motivate or advocate for GA itself, since there are plenty
of [much better resources](https://bivector.net/doc.html#five) out there. Hence
I'm going to focus on explanation and implementation of some basic foundations
of GA, since that's a good way to check my own understanding of the subject!

 Here's the start of the implementation, to get us
going:

```{.scheme pipe="./show"}
#lang racket
```

```{pipe="./hide"}
;; We'll write our documentation using Scribble
(require scribble/srcdoc
         (for-doc racket/base scribble/manual))

;; We'll define a test suite as we go, as a "sub-module" called 'test'
(module+ test (require rackunit rackcheck-lib))
```

## Geometric Numbers ##

I don't want to give a full account of GA, but a nice starting point for our
numerical tower is to consider the equation:

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
`(sqr a)` and one quantity that *isn't squared* `1`. This feels "off", for a
couple of reasons:

 - As a physicist: the variable `a` must have different units than the constants
   `1` and `0` in order to be dimensionally consistent. For example, if `a` were
   a length then the constants must be areas.
 - As a computer scientist: this feels like an ill-typed expression, like we're
   mixing up encodings of semantically-distinct quantities.

Sure, those fears *might* be unfounded; but we can put ourselves at ease by
re-stating the equation entirely with squared terms. This requires introducing a
couple of extra variables, which we'll call `b` and `c`:

```scheme
(= (+ (sqr a) (sqr b))
   (sqr c))
```

Now let's solve this equation:

 - We know that `(= (sqr b) 1)` and `(= (sqr c) 0)`, by definition.
 - We can rearrange the equation to find that `(= (sqr a) (- (sqr b)))`,
   and hence `(= (sqr a) -1)`

You may be tempted to "square root" these and say that `(= b 1)`, `(= c 0)` and
(if you're familiar with complex numbers) `(= a i)`; however, those are just
*some* of the possible solutions to these equations. Not only are there negative
solutions too, but Geometric Algebra provides even more by extending our
arithmetic to include *extra numbers*! These come in three flavours (apologies
for the intimidating names; they actually pre-date Geometric Algebra!):

 - We'll call solutions to `(= (sqr b) 1)` (other than 1 and -1) [hyperbolic
   units](https://en.wikipedia.org/wiki/Split-complex_number) and write them as
   `h₀`, `h₁`, `h₂`, etc.
 - We'll call solutions to `(= (sqr c) 0)` (other than 0) [dual
   units](https://en.wikipedia.org/wiki/Dual_numbers) and write them as `d₀`,
   `d₁`, `d₂`, etc.
 - We'll call solutions to `(= (sqr a) -1)` [imaginary
   units](https://en.wikipedia.org/wiki/Imaginary_number) and write them as
   `i₀`, `i₁`, `i₂`, etc.

Practical applications of GA will only use a few of these units, but I want my
code to support arbitrarily-many. Each of these units is a perfectly legitimate
`number`, but they are *not* part of `rational`; hence they must occur at a
higher level of our numerical tower. We'll define a new level called `geometric`
to contain all of them. I'll be referring to them as "GA units" or
"non-`rational` units"; we *cannot* call them "irrational", since that already
means something else!

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
treat them as symbolic constants, the same way we treat
[τ](https://tauday.com/tau-manifesto),
[𝑒](https://en.wikipedia.org/wiki/E_(mathematical_constant)),
[ϕ](https://en.wikipedia.org/wiki/Golden_ratio), etc.

### Representing GA Units In Scheme ###

This is pretty simple, since each unit contains two pieces of information: the
flavour and the index. We'll represent the flavour using a symbol: either `h` or
`d` or `i`. The index will just be a `number` (we'll be sticking to `natural`
indexes, but won't enforce that). We'll combine these into a *pair*, by either
giving them as inputs to the `cons` operation, like `(cons 'd 0)` for `d₀`; or
with a quotation, like `'(i . 2)` for `i₂` (where the `.` makes this a pair,
rather than a list).

It *looks like* we're calling functions named `d`, `h` and `i` with a `number`
as input; but for something to be a name, there must be some underlying
definition that it's referring to. In this case we have no definitions (or, if
you prefer, symbols are merely names for themselves). These are ["uninterpreted
functions"](https://en.wikipedia.org/wiki/Uninterpreted_function), meaning
Racket will just pass around these expressions as-is.

It may feel like cheating to claim these values are "incorporated deeply" into
the language, compared to "usual" numbers. Admittedly the `natural` type is a
special case (due to its place-value notation), but it turns out that *all* of
Scheme's standard numerical tower relies on this "uninterpreted function" trick!

Consider the simplest level, `integer`: this includes both `natural` numbers and
their negatives. The latter are represented by prefixing the former with a `-`
symbol: in other words, as an uninterpreted function call! The higher levels,
`rational` and `complex`, use uninterpreted functions with two inputs (numerator
& denominator, for `rational`; "real" & "imaginary" for `complex`).

In any case, here are some *actual* functions for operating on these GA units:

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

<!-- TODO: READER MACROS

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
number (i.e. `geometric` is closed under `+` and `×`). The rules of their
arithmetic should mostly match what you're used to, for example:

 - Any `geometric` number multiplied by `0`, or added to its negative, is `0`
 - Any `geometric` number divided by itself, or multiplied by its reciprocal, is
   `1`
 - Adding a `geometric` number to itself is the same as multiplying by a
   `natural`, e.g.
   `(= (+ h₀ h₀) (× 2 h₀))`. This extends to subtraction (multiplying by an
   `integer`) and division (multiplying by a `rational`)
 - Addition is associative and commutative, and nested sums can be flattened,
   e.g. `(= (+ d₀ (+ i₁ i₀)) (+ d₀ (+ i₀ i₁)) (+ (+ d₀ i₀) i₁) (+ d₀ i₀ i₁))`
 - Multiplication is associative and nested products can be flattened, e.g.
   `(= (× h₀ (× h₁ i₀)) (× (× h₀ h₁) i₀) (× h₀ h₁ i₀))`
 - Multiplication "distributes over" addition in the usual way, e.g.
   `(= (× d₁ (+ h₀ i₀)) (+ (× d₁ h₀) (× d₁ i₀)))`
 - Multiplying a GA unit by itself (squaring) produces an `integer`, according
   to the definition of each "flavour", e.g. `(= (× 3 i₀ i₀) (× 3 -1) -3)`

A product which includes *different* GA units, like `(× 5 d₀ h₁)`, cannot be
simplified further. We allow these products to be abbreviated as a single value,
like `5d₀h₁`. Likewise, a sum involving different (products of) GA units cannot
be simplified, e.g. `(+ 4d₀ i₀i₁)`: we allow such sums to be abbreviated as a
single value, whose "parts" are separated by `+`, like `4d₀+i₀i₁` (note the lack
of spaces!).

There are many equivalent ways to write such products and sums, so we declare
that a general `geometric` number should be written in the form of a sum of
products. For example `(× 3d₁ (+ h₁ i₀))` *does not* have this form (it's the
product of a product and a sum), but we can distribute the multiplication to get
`3d₁h₁+3d₁i₀`, which has the correct form. This makes it easier to spot when two
`geometric` numbers are the same (similar to why we write fractions in their
lowest form).

Addition and multiplication of general `geometric` numbers proceeds as if their
sums and products were ordinary terms grouped using parentheses, like the
example in the introduction, e.g. `(= (× 2+d₂ 5+3h₀) 10+5d₂+6h₀+3d₂h₀)`

### The Geometric Product Anti-Commutes! ###

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

This may seem weird, but it only affects the GA units; any `rational` numbers
occuring in a product commute in the usual way, both with each other and with GA
units.

From now on, not only will we convert all `geometric` numbers into a
sum-of-products, but we will also write their units in alphabetical order,
negating when a swap is needed.

### Representing Sums And Products in Scheme ###

Since products and sums don't always "reduce" to something simpler, we'll use
the same "uninterpreted function" trick as we did for representing GA units.
This time, since we're dealing with products and sums, we'll use the symbols `×`
and `+`. Also, since they can accept any amount of inputs, we'll keep them in a
list rather than a pair.

This makes the implementation of multiplication and addition pretty simple: we
just wrap all of the inputs into a product or sum, respectively. We then apply a
`canonical` function (defined in the next section), to rearrange them into our
preferred form (sum-of-products, in alphabetical order):

```{.scheme pipe="./show"}
;; Shorthands for creating products and sums
(define (geo-× . args) (canonical (cons '× args)))
(define (geo-+ . args) (canonical (cons '+ args)))

;; Predicates for spotting products, sums and general geometric numbers

(define/match (geo-×? n)
  [((cons '× ns)) (andmap geometric? ns)]
  [(_) #f])

(define/match (geo-+? n)
  [((cons '+ ns)) (andmap geometric? ns)]
  [(_) #f])

(define geo? (disjoin unit-ga? geo-×? geo-+?))

(define geometric? (disjoin number? geo?))
```

### Reducing To Canonical Form ###

Our GA implementation relies on the canonical form mentioned above, with a few
extra conditions arising from our encoding as Racket values. We convert
`geometric` numbers into this form using a `canonical` function, which uses
pattern-matching to rearrange various non-canonical structures; and recurses
until the standard sum-of-products form is reached.

```{pipe="./hide"}
;; Helper functions

;; Append an element to the end of a list
(define (snoc xs x) (append xs (list x)))

(define geo-zero? (conjoin number? zero?))

;; Return #f when no element of xs satisfies pred, otherwise return (list a b c)
;; where (append a (cons b c)) = xs, and (pred b)
(define ((split pred) xs)
  (match/values (splitf-at xs (negate pred))
    [(xs (cons y ys)) (list xs y ys)]
    [(_  _          ) #f]))

(module+ test
  (check-property
    (property split-works ([xs (gen:list gen:natural)])
      (match ((split even?) xs)
        [#f (test-equal? "No split means no matches" (filter even? xs) '())]
        [(list pre x suf)
          (test-equal? "Split should pick first match" (filter even? pre) '())
          (check-pred even? x)
          (test-equal? "Split results should append to input"
            (append pre (cons x suf)) xs)]))))

;; Check (pred (nth 0 xs) (nth 1 xs)), (pred (nth 1 xs) (nth 2 xs)), etc. until
;; we find a neighbouring pair elements a & b which pass the predicate, then
;; return (list pre a b suf), where (append pre (cons a (cons b suf))) = xs.
;; If no such pair of elements is found, return #f.
(define ((split-pair pred) xs)
  (define/match (go xs ys)
    [((cons x xs) '()        ) (go xs (list x))]
    [('()         ys         ) #f]
    [((cons x xs) (cons y ys))
     (if (pred y x)
       (list (reverse ys) y x xs)
       (go xs (cons x (cons y ys))))])

  (go xs '()))

(module+ test
  (check-property
    (property split-pair-works ([xs (gen:list gen:natural)])
      (match ((split-pair <) xs)
        [#f
         (test-equal? "When no pairs are <, xs must be reverse sorted"
           xs (reverse (sort xs <)))]

        [(list pre x y suf)
         (test-equal? "split-pair should split input" `(,@pre ,x ,y ,@suf) xs)
         (test-check "split-pair should find pair matching predicate" < x y)

         ;; For x & y to be the first pair where x < y, that means no elements
         ;; before y (i.e. in (snoc pre x)) should be greater than their
         ;; predecessor. Or, in other words, the elements of (snoc pre x) should
         ;; be in descending order. To test this, sorting should be a no-op.
         (test-equal? "split-pair should return first pair matching predicate"
           (snoc pre x)
           (sort (snoc pre x) >))]))))

;; Compare whether xs is less than ys: shorter lists are less than
(define (list<? xs ys <?)
  (cond
    [(and (empty? xs) (empty? ys)) #f]      ;; Not < when both are empty
    [(<   (length xs) (length ys)) #t]      ;; Fewer elements is <
    [(<   (length ys) (length xs)) #f]      ;; More elements mis not <
    [(<?  (car    xs) (car    ys)) #t]      ;; Smaller head is <
    [(<?  (car    ys) (car    xs)) #f]      ;; Bigger head is not <
    [else (list<? (cdr xs) (cdr ys) <?)]))  ;; Recurse on tails

(module+ test
  (check-property
    (property list<?-works ([x  gen:natural]
                            [xs (gen:list gen:natural)]
                            [ys (gen:list gen:natural)])
      (test-false "Lists not list<? themselves" (list<? xs xs          <))
      (test-true  "list<? shorter longer"       (list<? xs (cons x xs) <))
      (test-false "Not list<? longer shorter"   (list<? (cons x xs) xs <))
      (match* ((list<? xs ys <) (list<? ys xs <))
        [(#t #t) (test-false "Can't be list<? both ways round" #t)]
        [(#f #f) (test-true "Neither list<? means equal?" (equal? xs ys))]
        [((or (and #t (app (const* xs ys) lo hi))
              (and #f (app (const* ys xs) lo hi))) _)
         (test-true "Prepending equal elements maintains list<?"
           (list<? (cons x lo) (cons x hi) <))
         (test-true "Appending the same number of things maintains list<?"
           (list<? (append lo (shuffle (append xs ys)))
                   (append hi (shuffle (append xs ys)))
                   <))])
      )))
```

```{.scheme pipe="./show"}
;; Converts the given number? to its canonical form
(define/match (canonical n)
```

A `number` which isn't a product or sum is already in canonical form:

```{.scheme pipe="./show"}
  [((? geometric? (not (? geo?)))) n]
  [((? unit-ga?)) n]
```

The easiest fixes to make are replacing products and sums that have fewer than
two elements:

```{.scheme pipe="./show"}
  ;; Empty sums/products are equivalent to their respective identities
  [('(+)) 0]
  [('(×)) 1]

  ;; The sum/product of a single number is just that number
  [(`(+ ,n)) (canonical n)]
  [(`(× ,n)) (canonical n)]
```

The next few rules use a helper function called `split`, which splits a list at
an element satisfying the given predicate. This lets us spot problems anywhere
inside a product or sum.

```{.scheme pipe="./show"}
  ;; Any product containing 0 is itself 0
  [((cons '× (app (split geo-zero?) (list _ _ _)))) 0]

  ;; Remove 0 from sums, as it makes no difference
  [((cons '+ (app (split geo-zero?) (list pre _ suf))))
   (apply geo-+ (append pre suf))]

  ;; Remove 1 from products, as it makes no difference
  [((cons '× (app (split (curry equal? 1)) (list pre _ suf))))
   (apply geo-× (append pre suf))]

  ;; Unwrap nested sums
  [((cons '+ (app (split geo-+?) (list pre (cons '+ xs) suf))))
   (apply geo-+ (append pre xs suf))]

  ;; Unwrap nested products. The inner elements must be spliced in place of the
  ;; nested product, to maintain the order of multiplication.
  [((cons '× (app (split geo-×?) (list pre (cons '× xs) suf))))
   (apply geo-× (append pre xs suf))]

  ;; Recurse when elements of a sum or product aren't canonical
  [((cons (and sym (or '× '+)) (app (split (lambda (x)
                                             (not (equal? x (canonical x)))))
                                    (list pre x suf))))
   (canonical (cons sym (append pre (list (canonical x)) suf)))]
```

The remaining cases spot patterns between neighbouring elements inside products
and sums: for example, when they're not in alphabetical order; or when a GA unit
is multiplied by itself. We implement these using a helper function called
`split-pair`, which is like `split` but finds a pair of neighbouring elements
that satisfy the given predicate. Note that we only need to compare neighbours,
since the sorting rules will bring problematic elements together!

```{.scheme pipe="./show"}
  ;; Replace the product of two equal? GA units, based on their flavour
  [((cons '× (app (split-pair equal?)     ;; Match pairs of equal? values
                  (list
                    pre                   ;; Binds to elements preceding pair
                    (? unit-ga?           ;; Match only when we found a GA unit
                       (app unit-flavour  ;; Select result based on flavour
                            (or (and 'd (app (const  0) x))
                                (and 'h (app (const  1) x))
                                (and 'i (app (const -1) x)))))
                    _        ;; Ignore 2nd unit, since it's equal? to the 1st
                    suf))))  ;; Bind to elements occurring after matching pair
   (apply geo-× (append (list x) pre suf))]

  ;; Combine elements which involve no GA units, products or sums
  [((cons (and sym (or '× '+))
          ;; Find neighbouring elements which aren't GA units, products or sums
          (app (split-pair (lambda (x y) (not (or (geo? x) (geo? y)))))
               (list pre x y suf))))  ;; Bind results
   ;; Remove x and y, combine them accoding to sym, and prepend the result on to
   ;; the remaining list
   (canonical (append
                (list sym (match sym ['× (× x y)] ['+ (+ x y)]))
                pre
                suf))]

  ;; A sum of two terms containing the same GA units, can be replaced by a
  ;; single multiple of those units; that multiple being the sum of the two
  ;; original multipliers/coefficients.
  [((cons '+ (app (split-pair (match-lambda*
                                ;; Find a pair of products, or standalone units
                                ;; (which are implicitly multiplied by one)
                                [(list
                                   (or `(× . ,xs) (? unit-ga? (app list xs)))
                                   (or `(× . ,ys) (? unit-ga? (app list ys))))
                                 ;; which do not contain sums or products
                                 (and (not (findf (disjoin geo-+? geo-×?)
                                                  (append xs ys)))
                                      ;; and have the same GA units
                                      (equal? (filter unit-ga? xs)
                                              (filter unit-ga? ys)))]
                                [_ #f]))
                  (list
                    pre
                    ;; Match the products. If either is a standalone GA unit,
                    ;; wrap it in a list when matching (for consistency)
                    (or (cons '× xs) (? unit-ga? (app list xs)))
                    (or (cons '× ys) (? unit-ga? (app list ys)))
                    suf))))
   (let ([units   (filter unit-ga? xs)]  ;; xs and ys have the same units
         ;; Multiply all the coefficients in xs together into a single value
         ;; (this may be 1, if there were no coefficients!); and same for ys.
         [x-coeff (apply × (filter (negate unit-ga?) xs))]
         [y-coeff (apply × (filter (negate unit-ga?) ys))])
     (apply geo-+ (append
       pre
       ;; Multiply units by the sum of both coefficients
       (list (apply geo-× (cons (+ x-coeff y-coeff) units)))
       suf
     )))]
```

The following two rules match a product-of-sums, and turn it 'inside out' into
the sum-of-products we need. The first rule multiplies the elements of a sum by
a term appearing on its left, e.g. `(× a b (+ c d) e f)` becomes
`(× a (+ (× b c) (× b d)) e f)`. The second multiplies the elements of a sum by
a term appearing on its right, e.g. `(× a b (+ c d) e f)` becomes
`(× a b (+ (× c e) (× d e)) f)`. Notice that these rules preserve the order that
terms are multiplied, which is required when we have anti-commutativity.

These rules will keep matching as long as a sum is being multiplied, until
eventually nothing remains on its left/right, e.g. our example will become
`(× (+ (× a b c e f) (× a b d e f)))`. Since that's a product of one element,
the outer `(× ...)` wrapper will be discarded (thanks to a previous rule),
resulting in a sum-of-products, which is what we need!

```{.scheme pipe="./show"}
  ;; Distribute a product on the left, turning (× ... a1 a2 (+ b1 b2 ...) c ...)
  ;; into (× ... a1 (+ (× a2 b1) (× a2 b2) ...) c ...)
  [((cons '× (app (split-pair (lambda (x y) (geo-+? y)))
                  (list pre x (cons '+ ys) suf))))
   (apply geo-× (append
     pre
     (list (apply geo-+ (map (curry geo-× x) ys)))
     suf
   ))]

  ;; Distribute a product on the right turning (× ... a (+ b1 b2 ...) c1 c2 ...)
  ;; into (× ... a (+ (× b1 c1) (× b2 c1) ...) c2 ...)
  [((cons '× (app (split-pair (lambda (x y) (geo-+? x)))
                  (list pre (cons '+ xs) y suf))))
   (apply geo-× (append
     pre
     (list (apply geo-+ (map (curryr geo-× y) xs)))
     suf
   ))]
```

We've now run out of simplification rules, and move on to arranging the elements
of sums and products into alphabetical order. We do this by swapping any
neighbouring elements which appear in the wrong order, which acts like a Bubble
Sort algorithm. This is notorious for needing O(n²) comparisons in the worst
case, but is acceptable here since (a) our lists are usually sorted, which gives
optimal O(n) performance, and (b) it has low overhead, which can dominate the
small-list regime we're in.

```{.scheme pipe="./show"}
  ;; Arrange a sum into alphabetical order. Addition is commutative, so we can
  ;; swap elements without affecting the result.
  [((cons '+ (app (split-pair
                    (match-lambda*
                      ;; Non-unit/product/sum values come first
                      [(list (? geo?) (not (? geo?))) #t]

                      ;; GA units and products come next (a standalone unit is
                      ;; equivalent to a product of that unit and 1)
                      [(list (? geo-+?) (? (disjoin geo-×? unit-ga?))) #t]

                      ;; Products with fewer elements (or standalone GA units)
                      ;; should appear earlier; those with the same length
                      ;; should be compared by their list of GA units
                      [(list (or `(× . ,xs) (? unit-ga? (app list xs)))
                             (or `(× . ,ys) (? unit-ga? (app list ys))))
                       (list<? (filter unit-ga? ys)
                               (filter unit-ga? xs)
                               unit<?)]

                      ;; Leave other combinations as-is, e.g. nested
                      ;; sums should get unwrapped by other rules
                      [_ #f]))
                  (list pre x y suf))))
   (apply geo-+ (append pre (list y x) suf))]  ;; Swap x & y
```

When rearranging a product we need to introduce a `-1` when swapping GA units
(since they anti-commute):

```{.scheme pipe="./show"}
  ;; Swap a product of GA units in the wrong order, and negate
  [((cons '× (app (split-pair (match-lambda*
                                [(list (? unit-ga? x) (? unit-ga? y))
                                 (unit<? y x)]
                                [_ #f]))
                  (list pre x y suf))))
   (canonical (append '(× -1) pre (list y x) suf))]

  ;; At this point products should contain no nested sums or products, only GA
  ;; units and non-geo? coefficients. Move the latter to the front.
  [((cons '× (app (split-pair (lambda (x y) (and (geo? x) (not (geo? y)))))
                  (list pre x y suf))))
   (apply geo-× (append (list y) pre (cons x suf)))]
```

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

Our final patterns match the canonical forms that we want, and return them
unchanged (this way, we'll get a match error if we've forgotten to handle some
case).

```{.scheme pipe="./show"}
  ;; A product of GA units, preceded by an optional non-geo?, is
  ;; canonical
  [((list '×
          (? (disjoin unit-ga? (conjoin geometric? (negate geo?))))
          (? unit-ga?) ...))
   n]

  ;; A sum of one optional non-geo?, followed by GA units and products, is
  ;; canonical
  [((list '+
          (? geometric?)
          (? (disjoin unit-ga? geo-×?)) ...))
   n]
)
```

```{pipe="./hide"}
(module+ test
  ;; Generate a list, using gen:elem to generate its elements. The resulting
  ;; generator uses its size parameter (provided by RackCheck) as a sort of
  ;; "fuel": a portion of this fuel is spent on generating the first element,
  ;; and the rest passed on to a recursive call for the tail; an empty list is
  ;; generated when we run out of fuel. This is useful to avoid generating the
  ;; exponentially-large or exponentially-small lists that naive recursion would
  ;; produce (resulting in out-of-memory, or uninteresting tests)
  (define (gen:sized-list gen:elem)
    (gen:sized (lambda (fuel)
      (if (< fuel 2)
        (gen:const '())
        (gen:bind (gen:integer-in 1 (- fuel 1)) (lambda (cost)
          (gen:let
            ([head (gen:resize gen:elem cost)]
             [tail (gen:resize (gen:sized-list gen:elem) (- fuel cost 1))])
            (cons head tail))))))))

  ;; Generates arbitrary, possibly non-canonical, products
  (define gen:product
    (gen:map (gen:delay (gen:sized-list gen:geo)) (curry cons '×)))

  ;; Generates arbitrary, possibly non-canonical, sums
  (define gen:sum
    (gen:map (gen:delay (gen:sized-list gen:geo)) (curry cons '+)))

  ;; Generates arbitrary, possible non-canonical, geometric numbers
  (define gen:geo
    (gen:sized (lambda (fuel)
      ;; GA units & rationals are the leaves of our expression trees, so
      ;; generate them more often than products & sums (the nodes)
      (gen:frequency `((1 . ,gen:product )
                       (1 . ,gen:sum     )
                       (5 . ,gen:unit-ga )
                       (5 . ,gen:rational))))))

  ;; Generates canonical geometric numbers
  (define gen:geometric (gen:map gen:geo canonical))

  (check-property
    (property preserve-non-geometric-behaviour ([x gen:rational]
                                                [y gen:rational])
      (test-equal? "geo-× emulates constant ×" (geo-×)     (×))
      (test-equal? "geo-× emulates unary ×"    (geo-× x)   (× x))
      (test-equal? "geo-× emulates binary ×"   (geo-× x y) (× x y))
      (test-equal? "geo-+ emulates constant +" (geo-+)     (+))
      (test-equal? "geo-+ emulates unary +"    (geo-+ x)   (+ x))
      (test-equal? "geo-+ emulates binary +"   (geo-+ x y) (+ x y))
      (test-equal? "canonical rational is itself" (canonical x) x)))

  (check-property
    (property canonical-correctly-handles-units ([x gen:unit-ga]
                                                 [y gen:unit-ga]
                                                 [i gen:natural])
      (test-equal? "Dual units square"       (geo-× `(d . ,i) `(d . ,i))  0)
      (test-equal? "Hyperbolic units square" (geo-× `(h . ,i) `(h . ,i))  1)
      (test-equal? "Imaginary units square"  (geo-× `(i . ,i) `(i . ,i)) -1)
      (when (not (equal? x y))
        (test-equal? "× anticommutes for units" (geo-× x y) (geo-× -1 y x)))
      ))

  (check-property
    (property single-non-canonical-number ([x gen:geo])
      (test-pred "Non-canonical numbers are still geometric?" geometric? x)
      (test-equal? "Left product with 0 is 0"  (geo-× 0 x) 0)
      (test-equal? "Right product with 0 is 0" (geo-× x 0) 0)
      (test-equal? "Sum with self is doubling" (geo-× 2 x) (geo-+ x x))))

  (check-property
    (property single-canonical-number ([x gen:geometric])
      (test-pred "Canonical numbers are geometric?" geometric? x)
      (test-equal? "Canonical is idempotent"   x (canonical x))
      (test-equal? "1 is left identity of ×"   x (geo-× 1 x))
      (test-equal? "1 is right identity of ×"  x (geo-× x 1))
      (test-equal? "0 is left identity of +"   x (geo-+ 0 x))
      (test-equal? "0 is right identity of +"  x (geo-+ x 0))))

  (check-property
    (property two-non-canonical-numbers ([x gen:geo] [y gen:geo])
      (test-equal? "+ commutes" (geo-+ x y) (geo-+ y x))))

  (check-property
    (property three-non-canonical-numbers ([x gen:geo] [y gen:geo] [z gen:geo])
      (test-equal? "+ associates" (geo-+ (geo-+ x y) z) (geo-+ x (geo-+ y z)))
      (test-equal? "× associates" (geo-× (geo-× x y) z) (geo-× x (geo-× y z)))
      (test-equal? "× left-distributes over +" (geo-+ (geo-× x y) (geo-× x z))
                                               (geo-× x (geo-+ y z)))
      (test-equal? "× right-distributes over +" (geo-+ (geo-× x z) (geo-× y z))
                                                (geo-× (geo-+ x y) z))))
)
```

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

<details class="odd">
 <summary>Test results</summary>

The generated code includes unit/property tests defined using `rackunit` and
`rackcheck`. They are executed when this page is rendered to HTML: failure will
abort the rendering process; when successful the output is written here:

```{pipe="sh"}
raco test geo.rkt || {
  echo "BEGIN geo.rkt"
  cat -n geo.rkt
  echo "END geo.rkt"
  false
} 1>&2
```

</details>
