---
title: "Ivory: Geometric Algebra"
packages: ['racketWithRackCheck']
---

```{pipe="sh"}
bash "$setup" geo.rkt
for DEP in "$geometric_units" "$numbers_in_scheme" "$sums_and_products" \
           "$zero_one_many"
do
  ./extract "$DEP"
done
```

```{pipe="./hide"}
#lang racket
(require "geo-units.rkt")
(require "sums-and-products.rkt")
(module+ test (require rackunit rackcheck-lib))
```

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

### Reducing To Normal Form ###

Our GA implementation relies on the normal form mentioned above, with a few
extra conditions arising from our encoding as Racket values. We convert
`geometric` numbers into this form using a `normalise` function, which uses
pattern-matching to rearrange various non-normal structures; and recurses
until the standard sum-of-products form is reached.

```{pipe="./hide"}
;; Helper functions

(define geo? (disjoin unit-ga? ×? +?))

(define geometric? (disjoin number? geo?))

(define geo-zero? (conjoin number? zero?))

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
  (prop list<?-works ([x  gen:natural]
                      [xs (gen:list gen:natural)]
                      [ys (gen:list gen:natural)])
    (check-false (list<? xs xs          <) "Lists not list<? themselves")
    (check-true  (list<? xs (cons x xs) <) "list<? shorter longer")
    (check-false (list<? (cons x xs) xs <) "Not list<? longer shorter")
    (match* ((list<? xs ys <) (list<? ys xs <))
      [(#t #t) (check-false #t "Can't be list<? both ways round")]
      [(#f #f) (check-true  (equal? xs ys) "Neither list<? means equal?")]
      [((or (and #t (app (const* xs ys) lo hi))
            (and #f (app (const* ys xs) lo hi))) _)
       (check-true
         (list<? (cons x lo) (cons x hi) <)
         "Prepending equal elements maintains list<?")
       (check-true
         (list<? (append lo (shuffle (append xs ys)))
                 (append hi (shuffle (append xs ys)))
                 <)
         "Appending the same number of things maintains list<?")]))
)
```

```{.scheme pipe="./show"}
;; Converts the given number towards its normal form
(define (normalise normalise n) (match n
```

A `number` which isn't a product or sum is already in normal form:

```{.scheme pipe="./show"}
  [(? geometric? (not (? geo?))) n]
  [(? unit-ga?) n]
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

  ;; Recurse when elements of a sum or product aren't normal
  [((cons (and sym (or '× '+)) (app (split (lambda (x)
                                             (not (equal? x (normalise x)))))
                                    (list pre x suf))))
   (normalise (cons sym (append pre (list (normalise x)) suf)))]
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
   (normalise (append
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
   (normalise (append '(× -1) pre (list y x) suf))]

  ;; At this point products should contain no nested sums or products, only GA
  ;; units and non-geo? coefficients. Move the latter to the front.
  [((cons '× (app (split-pair (lambda (x y) (and (geo? x) (not (geo? y)))))
                  (list pre x y suf))))
   (apply geo-× (append (list y) pre (cons x suf)))]
```

Our final patterns match the normal forms that we want, and return them
unchanged (this way, we'll get a match error if we've forgotten to handle some
case).

```{.scheme pipe="./show"}
  ;; A product of GA units, preceded by an optional non-geo?, is
  ;; normal
  [((list '×
          (? (disjoin unit-ga? (conjoin geometric? (negate geo?))))
          (? unit-ga?) ...))
   n]

  ;; A sum of one optional non-geo?, followed by GA units and products, is
  ;; normal
  [((list '+
          (? geometric?)
          (? (disjoin unit-ga? geo-×?)) ...))
   n]
)
```

```{pipe="./hide"}
(module+ test
  ;; Generates arbitrary, possibly non-normal, products
  (define gen:product
    (gen:map (gen:delay (gen:sized-list gen:geo)) (curry cons '×)))

  ;; Generates arbitrary, possibly non-normal, sums
  (define gen:sum
    (gen:map (gen:delay (gen:sized-list gen:geo)) (curry cons '+)))

  ;; Generates arbitrary, possible non-normal, geometric numbers
  (define gen:geo
    (gen:sized (lambda (fuel)
      ;; GA units & rationals are the leaves of our expression trees, so
      ;; generate them more often than products & sums (the nodes)
      (gen:frequency `((1 . ,gen:product )
                       (1 . ,gen:sum     )
                       (5 . ,gen:unit-ga )
                       (5 . ,gen:rational))))))

  ;; Generates normal geometric numbers
  (define gen:geometric (gen:map gen:geo normalise))

  (prop preserve-non-geometric-behaviour ([x gen:rational]
                                          [y gen:rational])
    (check-equal?  (geo-×)     (×) "geo-× emulates constant ×")
    (check-equal?     (geo-× x)   (× x) "geo-× emulates unary ×")
    (check-equal?    (geo-× x y) (× x y) "geo-× emulates binary ×")
    (check-equal?  (geo-+)     (+) "geo-+ emulates constant +")
    (check-equal?     (geo-+ x)   (+ x) "geo-+ emulates unary +")
    (check-equal?    (geo-+ x y) (+ x y) "geo-+ emulates binary +")
    (check-equal?  (normalise x) x "normal rational is itself"))

  (prop normalise-correctly-handles-units ([x gen:unit-ga]
                                           [y gen:unit-ga]
                                           [i gen:natural])
    (check-equal?        (geo-× `(d . ,i) `(d . ,i))  0 "Dual units square")
    (check-equal?  (geo-× `(h . ,i) `(h . ,i))  1 "Hyperbolic units square")
    (check-equal?   (geo-× `(i . ,i) `(i . ,i)) -1 "Imaginary units square")
    (when (not (equal? x y))
      (check-equal?  (geo-× x y) (geo-× -1 y x) "× anticommutes for units")))

  (prop single-non-normal-number ([x gen:geo])
    (check-pred geometric? x "Non-normal numbers are still geometric?")
    (check-equal? (geo-× 0 x) 0           "Left product with 0 is 0")
    (check-equal? (geo-× x 0) 0           "Right product with 0 is 0")
    (check-equal? (geo-× 2 x) (geo-+ x x) "Sum with self is doubling"))

  (prop single-normal-number ([x gen:geometric])
    (check-pred geometric? x "Normal numbers are geometric?")
    (check-equal? x (normalise x) "Normalise is idempotent")
    (check-equal? x (geo-× 1 x)   "1 is left  identity of ×")
    (check-equal? x (geo-× x 1)   "1 is right identity of ×")
    (check-equal? x (geo-+ 0 x)   "0 is left  identity of +")
    (check-equal? x (geo-+ x 0)   "0 is right identity of +"))

  (prop two-non-normal-numbers ([x gen:geo] [y gen:geo])
    (check-equal?  (geo-+ x y) (geo-+ y x) "+ commutes"))

  (check-property
    ;; Double the deadline since it often times-out after around 90/100 tests!
    (make-config #:deadline (+ (current-inexact-milliseconds) (* 120 1000)))
    (property three-non-normal-numbers ([x gen:geo] [y gen:geo] [z gen:geo])
      (check-equal? (geo-+ (geo-+ x y) z) (geo-+ x (geo-+ y z)) "+ associates")
      (check-equal? (geo-× (geo-× x y) z) (geo-× x (geo-× y z)) "× associates")
      (check-equal? (geo-+ (geo-× x y) (geo-× x z)) (geo-× x (geo-+ y z))
                    "× left-distributes over +")
      (check-equal? (geo-+ (geo-× x z) (geo-× y z)) (geo-× (geo-+ x y) z)
                    "× right-distributes over +")))
)
```

## Code ##

Here's a URI containing all of the Racket code generated by this post:

```{.unwrap pipe="./dump geo.rkt"}
```

<details class="odd">
 <summary>Test results</summary>

```{pipe="./tests"}
```

</details>
</div>
