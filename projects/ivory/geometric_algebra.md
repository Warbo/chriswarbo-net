---
title: "A New Numeric Tower: Geometric Algebra"
---

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
    ;; Double the deadline since it often times-out after around 90/100 tests!
    (make-config #:deadline (+ (current-inexact-milliseconds) (* 120 1000)))
    (property three-non-canonical-numbers ([x gen:geo] [y gen:geo] [z gen:geo])
      (test-equal? "+ associates" (geo-+ (geo-+ x y) z) (geo-+ x (geo-+ y z)))
      (test-equal? "× associates" (geo-× (geo-× x y) z) (geo-× x (geo-× y z)))
      (test-equal? "× left-distributes over +" (geo-+ (geo-× x y) (geo-× x z))
                                               (geo-× x (geo-+ y z)))
      (test-equal? "× right-distributes over +" (geo-+ (geo-× x z) (geo-× y z))
                                                (geo-× (geo-+ x y) z))))
)
```

## Code ##

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
</div>
