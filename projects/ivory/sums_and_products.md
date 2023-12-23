---
title: "Ivory: Sums And Products"
packages: ['racketWithRackCheck']
---

```{pipe="sh > /dev/null"}
bash $setup sums-and-products.rkt
for DEP in "$numbers_in_scheme"
do
  ./extract "$DEP"
done
```

```{pipe="./hide"}
#lang racket
(provide
  +?
  ×?
  ivory-+
  ivory-×
  normalise)
(require (rename-in "num.rkt" [normalise old:normalise]))
(module+ test
  (require rackunit rackcheck-lib (submod "num.rkt" test)))
```

We tend to learn about sums ("adding numbers") quite early, either when starting
school or even before. We also learn about products ("times-ing numbers")
shortly after. We often represent each using a table, to show their
patterns. For example:

<figure>

```
┏━━━┳━━━┯━━━┯━━━┯━━━┯━━━┓
┃ + ┃ 2̅ │ 1̅ │ 0 │ 1 │ 2 ┃
┣━━━╋━━━┿━━━┿━━━┿━━━┿━━━┫
┃ 2̅ ┃ 4̅ │ 3̅ │ 2̅ │ 1̅ │ 0 ┃
┠───╂───┼───┼───┼───┼───┨
┃ 1̅ ┃ 3̅ │ 2̅ │ 1̅ │ 0 │ 1 ┃
┠───╂───┼───┼───┼───┼───┨
┃ 0 ┃ 2̅ │ 1̅ │ 0 │ 1 │ 2 ┃
┠───╂───┼───┼───┼───┼───┨
┃ 1 ┃ 1̅ │ 0 │ 1 │ 2 │ 3 ┃
┠───╂───┼───┼───┼───┼───┨
┃ 2 ┃ 0 │ 1 │ 2 │ 3 │ 4 ┃
┗━━━┻━━━┷━━━┷━━━┷━━━┷━━━┛
```

 <figcaption>Addition table for integers</figcaption>
</figure>

<figure>

```
┏━━━┳━━━┯━━━┯━━━┯━━━┯━━━┓
┃ × ┃ 2̅ │ 1̅ │ 0 │ 1 │ 2 ┃
┣━━━╋━━━┿━━━┿━━━┿━━━┿━━━┫
┃ 2̅ ┃ 4 │ 2 │ 0 │ 2̅ │ 4̅ ┃
┠───╂───┼───┼───┼───┼───┨
┃ 1̅ ┃ 2 │ 1 │ 0 │ 1̅ │ 2̅ ┃
┠───╂───┼───┼───┼───┼───┨
┃ 0 ┃ 0 │ 0 │ 0 │ 0 │ 0 ┃
┠───╂───┼───┼───┼───┼───┨
┃ 1 ┃ 2̅ │ 1̅ │ 0 │ 1 │ 2 ┃
┠───╂───┼───┼───┼───┼───┨
┃ 2 ┃ 4̅ │ 2̅ │ 0 │ 2 │ 4 ┃
┗━━━┻━━━┷━━━┷━━━┷━━━┷━━━┛
```

 <figcaption>Multiplication table for integers</figcaption>
</figure>

We can generalise these ideas of sum and product to much more than simple
numbers. To do so we have to decide which aspects to keep, and which to ignore.
In other words, what is the "essence" that makes the above tabulations a sum and
a product? Which things *cannot* be considered as sums or products? Whilst we're
at it, what's the actual *distinction* between a "sum" and a "product"?

### Representing Sums And Products in Scheme ###

```{pipe="cat > helpers"}
;; Predicates for spotting products and sums
(define (×? n) (and (pair? n) (equal? '× (car n))))
(define (+? n) (and (pair? n) (equal? '+ (car n))))

;; Indicates whether a value was/wasn't in normal form, and hence may need
;; further normalising.
(define (unchanged x) (values x #f))
(define (  changed x) (values x #t))
```

```{pipe="cat > split"}
;; Return #f when no element of xs  satisfies pred, otherwise return (list a b c)
;; where (append a (cons b c)) = xs, and (pred b)
(define ((split pred) xs)
  (match/values (splitf-at xs (negate pred))
    [(xs (cons y ys)) (list xs y ys)]
    [(_  _          ) #f]))
```

```{pipe="cat > split-pair"}
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
```

```{pipe="sh"}
# We define these helpers up here, so they're outside the definition of
# normalise. We want to show them further down, so write them to separate files
# as well
./hide < helpers
./hide < split
./hide < split-pair
```

```{pipe="./hide"}
;; Append an element to the end of a list
(define (snoc xs x) (append xs (list x)))

(module+ test
  (define (gen:cons gen:x gen:y gen:empty)
    (gen:sized (lambda (fuel)
      (if (< fuel 2)
        gen:empty
        (gen:bind (gen:integer-in 1 (- fuel 1)) (lambda (cost)
          (gen:let ([x (gen:resize gen:x cost)]
                    [y (gen:resize gen:y (- fuel cost 1))])
            (cons x y))))))))

  (define (gen:sized-list gen:elem)
    (gen:cons gen:elem
              (gen:delay (gen:sized-list gen:elem))
              (gen:const '())))

  (prop split-works ([xs (gen:list gen:natural)])
    (match ((split even?) xs)
      [#f (check-equal?  (filter even? xs) '() "No split means no matches")]
      [(list pre x suf)
        (check-pred   even? x "Found value should match")
        (check-equal? (filter even? pre) '() "Split should pick first match")
        (check-equal? (append pre (cons x suf)) xs
          "Split results should append to input")]))

  (prop split-pair-works ([xs (gen:list gen:natural)])
    (match ((split-pair <) xs)
      [#f
       (check-equal? xs (reverse (sort xs <))
         "When no pairs are <, xs must be reverse sorted")]

      [(list pre x y suf)
       (check-equal?  `(,@pre ,x ,y ,@suf) xs "split-pair should split input")
       (check < x y "split-pair should find pair matching predicate")

       ;; For x & y to be the first pair where x < y, that means no elements
       ;; before y (i.e. in (snoc pre x)) should be greater than their
       ;; predecessor. Or, in other words, the elements of (snoc pre x) should
       ;; be in descending order. To test this, sorting should be a no-op.
       (check-equal? (snoc pre x) (sort (snoc pre x) >)
         "split-pair should return first pair matching predicate")]))
)
```

Ivory will represent sums and products as "uninterpreted function symbols", i.e.
as lists like `'(+ 1 2)`{.scheme} or `'(× 3 4)`{.scheme}. This page defines
the clauses of a normalisation procedure, to reduce such lists into a unique
normal form:

```{.scheme pipe="./show"}
;; Put sums and products into normal form. The first argument can be called to
;; normalise sub-expressions; the second argument can be used to compare/sort
;; expressions.
(define (normalise normalise ≤ n) (match n
```

## Types ##

We can think of addition and multiplication as *operations* or *functions* which
transform "input values" into an "output value". A fundamental aspect of these
values is their *type*: what sort of things are we talking about?

For the addition and multiplication we learn in school, all of the values are
*numbers*; i.e. they "have type `number`". Numbers can be "literal", like
`12`{.scheme}; or a (named) "variable" which *represents* some number, like
`x`{.scheme}; or, crucially, they can be the *output* of some *other* numerical
operation, like `(+ 1 2)`{.scheme}.

Having the same type for inputs and outputs allows sums and products to be
*nested* in arbitrary ways: with the output one one used as the input of
another, forming "trees":

<figure>

```
    +
  ┌─┴─┐
  │   │
  ×   3
┌─┴─┐
│   │
7   ×
  ┌─┴─┐
  │   │
  2   9
```

 <figcaption>A tree of nested sums and products, equivalent to the s-expression
 `(+ (× 7 (× 2 9)) 3)`{.scheme}, or the infix expression $(7 × (2 × 9)) + 3$.
</figure>

This is a crucial aspect of sums and products. An operation which *doesn't*
output the same type as its inputs, or which combines inputs of different types,
is not usually considered a sum or a product.

## Laws of Algebra ##

We can characterise operations *algebraically* by stating "laws": equations
which an operation always satisfies, regardless of its inputs. This latter
aspect can be represented by using variables for inputs (we'll use the names
`a`, `b` and `c`), and stating the laws "for all" values of those variables.
(Programmers may also know these as
[invariants](https://en.wikipedia.org/wiki/Invariant_(mathematics)#Invariants_in_computer_science)
or
[properties](https://hypothesis.works/articles/what-is-property-based-testing/)).

Since these laws state that different forms of sums and products are equal, and
our goal with Ivory is to represent all numbers in a unique normal form, we will
choose one form for each law, and rewrite the other equal forms into that one.

### Associativity ###

<figure>

```
   +                   +                      +
┌──┴──┐             ┌──┴──┐             ┌─────┼─────┐
│     │             │     │             │     │     │
a     +      =      +     c      =      a     b     c
   ┌──┴──┐       ┌──┴──┐
   │     │       │     │
   b     c       a     b
```

---

```
   ×                   ×                      ×
┌──┴──┐             ┌──┴──┐             ┌─────┼─────┐
│     │             │     │             │     │     │
a     ×      =      ×     c      =      a     b     c
   ┌──┴──┐       ┌──┴──┐
   │     │       │     │
   b     c       a     b
```

<figcaption>The law of associativity for `+` and `×`. These equations always
hold, regardless of the values of `a`, `b` and `c`.
</figure>

This law states that sums-of-sums, or products-of-products, do not depend on how
they are nested; so long as the inputs occur in the same order from
left-to-right. This justifies the application of `+` and `×` to
*arbitrarily-many* inputs, which are equivalent to nested operations (but don't
require an arbitrary choice of branching structure).

#### Implementing Associativity ####

We will normalise such nested operations by flattening them, using the following
clauses of `normalise`{.scheme}:

```{.scheme pipe="./show"}
  ;; Match nested operations; bind the inner operation's inputs to xs, and
  ;; append them to the outer operation's other inputs (pre and suf)

  [(cons '+ (app (split +?) (list pre (cons '+ xs) suf)))
   (changed (cons '+ (append pre xs suf)))]

  [(cons '× (app (split ×?) (list pre (cons '× xs) suf)))
   (changed (cons '× (append pre xs suf)))]
```

These make use of a few trivial helper functions:

```{.scheme pipe="cat helpers"}
```

More interestingly, we use the `split` function to split up a list (above, we're
splitting up the inputs of a sum or product), by finding an element `y` that
satisfies a given predicate (above, the predicates are `+?`{.scheme} and `×?`,
to spot a nested sum or product). The result also includes all the elements
occuring before `y`{.scheme} (which we call `xs`{.scheme}) and all those
following `y`{.scheme} (which we call `ys`{.scheme}). If no such element is
found, we return `#f`{.scheme} (and hence the above clauses won't match):

```{pipe="cat split"}
```

### Identity ###

<figure>

```
+
│      =      a
a
```

---

```
×
│      =      a
a
```

<figcaption>
A sum or product containing only a single input, is equal to that input. Hence,
in the case of a single argument, both sum and product act as the
[identity function](https://en.wikipedia.org/wiki/Identity_function).
</figcaption>

</figure>

In mathematics, the word "identity" refers to something that leaves a value
unchanged. Here we consider two sorts of identity. Firstly, the *identity
function* (or operation) is that which outputs its input. Since a sum with only
a single input does not add anything to it, the output equals that input; and
hence that sum is an identity function. Likewise, a product with only a single
input does not multiply it by anything, so the output equals that input, and the
product too is an identity function. These facts allow us to "unwrap" sums or
products of a single value when normalising expressions.

The second sort of identity is a *value*, whose inclusion in the inputs of an
operation does not change the output. We know from associativity that nested
operations are equivalent to a single operation with all the same inputs; hence
an *empty* operation is the identity value, since it does not affect the inputs
(only the nesting). For example, `(+ a (+))`{.scheme} is (by associativity) the
same as `(+ a)`{.scheme}, and hence the same as `a`{.scheme} (since, as
described above, the sum of a single input leaves it unchanged). Here is the
same example shown graphically (with a half-line `╵` to indicate no inputs):

```
   +                +
┌──┴──┐      =      │      =      a
a     +             a
      ╵
```

Hence [`(+)`{.scheme}](https://en.wikipedia.org/wiki/Empty_sum) is the identity
value for sums, and a similar argument shows that
[`(×)`{.scheme}](https://en.wikipedia.org/wiki/Empty_product) is the identity
value for products.

Now, we know from primary school that adding `0` to a number *also* leaves it
unchanged; so zero must *also* be an identity for addition. In which case, what
happens if we add zero to empty sum, like `(+ 0 (+))`{.scheme}? We know that
adding `0` leaves a value unchanged, so the result is `(+)`{.scheme}; yet we
also showed that adding `(+)`{.scheme} leaves a value unchanged, so the result
is *also* `0`. This only makes sense if both of these are the same value! We can
use the same approach to find that `(×)`{.scheme} and `1`{.scheme} must be the
same value. Hence:

<figure>

```
+      =      0
╵
```

---

```
×      =      1
╵
```

<figcaption>
A *empty* sum or product (i.e. which has no inputs) is called the *identity
element* of that operation. These are equal to the values zero and one,
respectively.
</figcaption>

</figure>

#### Implementing Identity ####

Unwrapping a single-input sum or product is easy:

```{.scheme pipe="./show"}
  ;; A singleton sum or product outputs its only input
  [(list '+ n) (changed n)]
  [(list '× n) (changed n)]
```

Since the identity values can be written in two ways (`0`{.scheme} or
`(+)`{.scheme}; `1`{.scheme} or `(×)`{.scheme}) our normal form needs to choose
one for each. We'll use the empty operation, since that exposes structure that
other clauses can use, e.g. to allow the existing associativity clauses to
simplify multiplication by `1` and addition of `0`:

```{.scheme pipe="./show"}
  ;; Empty sums and products are their identity elements
  [0 (changed (list '+))]
  [1 (changed (list '×))]
```

### Distributivity ###

<figure>

```
   ×                    +
┌──┴──┐             ┌───┴───┐
│     │             │       │
a     +      =      ×       ×
   ┌──┴──┐       ┌──┴──┐ ┌──┴──┐
   │     │       │     │ │     │
   b     c       a     b a     c
```

---

```

      ×                 +
   ┌──┴──┐          ┌───┴───┐
   │     │          │       │
   +     c   =      ×       ×
┌──┴──┐          ┌──┴──┐ ┌──┴──┐
│     │          │     │ │     │
a     b          a     c b     c
```

<figcaption>
Distributivity relates addition and multiplication, with the latter
"distributing over" the former.
</figcaption>

</figure>

So far all of these laws have applied equally to both sums and products. Not
only does distributivity apply differently to each, but it tells us which
operations act like sums, and which act like products. Specifically, it says
that multiplying a sum, like `(× 2 (+ 3 7 19))`{.scheme}, is the same as
multiplying each element of that sum individually, like
`(+ (× 2 3) (× 2 7) (× 2 19))`{.scheme}. We say that multiplication "distributes
over" addition. The same is not true if we switch the role of `+` and `×` (you
may like to convince yourself of this by finding a counter-example)!

Notice that distributing a multiplication doesn't change its order: distributing
a multiplication "on the left", like `(× a ...)`{.scheme}, produces a sum of
products which *also* multiply on the left; and vice versa "on the right", like
`(× ... a)`{.scheme}. This isn't relevant for types like `integer`, since
their product also obeys the *commutativity law*, that
`(= (× a b) (× b a))`{.scheme}; however, we'll soon encounter numbers which
*don't* commute, so it's important that we don't swap things around by accident!

#### Absorption ####

<figure>

```
   ×                ×                ×
┌──┴──┐             │             ┌──┴──┐
│     │      =      +      =      │     │
a     +             ╵             +     a
      ╵             ╵             ╵
```

</figure>

Distributivity has an important relationship to our identity laws. When a
product contains a sum, distributivity says that all of the product's other
inputs can be distributed over the inputs of that sum. If that sum has *no*
inputs, the result will be an empty sum; we say that the empty sum has
[absorbed](https://en.wikipedia.org/wiki/Absorbing_element) the products.

We showed that an empty sum must equal `0`{.scheme}, so this "absorption"
behaviour tells us that products with zero occuring as an input will always
output zero (regardless of any other inputs).

#### Implementing Distributivity ####

Distributivity can help us normalise expressions, by replacing one of these
forms with the other. We'll choose the sum-of-products form to be normal, since
it's easy to convert *into* that form by distributing multiplications over sums;
going the other way, to produce a product-of-sums, would require
[factorising](https://en.wikipedia.org/wiki/Factorization), which is notoriously
slow!

We need to be careful not to change the order of any multiplications; to ensure
this, when a sum is multiplied by many values we'll only distribute those which
occur immediately to its the left or right; this can be repeated until all of
the values have been distributed into the sum. For example, given a product like
`(× a b (+ c d))`{.scheme} we will first distribute `b`, to get
`(× a (+ (× b c) (× b d)))`{.scheme}, then distribute `a` to get
`(+ (× a b c) (× a b d))`{.scheme}.

To find a sum and its immediate neighbour in a product, we use a modified
version of the above `split`{.scheme} function: `split-pair`{.scheme} will split
a list at a *pair of consecutive elements* which satisfy the given *binary*
predicate:

```{.scheme pipe="cat split-pair"}
```

We use `split-pair`{.scheme} to define two separate clauses. We first look
through the inputs of a product, for a neighbouring pair of the form
`(cons '+ xs)`{.scheme} (a sum with inputs `xs`{.scheme}) and any value
`y`{.scheme}. If found, we remove both from the product's inputs, and insert a
sum that's had the multiplication by `y`{.scheme} distributed across its inputs
(i.e. replacing each element `x`{.scheme} of `xs`{.scheme} with
`(× x y)`{.scheme}):

```{.scheme pipe="./show"}
  [(cons '× (app (split-pair (lambda (x y) (+? x)))  ;; A sum followed by y
                 (list pre (cons '+ xs) y suf)))
   ;; Keep the product, in case pre/suf contain more elements
   (changed (cons '× (append
     pre
     ;; Multiply all of xs's summands by y on their right
     (list (cons '+ (map (lambda (x) (list '× x y)) xs)))
     suf
   )))]
```

Next we do the same when the *second* value is a sum:

```{.scheme pipe="./show"}
  [(cons '× (app (split-pair (lambda (x y) (+? y)))  ;; x followed by a sum
                 (list pre x (cons '+ ys) suf)))
   ;; Keep the product, in case pre/suf contain more elements
   (changed (cons '× (append
     pre
     ;; Multiply all of ys's sumands by x
     (list (cons '+ (map (curry list '× x) ys)))
     suf
   )))]
```

Notice that we do not have to implement absorption separately, since the
identity clauses will replace `0`{.scheme} with `'(+)`, and the two clauses
above will cause it to absorb all of a product's inputs. In particular, their
results use `(map ... xs)`{.scheme}, which will return an empty list when
`xs`{.scheme} is empty!

### Commutativity ###

<figure>

```
   +                   +
┌──┴──┐             ┌──┴──┐
│     │             │     │
a     b      =      b     a
```

<figcaption>
Changing the order of the inputs to a commutative operation does not affect its
output.
</figcaption>

</figure>

Addition and multiplication of numbers are both *commutative*, meaning that the
order of their inputs does not affect their output. We will require sums to be
commutative, but we will not assume that products are; since that turns out to
be quite restrictive. We can use commutativity to normalise sums, by sorting
their inputs into a consistent order.

#### Implementing Commutativity ####

The following clause implements a form of Bubble Sort, by spotting a pair of
neighbouring inputs which are in the wrong order:

```{pipe="./show"}
  [(cons '+ (app (split-pair (lambda (x y) (not (lex≤ x y))))
                 (list pre x y suf)))
   (changed (append '(+) pre (list y x) suf))]
```

Note that we cannot sort the inputs based on their *numerical value*, since we
will encounter multi-dimensional values which don't have a linear ordering.
Instead we will compare the *literal syntax* of expressions, known as a
[lexicographical ordering](https://en.wikipedia.org/wiki/Lexicographic_order):

```{.scheme pipe="cat lessthan"}
```

## Final Pieces ##

Our `normalise` function needs a few more clauses to be complete. Firstly, if
we find a sum or product containing ordinary Racket `number`s, then we should
add/multiply them using Racket's usual addition/multiplication operations:

```{.scheme pipe="./show"}
  ;; Sums of multiple Racket numbers simplify via addition
  [(cons '+ (app (split number?)
                 (list pre x (app (split number?)
                                  (list mid y suf)))))
   (changed (append (list '+ (+ x y)) pre mid suf))]

  ;; Products of multiple Racket numbers simplify via multiplication
  [(cons '× (app (split number?)
                 (list pre x (app (split number?)
                                  (list mid y suf)))))
   (changed (append (list '× (× x y)) pre mid suf))]
```

If we have a sum or product which doesn't match *any* of the above clauses, we
should try normalising its inputs recursively:

```{.scheme pipe="./show"}
  ;; Recurse through the elements of a sum or product to see if any change
  [(cons (and op (or '+ '×)) xs)
   (for/fold ([result      (list op)]
              [any-changed #f])
             ([(x changed) (map normalise xs)])
     (values (snoc result x) (or any-changed changed)))]
```

Finally, a value which doesn't match any of the clauses defined on this page
should be left unchanged:

```{.scheme pipe="./show"}
  ;; Otherwise there's nothing left to do
  [_ (unchanged n)]))
```

## Code ##

Here's a URI containing all of the Racket code generated by this post:

```{.unwrap pipe="./dump sums-and-products.rkt"}
```

<details class="odd">
 <summary>Test results</summary>

```{pipe="./tests"}
```

</details>
</div>
