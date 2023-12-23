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

Ivory will represent sums and products as "uninterpreted function symbols", i.e.
as lists like `'(+ 1 2)`{.scheme} or `(× 3 4)`{.scheme}: our normalisation rules
will reduce such lists into their unique normal form.

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
our goal with Ivory is represent all numbers in a unique normal form, we will
choose one form for each law, and rewrite the other equal forms into this one.

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
they are nested; so long as the value occur in the same order from
left-to-right. This justifies the application of `+` and `×` to
*arbitrarily-many* inputs, which are equivalent to nested operations; but don't
require an arbitrary choice of branching structure.

#### Implementing Associativity ####

We will normalise such nested operations by flattening them. First, some helper
functions:

```{.scheme pipe="./show"}
;; Predicates for spotting products and sums
(define (×? n) (and (pair? n) (equal? '× (car n))))
(define (+? n) (and (pair? n) (equal? '+ (car n))))

;; Indicates whether a value was/wasn't in normal form, and hence may need
;; further normalising.
(define (unchanged x) (values x #f))
(define (  changed x) (values x #t))

;; Append an element to the end of a list
(define (snoc xs x) (append xs (list x)))

;; Return #f when no element of xs satisfies pred, otherwise return (list a b c)
;; where (append a (cons b c)) = xs, and (pred b)
(define ((split pred) xs)
  (match/values (splitf-at xs (negate pred))
    [(xs (cons y ys)) (list xs y ys)]
    [(_  _          ) #f]))
```

```{pipe="./hide"}
(module+ test
  (check-property
    (property split-works ([xs (gen:list gen:natural)])
      (match ((split even?) xs)
        [#f (test-equal? "No split means no matches" (filter even? xs) '())]
        [(list pre x suf)
          (test-equal? "Split should pick first match" (filter even? pre) '())
          (test-pred "Found value should match" even? x)
          (test-equal? "Split results should append to input"
            (append pre (cons x suf)) xs)]))))
```

Now we can spot nested sums/products and normalise them to a single sum/product:

```
;; Match nested operations; bind the inner operation's inputs to xs, and append
;; them to the outer operation's other inputs (pre and suf)
(define/match (normalise-associativity n)
  [(cons '+ (app (split +?) (list pre (cons '+ xs) suf)))
   (changed (cons '+ (append pre xs suf)))]

  [(cons '× (app (split ×?) (list pre (cons '× xs) suf)))
   (changed (cons '× (append pre xs suf)))]

  [_ (unchanged n)])
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

</figure>

So far all of these laws have applied equally to both sums and products. Not
only does distributivity apply differently to each, but it tells us which
operations act like sums, and which act like products.

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

```{.scheme pipe="./show"}
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

```{pipe="./hide"}
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
```

```{.scheme pipe="./show"}
;; Distributivity: if a product contains a sum, distribute everything on its
;; left and right into that sum. This will match over and over until the sum
;; is the only value inside the product.
(define (normalise-distributivity n)
  [(cons '× (app (split-pair (lambda (x y) (+? x)))  ;; A sum followed by y
                 (list pre (cons '+ xs) y suf)))
   ;; Keep the product
   (changed (cons '× (append
     pre
     ;; Multiply all of xs's summands by y on their right
     (list (cons '+ (map (lambda (x) (list '× x y)) xs)))
     suf
   )))]

  [(cons '× (app (split-pair (lambda (x y) (+? y)))  ;; x followed by a sum
                 (list pre x (cons '+ ys) suf)))
   ;; Keep the product, in case pre/suf contain more elements
   (changed (cons '× (append
     pre
     ;; Multiply all of ys's sumands by x
     (list (cons '+ (map (curry list '× x) ys)))
     suf
   )))]

  [_ (unchanged n)])
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


```{.scheme pipe="./show"}
;; Accept a normalising function as argument, and use that to recurse. That
;; allows more-sophisticated normalisation procedures to be passed in, like
;; defined in later pages!
(define (normalise normalise n) (match n
  [(app normalise-associativity  n #t) (changed n)]
  [(app normalise-identity       n #t) (changed n)]
  [(app normalise-distributivity n #t) (changed n)]

  ;; A singleton sum or product outputs its only input
  [(list '+ n) (changed n)]
  [(list '× n) (changed n)]

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

  ;; Recurse through the elements of a sum or product to see if any change
  [(cons (and op (or '+ '×)) xs)
   (for/fold ([result      (list op)]
              [any-changed #f])
             ([(x changed) (map normalise xs)])
     (values (snoc result x) (or any-changed changed)))]

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
