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
SUB: ₀₁₂₃₄₅₆₇₈₉
SUP: ⁰¹²³⁴⁵⁶⁷⁸⁹ⁿ
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

<script type="text/javascript">
//<![CDATA[
 tableOfContents('#content', '#toc');
//]]>
</script>
