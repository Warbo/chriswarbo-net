---
title: Complex And Hypercomplex Numbers
---

> You created this dream out of bits and pieces
> Filed away in your mind
> You're caught inside a fantasy
> But we'll find the truth inside
— <cite>Star One, *Cassandra Complex*</cite>

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
