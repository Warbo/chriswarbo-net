---
title: Exploring rational arithmetic
packages: [ 'mathml' ]
---

I've recently been playing with the [Ruler]()/[Enumo]() project (I'm not
entirely sure about the naming… so I'll stick with Enumo). These enable theory
exploration, akin to systems like [QuickSpec]() and [IsaCoSy](), but in an
indirect way: rather than using some fixed procedure to explore definitions,
they break down the problem into separate parts, which can be combined into a
variety of procedures tailored to different use-cases.

This post documents some of my initial experiments, using this approach to
discover equations in a simple theory of rational numbers. It's also my first
time writing Rust, so the examples will be mostly straightforward; and the code
may be messy!

## Defining rational arithmetic ##

Rational numbers are fractions of the form
`{ var 'a'; var 'b'; }`{.unwrap pipe="sh | rat | math"}. We can represent these
in Rust as follows:

```rust
#[derive(Clone, Copy, Debug, Hash)]
pub struct Rat{ num: i32, den: u32 }
```

That `derive`{.rust} line is just a macro call, to generate trivial
implementations of a few useful interfaces. I want to allow negatives, but
allowing both parts to be negative would introduce redundancy, e.g.
`{ num '1' | neg; num '2' | neg; }`{.unwrap pipe="sh | rat | math minus"} is
equivalent to `{ num 1; num 2; }`{.unwrap pipe="sh | rat | math"}. Hence we
allow the `num`{.rust}erator to be negative (`i32`{.rust} is 32bit integers;
`u32`{.rust} is "unsigned" 32bit, AKA 32bit naturals). Here are some example
values of this type:

``` rust
const zero: Rat = Rat { num: 0, den: 1 };
const  one: Rat = Rat { num: 1, den: 1 };
```

This representation still has some redundancy, since it allows un-normalised
fractions like `{ num '2'; num '4'; }`{.unwrap pipe="sh | rat | math"}; and it
also allows invalid values like
`{ num '1'; num '0'; }`{.unwrap pipe="sh | rat | math"}. To prevent this, we'll
avoid constructing `Rat`{.rust} values directly, and instead use the following
`rat`{.rust} function:

```rust
fn rat(num: i64, den: u64) -> Option<Rat> {
    // Immediately reject division by zero
    if den == 0 {
        return None;
    }
    // Promote num and den to 128bit, since den won't fit in an i64
    let mut m = i128::from(num);
    let mut n = i128::from(den);
    // Find their greatest common divisor
    while m != 0 {
        let old_m = m;
        m = n % m;
        n = old_m;
    }
    let common = n.abs();
    // Reduce by their common divisor, and try to fit into 32 bits
    match (
        i32::try_from(i128::from(num) / common).ok(),
        u32::try_from(i128::from(den) / common).ok(),
    ) {
        (Some(norm_n), Some(norm_d)) => Some(Rat {num: norm_n, den: norm_d}),
        _ => None,
    }
}
```

Notice that this accepts 64bit inputs, despite our `Rat`{.rust} type being
32bit. That gives some extra room to allow intermediate results to grow, as long
as they're later factored back down to a 32bit range (they're also bumped to 128
bits temporarily, so that the signed and unsigned values fit into a common
representation!). We return `None`{.rust} if the normalised values don't fit in
the 32 bits provided by `Rat`{.rust}; or if the given `den`{.rust}ominator is
zero.

We can use this "smart constructor" to define `Eq`{.rust}uality and
`Ord`{.rust}ering, ensuring that we're always comparing normal forms:

```rust
impl PartialEq for Rat {
    fn eq(&self, other: &Self) -> bool {
        match (
            rat(i64::from(self.num), u64::from(self.den)),
            rat(i64::from(other.num), u64::from(other.den))
        ) {
            (Some(Rat{num: n1, den: d1}), Some(Rat{num: n2, den: d2})) =>
                n1 == n2 && d1 == d2,
            _ => false,
        }
    }
}

impl Eq for Rat {}

impl Ord for Rat {
    fn cmp(&self, other: &Self) -> Ordering {
        match (
            rat(i64::from( self.num), u64::from( self.den)),
            rat(i64::from(other.num), u64::from(other.den))
        ) {
            (Some(Rat { num: n1, den: d1 }), Some(Rat { num: n2, den: d2 })) =>
                (i64::from(n1) * i64::from(d2)).cmp(
                    &(i64::from(n2) * i64::from(d1))
                ),
            _ => cmp::Ordering::Equal,  // Absurd
        }
    }
}

impl PartialOrd for Rat {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
```

We can also implement `Neg`{.rust}ation relatively safely:

```rust
impl Neg for Rat {
        type Output = Self;

    fn neg(self) -> Self::Output {
        Rat{num: -(self.num), den: self.den}
    }
}

impl Neg for Rat {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Rat{num: -(self.num), den: self.den}
    }
}
```

However, due to `rat`{.rust} returning an `Option<Rat>`{.rust} it doesn't
quite fit Rust's standard `Add`{.rust} and `Mul`{.rust} traits; so we'll use
these standalone `add`{.rust} and `mul`{.rust} functions instead:

```rust
fn add(x: Rat, y: Rat) -> Option<Rat> {
    i64::from(x.num)
        .checked_mul(i64::from(y.den))
        .zip(i64::from(y.num).checked_mul(i64::from(x.den)))
        .and_then(|xy| xy.0.checked_add(xy.1))
        .zip(u64::from(x.den).checked_mul(u64::from(y.den)))
        .and_then(|xy| rat(xy.0, xy.1))
}

fn mul(x: Rat, y: Rat) -> Option<Rat> {
    i64::from(x.num)
        .checked_mul(i64::from(y.num))
        .zip(u64::from(x.den).checked_mul(u64::from(y.den)))
        .and_then(|xy| rat(xy.0, xy.1))
}
```

### The importance of `Display`{.rust} and `FromStr`{.rust} ###

The `Display`{.rust} trait says how to format (`fmt`{.rust}) a `Rat`{.rust} as a
`String`{.rust}, and the `FromStr`{.rust} trait says how to parse
(`from_str`{.rust}) a `String`{.rust} into a `Rat`{.rust}. These should be
inverses of each other, so `from_str`{.rust} parses the result of `fmt`{.rust}
back to a `Rat`{.rust} value that equal to what we started with.

These will be important for our exploration: `Display`{.rust} will show us the
results of what was found; and `FromStr`{.rust} will be used in the construction
of terms. The latter will be explained in more detail later, but here are the
implementations I used:

```rust
impl std::fmt::Display for Rat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", self.num, self.den)
    }
}

impl FromStr for Rat {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (x, y) = s.split_once('/').ok_or(
            "No / in Rat '".to_owned() + s + "'"
        )?;
        let n = x.parse::<i64>().map_err(
            |_| "Error parsing numerator of Rat '".to_owned() + s + "'"
        )?;
        let d = y.parse::<u64>().map_err(
            |_| "Error parsing denominator of Rat '".to_owned() + s + "'"
        )?;

        match rat(n, d) {
            None => Err("Parse error constructing Rat '".to_owned() + s + "'"),
            Some(r) => Ok(r)
        }
    }
}
```

### `RatLang`{.rust}: a domain-specific language for rational numbers ###

Enumo operate on *languages*, of two sorts: first, a generic language of nested
`String`{.rust} values is generated, which I'll refer to as [s-expressions]()
for simplicity. Those generated s-expressions are then *parsed* into expressions
of a domain-specific language we define, via the `egg::define_language!`{.rust}
macro; like this `RatLang`{.rust} for rational number arithmetic:

```rust
egg::define_language! {
  pub enum RatLang {
    "~" = Neg(Id),
    "#" = Avg(Id),
    "+" = Add([Id; 2]),
    "*" = Mul([Id; 2]),
    Lit(Rat),
    Var(egg::Symbol),
  }
}
```

This contains quite a lot of information, which deserves some unpacking. Firstly
it defines a simple datatype with six constructors: `Neg`{.rust}, `Avg`{.rust},
`Add`{.rust}, `Mul`{.rust}, `Lit`{.rust} and `Var`{.rust}; with `Add`{.rust} and
`Mul`{.rust} taking two arguments, and the others taking one. The first four
constructors are preceded by a `String`{.rust}; this is optional, and specifies
the s-expressions that parse to that constructor. For example, the `Add`{.rust}
line tells us that an s-expression like `("+" a b)` will parse to `Add(A,
B)`{.rust}; assuming that the s-expressions `a`{.rust} and `b`{.rust} parse to
the `Id`{.rust} values `A`{.rust} and `B`{.rust}, respectively.

Those constructors which *aren't* preceded by a `String`{.rust}
(i.e. `Lit`{.rust} and `Var`{.rust}) will be parsed using their argument's
`FromStr`{.rust} implementation. Hence any s-expression which can be parsed as a
`Rat`{.rust} (e.g. the lone `String`{.rust} `"1/2"`{.rust}) will be parsed as a
`Lit`{.rust} applied to that `Rat`{.rust}; and any `String`{.rust} which
*doesn't* parse as a `Rat`{.rust} will become a `Var`{.rust} (since the parser
for `egg::Symbol`{.rust} accepts any `String`{.rust}). Note that the parser for
`RatLang`{.rust} will try each constructor in order, from top to bottom; so
`Var`{.rust} *must* come after `Lit`{.rust}, to prevent it matching every lone
`String`{.rust} (even those like `"1/2"`{.rust}!).

The final thing to mention is the `Id`{.rust} type: this represents a
sub-expression of our domain-specific language. Hence the argument of
`Neg`{.rust} is a `RatLang`{.rust} expression;; `Mul`{.rust} takes two
`RatLang`{.rust} expressions; etc. The reason it's an `Id`{.rust}entifier,
rather than some syntax tree structure, is that our `RatLang`{.rust} expressions
will be represented using [equation graphs](). This way, a single identifier can
reference *many* equivalent expressions; for example, let's assume the following
equations hold (we'll see how to discover equations later):

 1. `Add(x, y) == Add(y, x)`{.rust}
 2. `Add(x, Lit(Rat::zero)) == x`{.rust}
 3. `Neg(Lit(Rat::zero)) == Lit(Rat::zero)`{.rust}

A traditional syntax tree like `NEG(ADD(LIT(Rat::one), LIT(Rat::one)))`{.rust}
is limited to only representing one particular expression, and can't make use of
those equations. In contrast, the corresponding `RatLang`{.rust} equation graph
would look something like the following (in pseudocode):

``` rust
{
  Id0: [Neg(Id1), Add(Id0, Id2), Add(Id2, Id0)],
  Id1: [Add(Id3, Id3), Add(Id1, Id2), Add(Id2, Id1)],
  Id2: [Lit(Rat::zero), Neg(Id2), Add(Id2, Id2)],
  Id3: [Lit(Rat::one), Add(Id3, Id2), Add(Id2, Id3)]
}.Id0
```

Here each `Id`{.rust} refers to an *equivalence class* of expressions; the
sub-expressions of each constructor refer to *entire classes*, rather than
individual tree structures. For example, we can see that the `Id2`{.rust} class
contains `Lit(Rat::zero)`{.rust}, so the elements of that class are just
different encodings of zero. For example, the element `Neg(Id2)`{.rust} is in
this class due to equation 3, where the sub-expression `Lit(Rat::zero)`{.rust}
has been generalised to reference the *entire class* of zero encodings; and
hence represents an *unlimited number* of equivalent expressions, including
`Neg(Rat::zero)`{.rust}, `Neg(Neg(Rat::zero))`{.rust}, and so on. Likewise, the
other element `Add(Id2, Id2)`{.rust} generalises equation 2, to represent
*another* unlimited family of expressions `Add(Rat::zero, Rat::zero)`{.rust},
`Add(Rat::zero, Add(Rat::zero, Rat::zero))`{.rust},
`Add(Add(Rat::zero, Rat::zero), Rat::zero)`, etc. Taking this class as a whole,
we also get all of their combinations, like
`Neg(Add(Rat::zero, Neg(Rat::zero)))`{.rust}.

This encoding trick is used in all of the equivalence classes, such that the
overall result `Id0`{.rust} represents not just the expression
`Neg(Add(Lit(Rat::one), Lit(Rat::one)))`{.rust}, but also the result of applying
any combination of those equations to any part of it.

 - `Lit`{.rust} takes a `Rat`{.rust} as argument. We will use `Lit`{.rust} to represent literal
   numbers in our language, .
 - `Var`{.rust} takes a `Symbol`{.rust} as argument. `Symbol`{.rust} is an opaque value which can be
   compared for equality, i.e. we can check whether two `Symbol`{.rust} values are the
   same or different. We will use this to represent free variables in our
   language.
 - `Neg`{.rust} and `Avg`{.rust}  taking a single `Id`{.rust}entifier as argument

  and two binary constructors (`Add`{.rust} and `Mul`{.rust}, each
taking two `Id`{.rust}entifiers).
