Originally from https://stackoverflow.com/questions/43830085/what-it-means-lambda-calculus-is-equivalent-to-turing-machine?rq=1

> What it actually means that lambda calculus is equivalent to turing machine,
> and where it actually manifest itself?

It means that:

1) We can construct a Turing machine which can reduce any lambda calculus
expression (encoded on the tape in some way)

2) We can construct a lambda calculus expression which can simulate any Turing
machine (encoded as an expression in some way)

For (1), we need to pick some way to encode lambda calculus expressions as
symbols on a Turing machine tape. We can write the symbols `λ`, `(`, `)`, `.`
and ` ` (space) directly; the only tricky part is variable names, since there
are infinitely many of them (`x`, `y`, ...). My favourite way to encode
variables is using [de Bruijn
indexes](https://en.wikipedia.org/wiki/De_Bruijn_index), where we don't write
any argument names, and we use numbers instead of variable names: a variable
number N refers to the argument of the Nth lambda we're "wrapped in". For
example `λx. x` becomes `λ. 0`, whilst `λx. (λy. (x (x y)))` becomes `λ. (λ. (1
(1 0)))`, etc. This encoding is tricky to understand, but the point is that we
can write any lambda calculus term with a small, fixed set of symbols: the
"syntax" symbols `λ() .` plus the digits `0123456789` (or `01` if you prefer
binary). We can write these symbols one after another on a Turing machine's tape
(perhaps followed by another symbol to indicate "the end" (AKA `EOF` or
end-of-file)).

The equivalence tells us that there is a Turing machine (a table of
state-transition rules) which, when run on such a tape, will either:

 - Halt, with the tape containing the normal form of the given lambda
   expression, iff the given expression has a normal form.

 - Loop forever, iff the given expression has no normal form.

The transition rules for such a machine would be very complicated, so I don't
dare try to write them down here! The point is that Turing machines can compute
everything that lambda calculus can compute, since Turing machines can evaluate
every lambda calculus expression; i.e. if I can compute something using lambda
calculus, then I can write that same lambda expression on to a Turing machine
tape and run the machine to compute the same result. Hence Turing machines are
*at least* as powerful as lambda calculus, in terms of what they can compute.

For (2), we need to pick some way to encode Turing machines as lambda calculus
expressions. There are systematic ways to encode data as lambda expressions,
e.g. [Church encoding](https://en.wikipedia.org/wiki/Church_encoding) and
[Morgensen-Scott
encoding](https://en.wikipedia.org/wiki/Mogensen%E2%80%93Scott_encoding). We can
tackle this in a few steps.

First we need a way to represent any Turing machine as a piece of data. We can
do this by storing a [tuple](https://en.wikipedia.org/wiki/Tuple) containing the
table of state-transition rules, the current state, the tape and the read/write
head position. Symbols and states can both be represented as [natural
("counting") numbers](https://en.wikipedia.org/wiki/Natural_number) and the
direction (left or right) is a
[boolean](https://en.wikipedia.org/wiki/Boolean_data_type).

We can actually represent the tape and read/write head position together, in an
elegant data structure called a ["list with a
zipper"](https://en.wikipedia.org/wiki/Zipper_(data_structure)), which is just a
tuple containing a single symbol and two [(singly-linked) lists of
symbols](https://en.wikipedia.org/wiki/Linked_list#Singly_linked_list). Let's
say we have a Turing machine tape with contents like `...abchxyz...`, which
extends forever in both directions, where the read/write head is currently at
the symbol 'h'. We can imagine the tape being draped over the read/write head,
like a chain over a pulley (here I've drawn the head/pulley using ●):

```
 h
c●x
b y
a z
. .
. .
. .
```

This is where the idea for our zipper comes from: the single symbol is
whatever's over the read/write head, `h` in this case; whilst the two lists are
the two 'strands' dangling from the read/write head, `[c, b, a, ...]` and `[x,
y, z, ...]` respectively. Notice that the left 'strand' is in reverse order, due
to the way it hangs off the read/write head. Hence this zipper would be a tuple
like `(h, [c, b, a, ...], [x, y, z, ...])`.

The reason this "zipper" is so elegant is that we can 'move' the whole
(infinite!) tape left and right by doing a tiny amount of computation: adjusting
only three values (`c`, `h` and `x` in this example. Note that, due to the way
singly-linked lists are constructed, the sections `[b, a, ...]` and `[y, z,
...]` are unchanged, so we can re-use the old values without having to perform
any computation, even if they're "infinite")

```
Moved left    Original   Moved right
     c           h            x
    b●h         c●x          h●y
    a x         b y          c z
    . y         a z          b .
    . z         . .          a .
    . .         . .          . .
    . .         . .          . .
```

We can treat the state-transition table as another singly-linked list, where
each element is a rule. A rule can be represented as a tuple containing the
state and symbol which trigger the rule, the symbol to write when triggered, the
state to transition to when triggered, and the direction to move the read/write
head when triggered. That's just four numbers and a boolean.

Hence to represent a whole Turing machine in lambda calculus, we need some way
to represent booleans, natural numbers, tuples and singly-linked lists.

Booleans are the simplest of these. We need one lambda term to represent "move
left" and one to represent "move right" (remember, these are just entries in our
rule table; they don't need to actually perform the tape-moving work, just
represent which direction to go!). Both lambda terms need the same
[interface](https://en.wikipedia.org/wiki/Application_programming_interface),
since whatever code ends up handling the rules will need to work for both 'left'
and 'right' terms. The simplest lambda terms which can distinguish between two
possibilities are the [Church
booleans](https://en.wikipedia.org/wiki/Church_encoding#Church_Booleans): these
both take two arguments, but each return a different one of them. Hence we can
encode "move left" as `λx. λy. x` and "move right" as `λx. λy. y`. If we're
given one of these, we can apply it to two arguments, and we'll get back the
first if we're meant to move left, and the second if we're meant to move
right. For example, let's say we have:

 - A lambda calculus expression `L`, which moves a given tape to the left (by
   manipulating the first few elements of the zipper, as visualised above)
 - Another expression `R`, which moves a given tape to the right (ditto)
 - An expression `T`, representing a tape (see below for what this might look
   like)
 - A Church boolean `D`, representing which direction to move

Now consider the expression `(((D L) R) T)`:

 - If `D` is the "move left" expression, this will be `((((λx. λy. x) L) R) T)`,
   which reduces to `(((λy. L) R) T)`, then to `(L T)`, which (according to our
   bullet point assumptions above) will produce a new tape, which is like `T`
   but moved to the left.
 - If `D` is the "move right" expression, this will instead be `((((λx. λy. y)
   L) R) T)`, which reduces to `(((λy. y) R) T)`, then to `(R T)`, which is like
   `T` but moved to the right.

So that's how we encode booleans. What about (natural) numbers (which we're
using for states and symbols)? These are more complicated, since there are
infinitely many of them, but it's not too hard. The usual way we represent
numbers in lambda calculus is using ["Church
numerals"](https://en.wikipedia.org/wiki/Church_encoding#Church_numerals). These
come from the idea that natural numbers are either "zero" or "one more than some
other natural number". We can represent this formally by using the symbol `Z` to
represent the number zero, and stating that the expression `(S n)` is a number
iff `n` is a number (the choice of symbols is arbitrary, but we traditionally
use `Z` for zero and `S` for
["successor"](https://en.wikipedia.org/wiki/Successor_function)). In this way we
can write the number 0 as `Z`, the number 1 as `(S Z)`, the number 2 as `(S (S
Z))`, and so on. These are called ["Peano
numbers"](https://wiki.haskell.org/Peano_numbers) (based on their use in [Peano
arithmetic](https://en.wikipedia.org/wiki/Peano_axioms)), or
["unary"](https://en.wikipedia.org/wiki/Unary_numeral_system) or ["tally
marks"](https://en.wikipedia.org/wiki/Tally_marks).

Church numerals represent these Peano numbers as lambda terms. We will represent
the number 0 (i.e. `Z`) as the term `λx. λy. y`; the number 1 (i.e. `(S Z)`) as
the term `λx. λy. (x y)`; the number 2 (i.e. `(S (S Z))`) as the term
`λx. λy. (x (x y))`; and so on. In every case we take two arguments (`x` and
`y`), and we apply the first argument (`x`) to the second argument (`y`) N times
to represent the number N.

Remember that our choice of variable name doesn't matter in lambda calculus:
`λx. x` and `λy. y` are "the same" (technically they're
["α-equivalent"](https://en.wikipedia.org/wiki/Lambda_calculus#Alpha_equivalence). Side
note: α-equivalent terms are identical when written using de Bruijn
indexes!). Hence we can change the traditional `x` and `y` argument names into
`s` and `z` and it means the same thing, but the connection to Peano numerals
should hopefully be clearer:

```
╭────────╥──────────────┬───────────────────╮
│ Number ║ Peano number │ Church numeral    │
╞════════╬══════════════╪═══════════════════╡
│      0 ║       Z      │ λs. λz.       z   │
│      1 ║    (S Z)     │ λs. λz.    (s z)  │
│      2 ║ (S (S Z))    │ λs. λz. (s (s z)) │
│    ... ║ ...          │ ...               │
└────────╨──────────────┴───────────────────┘
```

If we have some Church numeral `x`, we can get an equivalent expression via
[η-expansion](https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B7-reduction):
`λs. λz. ((x s) z)`. This expression just accepts the two arguments `s` and `z`,
and passes them on to `x`, so has no observable difference from `x`. If we
instead wrap the result of `x` in an extra application of `s`, we get a Church
numeral which is identical to 'the successor of `x`' (AKA 'one more than `x`'):
`λs. λz. (s ((x s) z))`. If we accept `x` as an argument, we get an encoding of
the `S` from our Peano numbers:

```
λx. λs. λz. (s ((x s) z))
```

We can apply `s` as many times as we like to get larger and larger
increments. (In contrast, [getting the predecessor is more
difficult!](https://en.wikipedia.org/wiki/Lambda_calculus#Arithmetic_in_lambda_calculus))

The way I like to think about Church numerals is that they're like Peano
numbers, but where `Z` and `S` are abstracted out using [dependency
injection](https://en.wikipedia.org/wiki/Dependency_injection). To "use" a
Church numeral, we just apply it to two expressions: one for `z` and one for
`s`. We can think of it like a loop, where the value we give for `z` is used as
the "initial value", and the value we give for `s` performs one "step" of the
loop. In fact, Church numerals are very similar to the [`times` method of Ruby
integers](https://ruby-doc.org/core-2.5.0/Integer.html#method-i-times).

As a simple example of how we might "use" such numbers, consider a simple
function EVEN? which calculates whether a given number is even (divisible by
2). We can write this as a loop: zero is even, so our "initial value" is 'true';
then for each "step" of the loop, we negate the previous value (i.e. boolean
NOT). We can already encode booleans as lambda terms (using Church booleans),
but how might we define a NOT function? Since Church booleans just pick between
their arguments, we can get the opposite result by swapping those arguments!

```
λb. λx. λy. ((b y) x)
```

Once it's been applied to a Church boolean (`b`), this function will take two
arguments (`x` and `y`) and return one of them; i.e. it will act like a Church
boolean. It decides which argument to return by passing them both into `b`, but
in the opposite order to how they were received, so this will behave in the
opposite way to `b`. Hence it's a NOT function for Church booleans.

We can use this NOT function to define our EVEN? function for Church numerals:
the looping is provided by the Church numeral itself, so we just need to supply
the initial value 'true' and the 'step' function NOT:

```
λn. ((n NOT) TRUE)

AKA

λn. ((n (λb. λx. λy. ((b y) x))) TRUE)

AKA

λn. ((n (λb. λx. λy. ((b y) x))) (λx. λy. x))
```

Now we know how to encode booleans and numbers as lambda terms; that only leaves
the 'container' types of tuples and lists (remember: zippers are just tuples
containing two lists and a value).

Tuples are pretty easy: given a pair of terms `A` and `B`, we can encode the
pair as `λp. ((p A) B)`. This takes a single argument, which I've called `p`,
and applies it to both terms `A` and `B`. For example, to encode a pair of
values 0 and 'false', we first encode each separately to get `λs. λz. z` and
`λx. λy. y`, then we put those terms into our pair "template" above, to get:

```
λp. ((p (λs. λz. z)) (λx. λy. y))
```

Side note: our encoding of 0 is
[equivalent](https://en.wikipedia.org/wiki/Lambda_calculus#Alpha_equivalence) to
our encoding of false!

Hopefully it's clear how to "use" such a pair: we apply it to an expression, and
that expression will be applied to both values (similar to ["argument unpacking"
in Python](https://www.python.org/dev/peps/pep-0448)).

This just leaves us with lists. Lists are actually [a lot like (natural)
numbers](/blog/2014-12-04-Nat_like_types.html), so we can encode them in a
similar way: as a "loop". The only difference is that each "step" of the loop
will also be given an "element" of the list, as well as the loop state (this
sort of loop is usually called [a "fold" or
"reduce"](https://en.wikipedia.org/wiki/Fold_(higher-order_function))). We set
this up in a similar way to Peano numbers by saying that every list is either
empty, represented symbolically as `nil` (like the `Z` of Peano numbers), or
consists of an element `x` prepended to another list `xs`, represented
symbolically as `(cons x xs)` (like the `(S n)` of Peano numbers). Hence a list
like `[1, 2, 3]` will be represented as `(cons 1 (cons 2 (cons 3 nil)))`.

Side note: this presentation of lists is at the foundation of the [Lisp
programming
language](https://en.wikipedia.org/wiki/Lisp_(programming_language)), whose name
comes from "LISt Processing", and was directly inspired by lambda calculus. Lisp
originated in the 1950s, although back then it [implemented variables
incorrectly](https://en.wikipedia.org/wiki/Scope_(computer_science)#Dynamic_scoping);
this was
[fixed](https://en.wikipedia.org/wiki/Scope_(computer_science)#Lexical_scoping)
in the [Scheme
language](https://en.wikipedia.org/wiki/Scheme_(programming_language)) in the
1970s.

Encoding `nil` in lambda calculus is identical to `Z` and 'false', except I'll
call the arguments `c` and `n` to represent "dependency injection" of `cons` and
`nil, respectively: `λc. λn. n`

We encode `(cons x xs)` in a similar way to `(S n)`, but we use an extra
application to provide `c` with our `x` element. Here are some simple lists and
their lambda calculus encoding:

```
╭───────────╥──────────────────────────┬───────────────────────────────────────────────────────────────╮
│ List      ║ Lisp notation            │ Lambda encoding                                               │
╞═══════════╬══════════════════════════╪═══════════════════════════════════════════════════════════════╡
│        [] ║                    nil   │ λc. λn.                                                   n   │
│       [0] ║         (cons 0    nil)  │ λc. λn.                                  ((c (λs. λz. z)) n)  │
│ [S, true] ║ (cons S (cons true nil)) │ λc. λn. ((c (λx. λs. λz. (s ((x s) z)))) ((c (λx. λy. x)) n)) │
│       ... ║ ...                      │ ...                                                           │
└───────────╨──────────────────────────┴───────────────────────────────────────────────────────────────┘
```

Lists encoded in this way act like ["fold" or "reduce"
functions](https://en.wikipedia.org/wiki/Fold_(higher-order_function)), [which
are widely used in many
languages](https://en.wikipedia.org/wiki/Fold_(higher-order_function)#In_various_languages),
so there's no shortage of examples of their usefulness.

Finally, we might wonder how a Turing machine's infinite tape can be
represented. This isn't actually a problem in lambda calculus, since our
encoding for lists will work perfectly well for infinite lists; we just write an
expression which will keep calling the `c` argument forever. For example, here's
an infinite list of zeros:

```
λc. λn. ((λx. ((c (λs. λz. z)) (x x))) (λx. ((c (λs. λz. z)) (x x))))
```

This is just the [Y
combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed-point_combinators_in_lambda_calculus)
applied to the expression `(c (λs. λz. z))` (i.e. `(c 0)`, or `Cons` applied to
the number zero). Since the Y combinator keeps applying an expression to itself
forever, the result is `((c 0) ((c 0) ((c 0) (...))))`, i.e. equivalent to
`(Cons 0 (Cons 0 (Cons 0 (...))))` forever. Note that we can actually simplify
this by getting rid of the `n` argument, since it will never appear in an
infinite list. The resulting datatype is known as a
["stream"](https://wiki.c2.com/?CoinductiveDataType), and it's slightly simpler
(but maybe less intuitive) to use a "stream with a zipper" to represent our
tape, instead of a "list with a zipper".

We've just seen that any Turing machine can be represent as a lambda calculus
expression. The equivalence between Turing machines and lambda calculus tells us
that there is a lambda calculus expression which, when applied to such an
encoding of a Turing machine (plus tape), will either:

 - Reduce to a normal form, iff that Turing machine halts; and the resulting
   expression will be an encoding of the Turing machine's final configuration
   (including state, head position and tape contents).
 - Keep reducing forever, without a normal form; iff that Turing machine doesn't
   halt.

The existence of such an "intepreter" expression shows that lambda calculus can
compute everything that Turing machines can compute, since lambda calculus can
simulate every Turing machine; i.e. if I can compute something using a Turing
machine, then I can encode that same Turing machine as a lambda expression and
apply the interpreter to it to compute the same result. Hence lambda calculus is
*at least* as powerful as Turing machines, in terms of what they can compute.

Since each of these (Turing machines and lambda calculus) is at least as
powerful as the other, it must be the case that neither is more powerful than
the other; and hence they're equally powerful in terms of what they can compute.

With this central issue made concrete, we can approach some of your other
questions (note that I've already touched on Lisp and its relation to lambda
calculus a little):

> I don't understand how lambda calculus could supersede turing machine as a
> theoretical model of computation.

Neither supercedes the other, but likewise neither is more fundamental. The only
reason to prefer one to the other is whether it makes some particular task
easier. For example, I find it much easier to read, write and think about
programs in lambda calculus (just look at how much I did it above; whilst I
couldn't bring myself to write a single transition rule for a Turing
machine!). On the other hand, it's much easier to keep track of a Turing
machine's time complexity, so they're useful for situations where that's more
important than understanding what the program is doing (e.g. in [Levin
search](http://www.scholarpedia.org/article/Universal_search)).

> [Lambda calculus] is more abstract, like a programming language of it's own,
> not the model of how to practically compute something, make things happen.

It depends! Both are just mathematical models, but you're asking about their
semantics/interpretation, but there are many ways to interpret them (AKA "assign
a semantics"). It's true that the most "obvious" interpretation of a Turing
machine is a collection of nuts and bolts, transistors and capacitors, reading
and writing a (magnetic?) tape; and the most "obvious" interpretation of lambda
calculus is via an interpreter program running on some electronic computer (my
hand-waved encoding of lambda calculus to a Turing machine tape is one of these;
there are loads of "real", runnable interpreters out there, e.g. [here's my
first Google hit](https://jacksongl.github.io/files/demo/lambda)). Both of these
are known as ["operational
semantics"](https://en.wikipedia.org/wiki/Operational_semantics), since they
ascribe "meaning" by asking "how does it make this machine behave?". Yet those
aren't their only interpretations!

In particular, my translation of Turing machines into lambda calculus
expressions gives a *different* semantics to Turing machines, namely: Turing
machines are lambda expressions, which can be reduced if a suitable
"interpreter" expression is applied to them. From this perspective, it's Turing
machines that are more abstract, since they're being "compiled down to" lambda
calculus!

Lambda calculus actually has *many* interpretations, and some of them are very
useful (not just theoretical toys, like the encodings between Turing machines
and lambda calculus I gave above). For example, let's say that we built a
physical Turing machine; we would presumably implement the state-machine as a
*digital circuit* (using transistors, capacitors, etc.). Well, one way of
interpreting what lambda calculus expressions are is [*digital
circuits*](http://conal.net/papers/compiling-to-categories) (to be clear: that's
not "circuits which evaluate lambda expressions", that's "circuits which *are*
lambda expressions", similar to hardware description languages like
[Verilog](https://en.wikipedia.org/wiki/Verilog)). In other words, even if we
made a physical Turing machine, we'd still be building it out of (physical)
lambda expressions! Note that this semantics isn't just a coincidence, it was
discovered in a systematic way (via category theory), so any alternative method
of controlling our Turing machine (e.g. [billiard
balls](https://en.wikipedia.org/wiki/Billiard-ball_computer), [gas
dynamics](http://lambda-the-ultimate.org/node/4120) or
[crabs](https://www.complex-systems.com/abstracts/v20_i02_a02)) would presumably
also turn out to be an implementation of lambda calculus.

On a purely personal, subjective note: since lambda calculus "turns up" in more
situations than Turing machines (e.g. in [cartesian closed
categories](https://en.wikipedia.org/wiki/Cartesian_closed_category) and
[natural
deduction](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence#Natural_deduction_and_lambda_calculus)),
I consider it to be more important and fundamental, mathematically and
physically; whilst Turing machines are more of a human invention, whose major
use is philosophical (e.g. as Turing originally used them, to argue that
universal computation encompasses all "effective methods" of
mathematics/logic). Whilst it's true that the resource usage of a Turing machine
is easier to track than a lambda calculus reduction, that doesn't mean Turing
machines are the best model we have. RAM machines are more representative of
"traditional" computers, whilst everyday consumer hardware is becoming
multi-processor and massively-parallel (e.g. with GPUs, and maybe soon TPUs);
someone's always trying to sell FPGA-enabled machines; plus resources like power
consumption are becoming more important, that aren't modelled too well by Turing
machines. On the "language" side we have the growing field of [cost
semantics](http://lambda-the-ultimate.org/node/5021) to figure our
