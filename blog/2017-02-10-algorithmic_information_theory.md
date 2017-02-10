---
title: Algorithmic Information Theory
---

The field of [algorithmic information theory](https://en.wikipedia.org/wiki/Algorithmic_information_theory)
studies the computational complexity of algorithms, where the resource being
considered is the *length of the program*. This is a good model of the
scientific process: we want a theory to *explain* our observations, and hence we
want a *program* which takes fewer bits to store than those observations, but
which will produce them as output when executed. Gregory Chaitin claims that
this compression *is* explanation: if a program is as long as its output, it
doesn't explain anything.

When I came across that claim I immediately thought of the case where some long
and tedious program is written to calculate a number, or even just a single bit
true/false answer to a statement which is full of arbitrary or "random"
parameters (i.e. difficult to compress). I'm thinking of examples like the
software Appel and Haken used in their original proof of the four colour
theorem.

I think there are a couple of ways to think about this:

The algorithmic complexity Chaitin is discussing assumes "logical omniscience",
i.e. when we read some logical statement/line of code (or add some bit to a
program/theory) we immediately know all of the consequences. In such a setting,
we never have to run a computer program to see what it produces: we know as soon
as we've read the code. An compiler with such omniscience could optimise a long
and tedious calculation by replacing it with the result, e.g. "4", and hence
prevent programs which are larger than their output. (Note that this isn't
actually possible in general, since we'd essentially be running the program at
compile time, and we'd run into the halting problem, turning our compiler into a
partial function).

In such cases the programs would actually be the same as their outputs, and
hence don't offer any "explanation" (compression), as Chaitin puts
it. Mathematicians might agree with this, since it's a common complaint that
having an automated theorem prover tell us "true"/"false" isn't considered very
useful, even if it's known to be correct (e.g. if we've verified the software).

This leads into the second perspective: that of ["boolean blindness"](https://existentialtype.wordpress.com/2011/03/15/boolean-blindness).
If we consider such programs from a Curry-Howard point of view, the output type
corresponds to a theorem of which the program is a proof. If all we're
outputting is a boolean value, then the corresponding theorem can be thought of
as "there exists a boolean". If we think of our program as a proof of this
statement, it's massively over-complicated; we could just write "true" or
"false" (or "0"/"1", etc.), and that would be just as good a proof that a
boolean exists.

The "blindness" is that trivial values like "true", "false", "0", "1", "2",
etc. tell us nothing about their intended meaning: the language/representation
makes no distinction between the value of "1 < 5" and the value of
"solve(fermatsLastTheorem)".

We can change our program's type, such that it encodes the problem we're trying
to solve, e.g. something like (in pseudo-Coq):

    forall (m : Map), exists(c : Colouring(m) | length(colours(c)) <= 4)

This forces our program to output a more complicated value than simple "true" or
"false" (if this were Coq, we'd have to construct a function from Maps to pairs,
where the first element of the pair is a colouring of the given map, and the
second element of the pair is a Natural number equal to the difference between
the number of colours used in that colouring and 4).

This brings us back into the realm of interesting, non-trivial comparisons
between program length and output length. There's also an interesting
distinction to be made between "proof objects" (programs which represent proofs
of our theorem) and programs for constructing such proof objects. To be
comparable with Chaitin, the language for constructing proofs should be
turing-complete (e.g. like Coq's "Ltac"); yet we might want to avoid having our
proof object's language be turing-complete, since that would permit unsound
proofs.

I think it's this distinction between turing-complete and non-turing-complete
languages that is the key to Chaitin's "explanations" (i.e. compression). An
"elegant" (shortest) program in the proof-constructing language cannot be longer
than an elegant proof-object, since we can just return that elegant proof object
verbatim: the program will be as long as its output, and we have "explained"
nothing, just like optimising a boolean program down to "true".

Yet an elegant proof-constructing program can be shorter than an elegant
proof-object! We can't make the proof-object language total by rejecting only
those programs which don't halt, since the halting problem is undecidable; our
criteria for accepting/rejecting must necessarily allow too many or too few
programs. If we allow too many, we're allowing invalid proofs, which we don't
want. We must choose to allow too few, and hence some perfectly valid proofs
will get rejected.

By having our proof-constructing language be turing-complete, we don't have to
reject any valid proofs, and hence we can write proof-constructing programs
which are not valid proof-objects. Some of those programs may be smaller than
the smallest proof-object, and hence they "explain" part of the
proof-object. For example, there may be some repetitive structure in the
proof-object which the proof-object language does not allow us to optimise
away. The proof-constructing language has no such restriction, allowing us to
write a smaller program which iteratively builds up the proof-object.
