---
title: Turing machine tapes
---

I'm very interested in ideas related to computation, computability, constructive
logic, finite representations, and so on. Turing machines appear frequently in
these fields (due to their importance in the history of computation), but are
often presented in a problematic way.

## My preferred presentation ##

<figure>
```
┌────────────────┐
│ Turing machine │
└─────┐  ┌───────┘
      │  │<───────── Read/write head
   ╭┄┄┼┄┄┼┄┄╮
   ┆  └──┘  ┆<─── Tape
   ┆        ┆
   ┆        ┆
   ┆       ┌┴┐
   ┆       └─┘<─── Tape factories
  ┌┴┐
  └─┘
```
</figure>

### The machine ###

A Turing machine is an automaton. The behaviour of a particular Turing machine
is described by its "transition table": a fixed set of rules which specify the
action it will take in each situation. The machine sits on a "tape", similar to
an [audio cassette](https://en.wikipedia.org/wiki/Cassette_tape), with its
"read/write head" positioned over some part of that tape.

The situations described in the transition table include:

 - The content of the tape immediately below the head. For simplicity, this is
   usually assumed to be one of two possible values (e.g. 0 or 1).
 - Which "state" the machine is in. This is just a number, e.g. beginning at
   1 when the machine starts.

The actions specified in the table include:

 - What to write on the tape, directly below the head (again, usually simplified
   to being either 0 or 1)
 - Which direction to move along the tape: either one step to the left, or one
   step to the right. (We can either think of the machine moving along a static
   tape, or the tape moving through a static machine; it makes no difference)
 - Which state to put the machine in (again, just a number).

There can be any number of states, so long as the transition table contains one
row for each possible situation (e.g. if we're limiting the tape to 0 and 1,
then the table needs two rows for each state: specifying which action to take
when the tape contains 0, and when the tape contains 1).

Optionally, we can choose one of the states to be a "halt state", and stop the
machine if/when it enters that state.

### The tape ###

Before we start the machine, we get to decide what we want to put on the tape.
These days we'd think of the tape contents as a "program" for the Turing machine
to "execute" (especially if it's a
[universal machine](https://en.wikipedia.org/wiki/Universal_Turing_machine)); or
perhaps as "data" for it to "process".

Once we've made a tape with our desired contents, we attach a "tape factory" to
each end. Whenever the Turing machine gets close to either end of the tape, that
factory will extend the tape, to ensure the machine never "falls off". The extra
tape is usually filled with a particular value, e.g. 0 (sometimes referred to as
being "blank").

## The problem ##

The way I present Turing machines above is mostly standard, except in one
respect: most descriptions do not mention "tape factories", and instead say the
tape is "infinitely long". If you're happy with that description, then that's
fine and I won't argue against it.

However, I've noticed some people are *not* happy with the idea of an "infinite
tape", and this seemingly-minor quibble can become a major distraction; or
worse, a way to disregard whatever point is being made, or even the concept of
computability itself, as being built on shaky foundations.

I welcome discussion and argument in these fields, and even in their
foundations; but this *particular* dismissal annoys me, since its *irrelevant*!
If someone rejects the premise of a machine having "infinite memory", or taking
up "an infinite amount of space", or what-have-you, I don't *disagree*; I simply
*do not care*, because such premises are usually *not necessary*, and can be
replaced with a finitary premise instead.

As an analogy, consider the common popular idea that
[½ + ¼ + ⅛ + … = 1](https://en.wikipedia.org/wiki/1/2_%2B_1/4_%2B_1/8_%2B_1/16_%2B_%E2%8B%AF).
If someone wants to reject this, due to infinite sums never being "completed",
then I won't disagree. Yet I wouldn't consider that a good reason to reject the
concept of *the number 1*; since we can define 1 in all sorts of other ways,
including many that do not involve such infinite processes!

## The solution ##

I'm particularly fond of the "tape factory" as a way of avoiding such quabbles.
Most importantly, it is finitary: at any particular moment, our machine only has
a finite amount of tape/memory. It's reasonably intuitive, and turns complaints
about being "too big" into ones about "taking too long": which is good, since
those are *precisely* the questions that computability theory tends to focus on
(like the [halting problem](https://en.wikipedia.org/wiki/Halting_problem)).

Tape factories also have
<abbr title="debatable...">practical applications</abbr> in
<abbr title="dubious...">real life</abbr> Turing machines, like the
["fully universal" Turing machine](https://conwaylife.com/wiki/Fully_universal_Turing_machine)
in [Conway's Game of Life](https://conwaylife.com/wiki/Conway%27s_Game_of_Life).
