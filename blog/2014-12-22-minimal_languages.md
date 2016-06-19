---
title: Minimalist Programming Languages
---
The [BrainFuck](http://en.wikipedia.org/wiki/Brainfuck) programming language is well-known amongst Computer Scientists for its minimalism.

Whilst it's certainly original, I think its influence on CS and esoteric programming languages has been too strong. Many people's idea of an "esoteric language" is just [Brainfuck + something](http://esolangs.org/wiki/Category:Brainfuck_derivatives), and Brainfuck is a go-to language for those who need minimalism. For example, [a NASA study on self-replicating machines](http://www.niac.usra.edu/files/studies/final_report/883Toth-Fejel.pdf) mentions it on page 43:

> ...we thought that our Controller subsystem should also be as simple as possible. We spent quite a bit of time looking at the simplest implementation of a Turing machine, running the simplest possible language, BrainF[uck] (also called BF) - the smallest Turing-equivalent language in existence, having only eight instructions.

Another example is using a minimal language to minimise bias when [measuring the performance of AI systems](http://arxiv.org/abs/1109.5951).

However, when it comes to minimalism, Brainfuck is actually rather *complicated*.

## Too Many Instructions ##

As mentioned in the above quote, it has 8 instructions:

 - `+` increment the current memory cell
 - `-` decrement the current memory cell
 - `,` request some input and put it in the current memory cell
 - `.` output the current memory cell
 - `<` move left one memory cell
 - `>` move right one memory cell
 - `[` marks the start of a loop
 - `]` loop back to the matching `[`, if the current memory cell is non-zero

This is more complicated than necessary, since we can make a so-called ["One-Instruction Set Computer"](http://esolangs.org/wiki/OISC).

## Unnecessary Asymmetry ##

Brainfuck separates code from data, which causes it to bloat. For example, we have an *instruction pointer* which moves and loops through the code. We also have a separate *memory pointer* which indicates which data to modify.

Many languages don't do this, for example the earliest computing schemes like [Turing machines](http://en.wikipedia.org/wiki/Turing_machine) and [Lambda Calculus](http://en.wikipedia.org/wiki/Lambda_calculus).

The OISC languages above, like [BitBitJump](http://esolangs.org/wiki/BitBitJump), have only one representation for code and data (ie. self-modifying code).

## Hard-coded IO ##

Brainfuck has a built-in notion of IO, using the `,` and `.` instructions. Other languages, like [Lazy K](http://esolangs.org/wiki/Lazy_K), encode their IO as regular data, using streams or monads.

## Degrees of Freedom ##

Brainfuck implementations also have many degrees of freedom, requiring arbitrary implementation decisions. These include the word size (how many bits per memory cell) and memory size (how many cells). Other languages, like [Flump](http://esolangs.org/wiki/Flump), avoid these issues allowing an unbounded amount of data (in Flump, we write numbers in unary (eg. `11111` for 5) and separate them using zeros (eg. `0111011` for 3 followed by 2).

There are also arbitrary implementation decisions like what happens when integers overflow, what happens when the cursor overflows the memory, etc. which are complete non-issues when there's no word size, no external memory, etc.

## Imperative Order-of-Execution ##

Brainfuck is completely dependent on the particular sequence that its code is executed in, which is why it needs exactly one instruction pointer to keep track of where it's up to in the program. Purely functional languages like [Binary Lambda Calculus](http://en.wikipedia.org/wiki/Binary_lambda_calculus) don't suffer this problem; all parts of a program can be evaluated in any order, any number of times, or even all at once.

## Implementation Overhead ##

If we want the simplest language to *implement* (in a Worse is Better sort of way), then we shouldn't choose Brainfuck as its implementation is quite complicated. For example, when I wanted to embed a simple language in [a Javascript search algorithm](/projects/optimisation/levin.html) I chose BitBitJump.

Most of [the code](/js/optimisation/levin_bbj.js) is dedicated to the search algorithm. The BitBitJump implementation is just these two lines, inside one of the loops:

```javascript
mem[read_address(counter, m)] = mem[read_address(counter+m, m)];
counter = read_address(counter+2*m, m);
```

`m` is the word size (constant during a program execution), `counter` starts at 0, `mem` is an array of bits and `read_address(x, y)` converts those bits into a Javascript int (reading `y` bits, starting at index `x`).

## Conclusion ##

Whether your interest in Brainfuck is due to its bizarre workings, or due to its small size, you'd actually be better off looking at alternatives. Some of the languages mentioned above are elegant and simple, and a great fit for those wanting minimalism. Others are just as bizarre as Brainfuck, but their strangeness is much clearer and more accessible. In other words, they have an inherent *complexity*, rather than just being *complicated* by implementation details.
