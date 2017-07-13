---
title: Tail-recursive Unique
packages: [ 'nix-instantiate' ]
---

## Intro to Nix

I've been playing around quite a bit with the [Nix expression language](http://nixos.org/nix/manual/#chap-writing-nix-expressions) recently. I've [written about](/projects/nixos) [Nix](http://nixos.org/nix) before: it's usually described as a [package manager](https://en.wikipedia.org/wiki/Package_manager) but I think it's actually more useful to think of Nix as a [build tool](https://en.wikipedia.org/wiki/Build_automation#Build_automation_utilities); i.e it's more useful to think of Nix in comparison to [Make](https://en.wikipedia.org/wiki/Make_(software)), [SBT](https://en.wikipedia.org/wiki/SBT_(software)), etc. than to [dpkg](https://www.debian.org/doc/manuals/debian-faq/ch-pkgtools.en.html), [RPM](https://en.wikipedia.org/wiki/RPM_Package_Manager‎), [NPM](https://www.npmjs.com), etc.

I think of Nix as "Make done right": by using a task/build description language with clean syntax and semantics, rather than a fragile, quirky mess of Makefiles, Nix is able to scale from [gathering dependencies via shebang invocation](/projects/nixos/nix_shell_shebangs.html) all the way up to building [whole operating systems](http://nixos.org) and [server](https://nixos.org/nixops) [orchestration](https://nixos.org/disnix).

## The `unique` Function

Anyway, the purpose of this post is to discuss the definition of the `unique` function provided by [Nix's standard library](https://github.com/NixOS/nixpkgs/blob/c71e2d42359f9900ea2c290d141c0d606471da16/lib/lists.nix#L406). This simple function takes a list as its argument, and returns a list containing the same elements as the input but without any duplicate entries:

```{pipe="tee unique_example.nix"}
(import <nixpkgs> {}).lib.unique [ "x" "x" "y" "z" "y" ]
```

```{.odd pipe="nix-instantiate --eval unique_example.nix"}
```

The duplicate copies of `"x"` and `"y"` have been removed, with only their *first* occurence remaining (we can tell because `"y"` appears between `"x"` and `"z"` in the output).

Here's the definition of `unique`, as of the date I write this:

```
unique = list:
  if list == [] then
    []
  else
    let
      x = head list;
      xs = unique (drop 1 list);
    in [x] ++ remove x xs;
```

Personally, I'm quite picky about code layout, some I'm going to adjust the whitespace a little (differences in whitespace are not significant in Nix, except inside strings). I'll also fix some of the variable names to work outside the [lexical scope](https://en.wikipedia.org/wiki/Lexical_scope) where it was originally defined:

```{pipe="tee unique.nix"}
with import <nixpkgs> {};

# Put our definition in a recursively-defined set, so that:
#  - `unique` can reference itself
#  - Our expression returns a value (if we used `let unique = ...;`, we'd need to
#    provide an expression for `in ...`)
#  - We can `import` it for testing
rec {

  unique = list:
    if list == []
       then []
       else let x  =         lib.head   list;
                xs = unique (lib.drop 1 list);
             in [x] ++ lib.remove x xs;

}
```

Hopefully the `if`/`then`/`else` syntax is intuitive enough (note that there's no `endif`/`fi`/`{}`/etc.), and `let`/`in` is straightforward (mutually recursive definitions are allowed, and each definition ends with `;`). `foo.bar` looks up the name `bar` in the set `foo`. The `list: foo` syntax denotes a function, taking a single argument called `list` and returning the expression `foo` (which may contain references to `list`). Juxtaposition, like `lib.head list` and `lib.drop 1 list`, applies functions to arguments; precedence is controlled using parentheses and all functions are curried. The `unique = foo;` syntax just binds this function to the name `unique`, as if it were in a `let` block (or, if you prefer, if it were defining a field of a JSON object `{"unique": ....}`).

Hopefully the operator notation (`==`, `++`, etc.) and list notation (`[` and `]`) are familiar enough from other languages. Note that list elements are separated by spaces, as in Lisp; hence `[foo, bar]` is a syntax error, `[foo bar]` is a two-element list containing `foo` and `bar`, whilst `[(foo bar)]` is a one-element list containing the result of applying `foo` to `bar`.

Now that we've had a crash-course in Nix, we can dig into this function and discover why I found it interesting enough to blog about.

## The Problem

The first reason why `unique` might appear on your radar is if you want to deduplicate a long list. For example, if we dedupe a list of list of numbers between `1` and `10000` we'd expect to get back the same list unchanged (I'm actually returning its length, to avoid spewing 10,000 numbers into the page if it works!):

```{pipe="tee big_list.nix"}
with import <nixpkgs> {};
with builtins;

length (lib.unique (lib.range 1 10000))
```

```{.odd pipe="sh"}
if nix-instantiate --show-trace --eval big_list.nix 2>&1
then
  echo "Evaluating big_list.nix shouldn't have worked!" 1>&2
  exit 1
fi
```

Uh oh! Not only did we overflow the stack, but [rendering](/projects/activecode) this page causes `nix-instantiate` to hit 100% CPU and consume over 1GB of RAM!

This is exactly the issue I hit when testing that a list of Haskell package names I grabbed from [Hackage](http://hackage.haskell.org) didn't contain any duplicates (i.e. `assert length pkgNames == length (unique pkgNames); ...`). I know that `unique`'s duplicate-detecting algorithm takes O(n^2^) time, but a gig of RAM seems excessive, and since Nix is a [sensible](https://en.wikipedia.org/wiki/Tail_call) language it certainly shouldn't be overflowing the stack!

## Unfolding the Algorithm

Going back to the definition of `unique`, we can immediately see that it's not [tail-recursive](https://en.wikipedia.org/wiki/Tail_call), which is why we're overflowing the stack. In a tail-call, the last thing our function should do is call another function; if that function call is recursive (either directly, or via some mutual recursion) then we're tail-recursive:

```
# Simple function, just increments its argument
foo = a: a + 1;

# Doubles its argument, then passes it to a tail-call of `foo`
tailCallToFoo = b: foo (b * 2);

# Not a tail-call to `foo`, since we're doing something (multiplying) afterwards
noneTailCall = c: 3 * (foo c);
```

Concepts like "the last thing we do" are a little tricky in [lazily evaluated](https://en.wikipedia.org/wiki/Lazy_evaluation) languages like Nix, but it's clear that the recursive call in `unique` is not a tail-call, since we perform some extra steps on the result:

```
let x  =         lib.head   list;
    xs = unique (lib.drop 1 list);
 in [x] ++ lib.remove x xs;
```

The usual way to talk about tail-recursion involves [call stacks](https://en.wikipedia.org/wiki/Call_stack) (indeed, that explains why we got a stack overflow!), but I think it's clearer to demonstrate our problem by evaluating our code using [term rewriting](https://en.wikipedia.org/wiki/Rewriting) instead of stacks.

Since the variable `xs` only appears once in the above `let` expression, we can inline it to see more clearly that the call to `unique` is not in tail position:

```
let x = lib.head list;
 in [x] ++ lib.remove x (unique (lib.drop 1 list));
```

We can see the effect of this if we consider our original code example: the list `[ "x" "x" "y" "z" "y" ]`. To distinguish between the elements, I'll use the following variable names:

```
myList = let x1 = "x";
             x2 = "x";
             y1 = "y";
             y2 = "y";
             z1 = "z";
          in [ x1 x2 y1 z1 y2 ];
```

Let's pass this to `unique` and use term rewriting to trace the execution. For clarity, I won't include the above `let` expression (these lines are long enough as it is!), I'll drop the `lib.` prefix and I'll assume our lists are [singly-linked](https://en.wikipedia.org/wiki/Linked_list#Singly_linked_lists) and hence that `head` and `drop 1` are O(1) operations which can be performed as part of the inlining.

Each line is a rewrite of the previous, with a '#' comment on the right-hand-side to explain what's been rewritten; I've tried to align common elements between the lines:

```
                                                                                                         unique myList
                                                                                                         unique [x1 x2 y1 z1 y2]           # Inline myList, ignore the `let`
[x1] ++ (remove "x"                                                                                     (unique    [x2 y1 z1 y2]))         # Inline unique (and `head` and `drop 1`)
[x1] ++ (remove "x" ([x2] ++ (remove "x"                                                                (unique       [y1 z1 y2]))))       # Ditto
[x1] ++ (remove "x" ([x2] ++ (remove "x" ([y1] ++ (remove "y"                                           (unique          [z1 y2]))))))     # Ditto
[x1] ++ (remove "x" ([x2] ++ (remove "x" ([y1] ++ (remove "y" ([z1] ++ (remove "z"                      (unique             [y2]))))))))   # Ditto
[x1] ++ (remove "x" ([x2] ++ (remove "x" ([y1] ++ (remove "y" ([z1] ++ (remove "z" ([y2] ++ (remove "y" (unique               [])))))))))) # Ditto
[x1] ++ (remove "x" ([x2] ++ (remove "x" ([y1] ++ (remove "y" ([z1] ++ (remove "z" ([y2] ++ (remove "y"                       [])))))))))  #           unique [] -->         []
[x1] ++ (remove "x" ([x2] ++ (remove "x" ([y1] ++ (remove "y" ([z1] ++ (remove "z" ([y2] ++                                   []))))))))   #         remove y [] -->         []
[x1] ++ (remove "x" ([x2] ++ (remove "x" ([y1] ++ (remove "y" ([z1] ++ (remove "z"                                          [y2])))))))    #          [y2] ++ [] -->       [y2]
[x1] ++ (remove "x" ([x2] ++ (remove "x" ([y1] ++ (remove "y" ([z1] ++                                                      [y2]))))))     #       remove z [y2] -->       [y2]
[x1] ++ (remove "x" ([x2] ++ (remove "x" ([y1] ++ (remove "y"                                                            [z1 y2])))))      #        [z1] ++ [y2] -->    [z1 y2]
[x1] ++ (remove "x" ([x2] ++ (remove "x" ([y1] ++                                                                           [z1]))))       #    remove y [z1 y2] -->       [z1]
[x1] ++ (remove "x" ([x2] ++ (remove "x"                                                                                 [y1 z1])))        #        [y1] ++ [z1] -->    [y1 z1]
[x1] ++ (remove "x" ([x2] ++                                                                                             [y1 z1]))         #    remove x [y1 z1] -->    [y1 z1]
[x1] ++ (remove "x"                                                                                                   [x2 y1 z1])          #     [x2] ++ [y1 z1] --> [x2 y1 z1]
[x1] ++                                                                                                                  [y1 z1]           # remove x [x2 y1 z1] -->    [y1 z1]
                                                                                                                      [x1 y1 z1]           #     [x1] ++ [y1 z1] --> [x1 y1 z1]
```

The problem is clear, even for a list containing only 5 elements: each time we evaluate `unique`, we accumulate more terms on the left-hand-side which are 'stuck'; we can't begin to evaluate them until we've finished evaluating those on the right-hand-side. For example, we can't apply `remove "x"` directly to `unique [x2 y1 z1 y2]`, since we don't know what the elements of that list will be (and therefore if there are any `"x"` values to remove) until we've evaluated `unique`, at least partially.

In fact, there is an exception to this rule, which might already be [tickling your nostrils](https://en.wikipedia.org/wiki/Code_smell). We know, by definition, that whatever appears on the left-hand-side of the list concatenation operator `++` will definitely appear in the resulting list, even if we don't know what the right-hand-side is yet. That's why I said "partially" above: we can begin to evaluate some of those `remove` calls (assuming [a straightforward implementation](https://github.com/NixOS/nixpkgs/blob/c71e2d42359f9900ea2c290d141c0d606471da16/lib/lists.nix#L112)) before we evaluate the recursive calls to `unique`.

For example, given this line:

```
[x1] ++ (remove "x" ([x2] ++ (remove "x" (unique [y1 z1 y2]))))
```

We can partially evaluate the outer `remove` call instead of the `unique` call to get:

```
[x1] ++ (remove "x" (remove "x" (unique [y1 z1 y2])))
```

Since the value of `x2` is `"x"`, removing `"x"` from a list `[x2] ++ foo` is equivalent to removing it from the list `foo`: the concatenation is completely redundant, since the prefix `[x2]` will eventually be discarded by `remove "x"`, there's no point doing it at all. Of course, that's a quirk of our particular list, but it does make the algorithm seem a little pungent.

Notice that we *still* don't need to evaluate `unique` yet, since this line still contains some redundancies: namely that `remove "x" (remove "x" foo)` is equivalent to `remove "x" foo` (the pong is increasing). We can either use that fact as-is, and rewrite straight to:

```
[x1] ++ (remove "x" (unique [y1 z1 y2]))
```

Alternatively, we can be a little more generic and inline the [definition](https://github.com/NixOS/nixpkgs/blob/c71e2d42359f9900ea2c290d141c0d606471da16/lib/lists.nix#L112) of `remove`:

```
[x1] ++ (filter (a: a != "x") (filter (b: b != "x") (unique [y1 z1 y2])))
```

Now we can fuse the two `filter` traversals, to [avoid producing intermediate lists](https://wiki.haskell.org/Deforestation); note that this will improve performance even if the two values being removed weren't the same (both `"x"` in our case), since we're turning list traversals into conjunctions of boolean expressions, which computers are much better at dealing with:

```
[x1] ++ (filter (c: (a: a != "x") c && (b: b != "x") c) (unique [y1 z1 y2]))
[x1] ++ (filter (c:    (c != "x")   &&    (c != "x"))   (unique [y1 z1 y2])) # Beta-reduce
```

Since they *are* both `"x"`, we can replace `foo && foo` with `foo` to get:

```
[x1] ++ (filter (c: c != "x") (unique [y1 z1 y2]))
```

Note that this is equivalent to the direct replacement of `remove "x" (remove "x" ...)` with `remove "x" ...`.

Still, whilst these transformations give us a bit more insight into the algorithm, such micro-optimisations are just ignoring the elephant in the room which is the recursive call to `unique`.

## Becoming Tail-Recursive

The usual fix to make a recursive algorithm tail-recursive is to give it an extra argument (usually called an *accumulator*), and perform our extra processing steps to that instead of the return value. Here's a version of `unique` which uses an accumulator; we've put the real implementation in the `uniq` helper function, which is used by `unique` to provide the original interface:

```{pipe="tee unique_acc.nix"}
with import <nixpkgs> {};

let uniq = acc: list:
      if list == []
         then acc
         else let x = lib.head list;
               in uniq ([x] ++ lib.remove x acc) (lib.drop 1 list);

 in rec { unique = uniq []; }
```

Let's compare the performance of this version with the original:

```{pipe="cat > time.sh"}
#!/usr/bin/env bash

set -e

function mkExpr {
  echo "with import <nixpkgs> {}; builtins.length ($1 (lib.range 1 $2))"
}

function timeExpr {
  time nix-instantiate --eval -E "$(mkExpr "$1" "$2")"
}

function timeOriginal {
  timeExpr lib.unique "$1"
}

function timeAcc {
  timeExpr "(import unique_acc.nix).unique" "$1"
}

I=1
while [[ "$I" -lt 100 ]]
do
  echo "Timing original with $I" 1>&2
  OUTPUT=$(timeOriginal "$I")
  [[ "$OUTPUT" -eq "$I" ]] || {
    echo "Should have got '$I', instead got '$OUTPUT'" 1>&2
  }

  OUTPUT=$(timeAcc "$I") 1>&2
  [[ "$OUTPUT" -eq "$I" ]] || {
    echo "Should have got '$I', instead got '$OUTPUT'" 1>&2
  }

  I=$(( I * 2 ))
done
```

try this on our original list-length

```{pipe="tee acc_test.nix"}
with import <nixpkgs> {};
with builtins;

length ((import ./unique_acc.nix).unique (lib.range 1 10000))
```

```{.odd pipe="sh"}
if nix-instantiate --show-trace --eval acc_test.nix 2>&1
then
  echo "acc_test shouldn't work!" 1>&2
  exit 1
fi
```

Oops! Looks like that's not done the job. In this case, the culprit is probably lazy evaluation: we've managed to 'unstick' our computations, but they still won't be performed until the result is forced. We can work around this using `seq`: the expression `seq foo bar` will force `foo` and return the expression `bar`; we can use this to force the computations on our accumulator at each step:

```{pipe="tee unique_seq.nix"}
with import <nixpkgs> {};
with builtins;

let uniq = acc: list:
      if list == []
         then acc
         else let x = lib.head list;
               in uniq (seq acc ([x] ++ lib.remove x acc)) (lib.drop 1 list);

 in rec { unique = uniq []; }
```

```{pipe="cat > seq_test.nix"}
with import <nixpkgs> {};
with builtins;

length ((import ./unique_seq.nix).unique (lib.range 1 10000))
```

```{.odd pipe="sh"}
if nix-instantiate --show-trace --eval seq_test.nix 2>&1
then
  true
else
  echo "seq_test should work!" 1>&2
  #exit 1
fi
```

Success!
