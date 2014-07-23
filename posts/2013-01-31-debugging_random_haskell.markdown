---
title: Debugging random Haskell
---
I was asked to look at some Haskell code the other day, from some kind of
online roleplaying game. The code is given, along with the briefing that
it should find a valid password amongst a file of about 35,000. Its creator
supposedly fired, and managed to mangle it before he was kicked out. Our
task is to find the correct password.

Rather than look at the file, let's dive straight into the code. I found
this particularly enjoyable, since it's essentially an exercise in debugging
and refactoring, which is how I tend to write code. I'll write a naive version
which is generally very inefficient, over-complicated, repetitive, hard-coded
and fragile. I'll then repeatedly refactor it until it becomes efficient,
simple, terse, generic and robust (I hope!). I assume no knowledge of Haskell,
but knowing a language with first-class functions like Javascript may be
useful.

```haskell
import ParseLib
```

Import an unspecified parsing library; this isn't too significant, but some
Googling will find it if you really care.

```haskell
runner :: [Char] -> [a]
```

This is a type annotation. It's only a sanity check, since Haskell infers types.
Thus there's no point debugging it, so we remove it. I'll ignore type
annotations from now on, as a mangled annotation will just confuse us.

```haskell
runner a | (length (test (a ++ "z")) == (sum [product (3..5), product (6..9), 1])) = False =
mzero
        | otherwise                                                                        =
result [papply defB a]
```

This is an invalid function definition. Let's clean up the line breaks:

```haskell
runner a | (length (test (a ++ "z")) == (sum [product (3..5), product (6..9), 1])) = False = mzero
         | otherwise                                                                       = result [papply defB a]
```

Each line is a different 'clause' of the function; they have the following
form (`|`{.haskell} means 'where'):

```haskell
myFunction myArgument | myCondition = iWillRunIfMyConditionIsTrue
                      | otherwise   = iWillRunIfMyConditionIsFalse
```

This syntax can simplify complicated function definitions, but in our case
I think it's clearer to use an equivalent if condition:

```haskell
runner a = if (length (test (a ++ "z")) == (sum [product (3..5), product (6..9), 1])) = False
              then mzero
              else result [papply defB a]
```

There's a rookie error at the end of the if-condition, common from many
languages: it uses `=`{.haskell} (definition) in the if condition, instead of `==`{.haskell}
(comparison). Let's swap it:

```haskell
runner a = if (length (test (a ++ "z")) == (sum [product (3..5), product (6..9), 1])) == False
              then mzero
              else result [papply defB a]
```

Notice that we're comparing something to `False`{.haskell}; we can get rid of the
`== False`{.haskell} if we swap the `then`{.haskell} and `else`{.haskell} branches around:

```haskell
runner a = if length (test (a ++ "z")) == (sum [product (3..5), product (6..9), 1])
              then result [papply defB a]
              else mzero
```

The right-hand-side of the condition doesn't contain `a`{.haskell}, so it must be constant
(due to 'referential transparency': the output of a function is completely
determined by its input, and here there are no inputs). Let's calculate it:

```haskell
sum [product (3..5), product (6..9), 1]
```

There's no such thing as `(3..5)`{.haskell} or `(6..9)`{.haskell}. It could mean `3.5`{.haskell} and `6.9`{.haskell}, but
that wouldn't type check, since `product`{.haskell} wants a list of numbers. This makes it
more likely to be `[3..5]`{.haskell} and `[6..9]`{.haskell} which is shorthand for `enumFromTo 3 5`{.haskell}
and `enumFromTo 6 9`{.haskell}, which are, respectively, `[3, 4, 5]`{.haskell} and `[6, 7, 8, 9]`{.haskell}:

```haskell
sum [product [3, 4, 5], product [6, 7, 8, 9], 1]
```

The products are easy to calculate:

```haskell
sum [60, 3024, 1]
```

As is the sum:

```haskell
3084
```

If we put this simplified condition back into our original function we get:

```haskell
runner a = if length (test (a ++ "z")) == 3084
              then result [papply defB a]
              else mzero
```

`++`{.haskell} is a built-in function, defined like this (where `[]`{.haskell} is an empty
list and `x : y`{.haskell} is a singly-linked list starting with element `x`{.haskell} and followed
by list `y`{.haskell}):

```haskell
[]             ++ list2 = list2  -- Concatenating an empty list does nothing
(elem : list1) ++ list2 = elem : (list1 ++ list2)  -- Recurse
```

What about `test`{.haskell}?

```haskell
test x = init x
```

The function `test`{.haskell}, of argument `x`{.haskell}, is defined as `init`{.haskell} applied to `x`{.haskell}. Clearly:

```haskell
test = init
```

That transformation is known as eta-reduction. The `init`{.haskell} function is built in
to Haskell's standard library too. It looks like the following:

```haskell
init (elem : []) = []  -- init of a one-element list is an empty list
init (elem : xs) = elem : init xs  -- Recurse
```

`init`{.haskell} chops an element off the end of a list. Since we're concatenating with
a non-empty list, `init`{.haskell} will recurse over `a`{.haskell} and end up at `"z"`{.haskell}. Let's simplify:

```haskell
length (init (a ++ "z"))
length (a ++ init "z")
length (a ++ init ('z' : []))  -- "z" is a one-element list of Chars
length (a ++ [])
length a
```

This gives us a much simpler version of the original function:

```haskell
runner a = if length a == 3084
              then result [papply defB a]
              else mzero
```

Now what's `result`{.haskell}?

```haskell
result []          = "Nope"
result ((x,xs):ys) = x
```

`(foo, bar)`{.haskell} is a pair, so `result`{.haskell} is extracting the first element of a pair,
which itself is the first element of a list. We know that this element will
be a `String`{.haskell}, since it has to be of the same type as `"Nope"`{.haskell}.

We can split this into two separate element extractions, using the built-in
functions `fst`{.haskell} (get the first element of a pair) and `head`{.haskell} (get the first
element of a list):

```haskell
result [] = "Nope"
result xs = fst (head xs)
```

Notice that `runner`{.haskell} calls `result`{.haskell} on a one-element list `[papply defB x]`{.haskell},
therefore:

```haskell
result [papply defB x]
result ((papply defB x) : [])  -- [foo] is sugar for foo:[]
fst (head ((papply defB x) : []))  -- Bring 'result' in-line
fst (papply defB x)  -- 'head (x:y)' is just 'x'
```

This gives us a simplified `runner`{.haskell}:

```haskell
runner x = if length x == 3084
             then fst (papply defB x)
             else mzero
```

`mzero`{.haskell} is funny; it's a 'method' of a 'type class' called `MonadPlus`{.haskell}. A
type class is basically an interface, and types can be instances of type
classes in the same way that Object Oriented classes can implement OO
interfaces (confusing re-use of existing terminology, I know!).

To know which implementation of `mzero`{.haskell} we're dealing with, we need to know
it's type. This must be the same as the `then`{.haskell} branch, which we just discovered
must be a `String`{.haskell} (otherwise we couldn't have put `"Nope"`{.haskell} as a possible return
value). As we saw above, `String`{.haskell} is just a synonym for a list of `Chars`{.haskell} (ie. `[Char]`{.haskell}), and indeed
lists are an instance of the type class `MonadPlus`{.haskell}. Their implementation of `mzero`{.haskell}
is the empty list `[]`{.haskell}, which (since we're dealing with `[Char]`{.haskell} AKA
`String`{.haskell}) we can also write as `""`{.haskell}:

```haskell
runner x = if length x == 3084
              then fst (papply defB x)
              else ""
```

That's as simple as we can get with `runner`{.haskell}. Although we can rearrange it back to
the two-clause version:

```haskell
runner x | length x == 3084 = fst (papply defB x)
         | otherwise        = ""
```

Next we look at the parsers:

```haskell
defB = do char 'X'
           baz <- digit
          char 'X'
          char 'W'
           foo <- defC
           bar <- defD
           char 'a'
           fooo <- many (do char 'f'
                           digit)
           char 'Y'
           char 'Z'
           return (foo ++ bar)
```

This is '`do`{.haskell} notation', which is nice syntactic sugar for Haskell's famously
scary feature, Monads. A `Monad`{.haskell} is just a way to chain functions together. Here
we're chaining together simple parsers into more complex ones. We just write
`do`{.haskell} followed by a bunch of parsers and the `Monad`{.haskell} will do all of the plumbing,
composition, backtracking, shortcutting, etc. for us.

A few points to mention:

 - Haskell's layout rule lets us do without braces and semicolons in favour of indenting and dedenting; the indentation above needs straightening out.
 - By default the intermediate return-values are ignored. To keep them we use an arrow, eg. `foo <- bar`{.haskell} will store the result of `bar`{.haskell} in `foo`{.haskell}.
 - Many of the results above are ignored, so there's not point using the arrows (eg. `baz`{.haskell} and `fooo`{.haskell} are ignored, but `foo`{.haskell} and `bar`{.haskell} are used at the end).
 - A `do`{.haskell} block is just a regular function, hence the nested argument to `many`{.haskell}
 - The return value of a `do`{.haskell} block is the return value of the last function in the chain; to override this, we can use the `return`{.haskell} function which doesn't perform any action (in our case any parsing) but does provide a return value.

```haskell
defB = do char 'X'
          digit
          char 'X'
          char 'W'
          foo <- defC
          bar <- defD
          char 'a'
          many (do char 'f'
                   digit)
          char 'Y'
          char 'Z'
          return (foo ++ bar)
```

It turns out that `defD`{.haskell} is only used once in the whole program, so we may
as well insert its definition straight in here (suitably indented):

```haskell
defB = do char 'X'
          digit
          char 'X'
          char 'W'
          foo <- defC
          bar <- do foo <- defC
                   do baar <- defC
                      digit
                      fooo <- many (do digit)
                      q <- defE
                      return (q : "d")
          char 'a'
          many (do char 'f'
                   digit)
          char 'Y'
          char 'Z'
          return (foo ++ bar)
```

Note that nested chains can be flattened; `do A; (do B; C)`{.haskell} is the same
as `do A; B; C`{.haskell}, as long as we keep the correct return values assigned
to any intermediate results. Likewise `do digit`{.haskell} is the same as `digit`{.haskell}, since
we're not chaining anything on to it:

```haskell
defB = do char 'X'
          digit
          char 'X'
          char 'W'
          foo <- defC
          defC
          defC
          digit
          many digit
          q <- defE
          char 'a'
          many (do char 'f'
                   digit)
          char 'Y'
          char 'Z'
          return (foo ++ q ++ "d")
```

Likewise, `defE`{.haskell} is never used anywhere else. We may as well inline it too:

```haskell
defB = do char 'X'
          digit
          char 'X'
          char 'W'
          foo <- defC
          defC
          defC
          digit
          many digit
          q <- do foo <- defC
                  do fooo <- defC
                     do foooo <- defC
                        return "i"
          char 'a'
          many (do char 'f'
                   digit)
          char 'Y'
          char 'Z'
          return (foo ++ q ++ "d")
```

Simplifying out (we now know the value of `q`{.haskell} is `"i"`{.haskell}):

```haskell
defB = do char 'X'
          digit
          char 'X'
          char 'W'
          foo <- defC
          defC
          defC
          digit
          many digit
          defC
          defC
          defC
          char 'a'
          many (do char 'f'
                   digit)
          char 'Y'
          char 'Z'
          return (foo ++ "id")
```

Now let's look at `defC`{.haskell}. We keep this separate since it's used many times:

```haskell
defC = do char '!'
          baz <- digit
          char 's'
          do s <- many (do char 's')
             return "Val"
         +++ do char '@'
                baz <- char 's'
                digit
                do s <- many (do digit)
                   return 'Val'
```

This is actually in two parts, combined using `+++`{.haskell}. I'll split them into
separate functions to make it clearer:

```haskell
defCA = do char '!'
           baz <- digit
           char 's'
           do s <- many (do char 's')
              return "Val"

defCB = do char '@'
           baz <- char 's'
           digit
           do s <- many (do digit)
              return "Val"

defC = defCA +++ defCB
```

According to ParseLib, `+++`{.haskell} is a choice, so `defC`{.haskell} will try to match `defCA`{.haskell}
first. If it fails, the input will be wound back and `defCB`{.haskell} will be tried.
`defC`{.haskell} will only fail to match if both `defCA`{.haskell} and `defCB`{.haskell} fail to match. Let's
clean it up:

```haskell
defCA = do char '!'
           digit
           char 's'
           many (char 's')
           return "Val"

defCB = do char '@'
           char 's'
           digit
           many digit
           return "Val"

defC = defCA +++ defCB
```

We can see that both branches will return `"Val"`{.haskell} if successful, so we no longer
need to use the `foo`{.haskell} variable in `defB`{.haskell}, we can just use `"Val"`{.haskell} directly:

```haskell
defB = do char 'X'
          digit
          char 'X'
          char 'W'
          defC
          defC
          defC
          digit
          many digit
          defC
          defC
          defC
          char 'a'
          many (do char 'f'
                   digit)
          char 'Y'
          char 'Z'
          return "Valid"

defCA = do char '!'
           digit
           char 's'
           many (char 's')

defCB = do char '@'
           char 's'
           digit
           many digit
```

That's about all the 'simplifying' we can do, but with a little more work we
can make it more succinct, if less simple.

First we can collect together individual characters into strings:

```haskell
string (c:cs) = do char c
                   string cs
```

This reduces `defB`{.haskell} and `defCB`{.haskell} to:

```haskell
defB = do char 'X'
          digit
          string "XW"
          defC
          defC
          defC
          digit
          many digit
          defC
          defC
          defC
          char 'a'
          many (do char 'f'
                   digit)
          string "YZ"
          return "Valid"
defCB = do string "@s"
           digit
           many digit
```

We can also special-case the `foo; many foo`{.haskell} pattern (`+`{.haskell} in
regular-expression syntax). This works by recursing on the left until the pattern `p`{.haskell}
fails to match, backtracking once, then matching `p`{.haskell} without recursing:

```haskell
plus p = (do p
             plus p) +++ p
```

This lets us write `defB`{.haskell}, `3A`{.haskell} and `3B`{.haskell} like this:

```haskell
defB = do char 'X'
          digit
          string "XW"
          defC
          defC
          defC
          plus digit
          defC
          defC
          defC
          char 'a'
          many (do char 'f'
                   digit)
          string "YZ"
          return "Valid"
defCA = do char '!'
           digit
           plus (char 's')
defCB = do string "@s"
           plus digit
```

Notice that `defC`{.haskell} is always used three times in a row? We can hard-code that, to get:

```haskell
defC3 = do defC
           defC
           defC
defB = do char 'X'
          digit
          string "XW"
          defC3
          plus digit
          defC3
          char 'a'
          many (do char 'f'
                   digit)
          string "YZ"
          return "Valid"
```

We can make this a bit more compact if we use explicit bind functions
`>>`{.haskell} rather than do notation in a few places (`do a; b`{.haskell} is sugar for
`a >> b`{.haskell}), and sprinkle some standard Haskell functions around to reduce
redundancy. This gives the final, complete program as:

```haskell
runner x | length x == 3084 = ""
         | otherwise        = fst (papply defB x)
defB = do string "X"
          digit
          string "XW"
          defC3
          plus digit
          defC3
          string "a"
          many (string "f" >> digit)
          string "YZ"
          return "Valid"
string = join . (map char)
plus p = (p >> plus p) +++ p
defC3  = defC >> defC >> defC
defC   = (string "!"  >> digit >> plus (string "s")) +++
         (string "@s" >> plus digit)
```

It's straightforward enough to see what this is doing even without
running it. If you do want to run it, say as a shell command, you
can add this:

```haskell
main = do stdInput <- getContents
          let inputLines = lines stdInput
              successes = filter (("Valid" ==) . runner) inputLines
              formatted = unlines successes
          putStr formatted
```

Pipe passwords in to stdin and you'll get valid ones out of stdout.
Note that Haskell is non-strict, meaning your program will handle
pipes properly, and not sit there buffering.
