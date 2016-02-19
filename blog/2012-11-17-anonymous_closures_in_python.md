---
title: Anonymous closures in Python
---
## Anonymous Closures In Python ##

If you're a Scheme or Javascript programmer, you're probably familiar with code like this:

```javascript
var toggle = (function() {
    var state = false;
    return function() {
        state = !state;
        return state;
    }
})();
```

In English, this roughly translates to "Run a function which creates a boolean called `state`{.javascript} and returns a function which returns the value of `state`{.javascript}, flipping it each time."

Each time we run `toggle`{.javascript}, we get the opposite value to the last time. The sequence is guaranteed to be `true`{.javascript}, `false`{.javascript}, `true`{.javascript}, `false`{.javascript}, ... regardless of what any other code is doing, since the value of `state`{.javascript} is only available to the `toggle`{.javascript} function; nobody else can change it's value, and we can see that `toggle`{.javascript} will flip it every time. Hence the guarantee.

This makes use of two different but related constructs: 'anonymous functions' and 'closures'.

### Anonymous Functions ###

For a function to be 'anonymous', it must not have a name. This means that it must be a value, for example:

```javascript
var full_url = 'http://' + domain + '/' + path;
```

We read this as "Stick together the text `'http://'`{.javascript} with the value we've called `domain`{.javascript}. Then stick the result on to the text `'/'`{.javascript}. Then stick this result on to the value we've called `path`{.javascript}, and call the final result `full_url`{.javascript}."

We use 3 names here (`domain`{.javascript}, `path`{.javascript} and `full_url`{.javascript}), so where does anonymity come in to it? The anonymous things are those I've referred to as "results". At one point we are manipulating the result of `('http://' + domain)`{.javascript}; what is the name of this temporary, unfinished piece of text? It doesn't have a name at all, we just pass it straight on to the next `+`{.javascript}. Likewise for all of the intermediate values, until eventually we give the name `full_url`{.javascript} to the result, which would otherwise be anonymous too.

The same thing happens in our `toggle`{.javascript} example. Here we have anonymous functions, which are referred to in the English as 'a function which...'. We have two anonymous functions here: the first function which is defined, run, then thrown away in favour of its output. This is similar to the text above which is created, sent to the next `+`{.javascript}, then thrown away in favour of the output of the `+`{.javascript}. Likewise, inside the first anonymous function we have another function which is defined, only to be sent back as a return value. In this case we do eventually give it a name, `toggle`{.javascript}, but not in its original context. The ability to manipulate and pass around functions requires them to be values, just like bits of text, numbers, booleans, etc.

### Closures ###

A closure is a function which contains free (ie. non-argument) variables along with values for those free variables (known as the "environment", "state" or "context" of the closure). In our example above, the inner function uses the variable `state` but doesn't define it or accept it as an argument. As far as this function's concerned, `state`{.javascript} is "free" to be anything; the definition of the function is "open", since its behaviour is unknown if we don't know `state`{.javascript}.

The outer function **does** define a variable called `state`{.javascript}, so this is used to plug the free variable hole in the inner function; the definition of `state`{.javascript} "closes" the inner function's definition, or the inner function is "closed over `state`{.javascript}" (in the same way that the Naturals are "closed over addition"), or that the inner function gets closure from `state`{.javascript}, or the inner function is a "closure".

Closures are a very powerful tool, and they complement anonymous functions well. In this example, we create our closure and return it. This allows the closure to be stored in a variable (`toggle`{.javascript}) and called over and over, even though the function which defines `state`{.javascript} only gets called once. This simple idiom causes the value of `state`{.javascript} to persist between calls to `toggle`{.javascript}, so that it correctly returns alternating values each time.

Since the value of `state`{.javascript} is only accessible by the outer and inner functions above, and since the outer function is anonymous and never stored in a variable, the only code which has access to `state`{.javascript} is `toggle`{.javascript}. This makes closures useful for *encapsulation*; looking after potentially-fragile data such that the rest of the world can only access it in safe and relevant ways. In our case above, it is safe to rely on `toggle`{.javascript} to give opposite answers every time it's called, since nothing can 'reset' the value of `state`{.javascript}, or break `toggle`{.javascript} by turning `state`{.javascript} into, for example, a number.

In Object Oriented languages there is the concept of "private members", but these are a crude approximation to free variables with closure. For example, in a class/instance Object Oriented language, objects can have private members and methods can access those private members. However, methods cannot have private members. Likewise methods generally aren't given methods. Everything in Object Oriented languages happens at the value level, which severely restricts the ability to abstract (since we don't have anonymous function values).

Note that it may seem rather arbitrary that we take the value of `state`{.javascript} from the enclosing function's scope. Indeed it is arbitrary, and there have been many alternative implementations. For example, McCarthy's original LISP takes free variables from the scope that the function is *called* in; this is known as dynamic scope, which still lives on in Emacs LISP. The convention of taking free variables from the scope where the function is *defined*, called lexical scope, came later and went mainstream with ALGOL 60. The virtues of lexical scope compared to dynamic scope became clear when it was incorporated into LISP, which resulted in Scheme. Scheme is a very lightweight language compared to most other LISPs (Common LISP, MACLISP, etc.) because it applied lexical scope and (closely-related) continuations uniformly to approach most problems; thus there are fewer edge-cases, fewer control structures, and much less of a combinatorial explosion of incompatibilites.

The question is: how can we do this in Python?

## Closures in Python ##

Python has anonymous functions, which look like the following:

```python
lambda x: x + 5
```

`lambda x:`{.python} means the same as `function (x) {`{.javascript} in Javascript, and there's no need to write `return`{.python}, since in Python an anonymous function can only ever contain a single expression, which is assumed to be the return value. This restriction to one expression, and thus no statements, is a Good Thing^TM^ since it can make reasoning about code much easier; if our functions can only do one thing, then we're encouraged to make small, simple, easy-to-understand functions, then compose them together. Also, since we're only allowed an expression, and not a statement, it is implied that anonymous functions will be pure and side-effect-free.

This restriction, however, also poses a problem when we try to use state. For example, we can't write the Javascript line `var state = false;`{.javascript}, since the Python equivalent `state = False`{.python} is a statement and not an expression.

We can get around this quite easily by turning local definitions into arguments. For example, we can rewrite our Javascript as:

```javascript
var toggle = (function(state) {
    return function() {
        state = !state;
        return state;
    }
})(false);
```

Here we've moved the declaration of `state`{.javascript} into the argument list of the first function, so our outer function doesn't define anything, it's become parameterised by a state. The assignment has moved into the function call, so `state`{.javascript} will initially be `false`{.javascript} due to Beta reduction. This method of capturing variables translates quite readily to Python:

```python
toggle = (lambda state: (lambda *_: not state))(False)
```

However, this isn't quite the same. Firstly, this version of the `toggle`{.python} function can take an argument. This is actually OK, since the argument is a 'vararg' (indicated by the asterisk) which makes it optional. I will use the standard convention of calling things `_`{.python} if they are unused.

The second difference with our Python version is actually very important: the value of `state`{.python} never gets flipped, so `toggle()`{.python} always gives the result `True`{.python}! This is because the Javascript line `state = !state;`{.javascript} is also a statement, not an expression, so we can't include it in our Python function.

Since Python is not a pure (side-effect-free) language there are expressions which cause side-effects, which we can use instead of statements. The problem is, they usually don't give us the values we want to return from our lambdas, so we need to work around the single-expression limitation and include multiple: those which perform the side-effects we want, and one for the return value. I claim that this limitation is actually liberating, since we're forced to implement our own evaluation strategy, and thus we can tailor it to the problem at hand.

A simple solution is to wrap many expressions inside a container (in this case I've chosen a tuple, which Python guarantees to evaluate in-order) then pick out the one we care about. Constructing and destructing a container constitutes an expression, so we can use it in our lambdas. For example, if we want to write the string `"hello"`{.python} to the console, and return the string `"world"`{.python}, we can do the following:

```python
lambda *_: (sys.stdout.write("hello"), "world")[1]
```

This function ignores any arguments (it's a thunk), then it creates a tuple. The first value in the tuple is `None`{.python}, the result of `sys.stdout.write("hello")`{.python}, which has the side-effect of sending `"hello"`{.python} to the output buffer. The second value is the string `"world"`{.python}. We then pull out the second value of our tuple as our result, ie. we return `"world"`{.python}.

Now we need to find an expression which is equivalent to the line "`state = !state;`{.javascript}" in our Javascript. Unfortunately we can't do this directly, since in the Javascript we're dealing with variable names, and simply changing which values they point to, whereas in our Python expressions we're dealing with the actual Boolean values `True`{.python} and `False`{.python} themselves. Understandably, Python will not let us change `True`{.python} into `False`{.python} or vice versa, so we need to create a pointer-based abstraction, similar to using variable names like the Javascript. A simple way to do this is to keep our values in lists, and replace the contents of the lists when we want to 'update' a value. Voila:

```python
toggle = (lambda state:
    (lambda *_: (state.append(not state.pop()), state[-1])[1])
)([False])
```

Here our `state`{.python} value is given as `[False]`{.python}, which is a list containing the value `False`{.python}. The side-effecting expression we use is `state.append(not state.pop())`{.python} which first "pops" the last value out of `state`{.python}, so that `state`{.python} becomes an empty list `[]`{.python} and we receive the value `False`{.python}. We then use the `not`{.python} operator on this value (equivalent to Javascript's `!`{.javascript}) to receive a value of `True`{.python}. We then use the `state.append`{.python} method to push this value on to the end of `state`{.python}, so that `state`{.python} has become a list containing the value `True`{.python}, or `[True]`{.python}. Next time it will start with `[True]`{.python} and result in `[False]`{.python}. Like before, we extract the second element of our tuple to produce our return value, correctly implementing the toggling behaviour.

## Stacks ##

The above may seem a pretty pointless exercise, and I would agree that production code should probably be written in a more straightforward way, but now that we've thrown away Python's built-in statement-sequencing and namespace implementations, we've actually exposed some pretty interesting problems, which we can apply some nifty Computer Science to.

The observant of you will have spotted that popping and appending lists like I've just done is actually stack manipulation. Since I've just exposed a stack-oriented subset of Python, it makes sense to look at some stack languages.

The exemplar of a language oriented around stacks is Forth; however, in Forth everything is a procedure (statement), which goes against the nice expression-only form we were inching towards above. In Forth, the stacks are global, mutable and, most importantly, external entities. Forth procedures can be used to manipulate stacks, but only in the same way that C procedures can be used to manipulate memory. Neither actually has a "stack"(/"memory") datatype; the stack(/memory) lives outside the program, and the language provides means to access it. In fact, the only reason Forth is 'stack-oriented' rather than 'memory-oriented' is that its procedures are generally written to make heavy use of the stacks; but they don't have to, they could just as well write to memory instead.

There is actually another nice stack language out there which *is* expression-based; in fact it's purely functional! It's called Joy and rather than running procedures which may happen to modify an external stack, in Joy everything is a function which accepts a stack and returns a stack.

For example, consider `dup`{.joy} which pops a value off the stack and pushes it back on twice. Although its before/after behaviour in Forth and Joy is identical (the operational semantics), the interpretations of what's happening (the denotational semantics) are very different.

In Forth, there is a stack containing a value `x`{.forth}. `dup`{.forth} is a procedure which takes no arguments and returns no values (just like everything in Forth). As a side-effect of running `dup`{.forth}, Forth looks at its stack and pops `x`{.forth} off the top. Forth then pushes `x`{.forth} back on to the stack, then pushes another `x`{.forth} on top as well.

In Joy, `dup`{.joy} is a function. To call `dup`{.joy}, we must give it a stack, and it will give us back a stack, but since in Joy everything does this, we don't need to bother writing it; it's implied. The stack which `dup`{.joy} returns is the input stack with a duplicate of the top value.

In Python we can write this as:

```python
dup = lambda xs: xs + [xs[-1]]
```

There are a lots of other stack functions we can make; in fact, since stack languages are Turing-complete, we can make any realisable function as one between stacks. However, there's no point implementing more than we need. As I wrote about before, [Brent Kerby] [1] conjectures (by analogy to Schoenfinkel's applicative combinatory logic) that a Universal set of stack functions is as concise as:

[1]: http://tunes.org/~iepos/joy.html

```{.python}
# Pops off two values, calling the top one on the remaining stack.
# Similar to Schoenfinkel's K combinator 'lambda x: lambda y: x'
k = lambda xs: xs[-1](xs[:-2])

# Pops off two values and pushes two new functions. The first function
# pushes the second value then calls the first value. The second
# function calls the first value then pushes the second.
cake = lambda xs: xs[:-2]               + \
    [lambda ys: xs[-1](ys  + [xs[-2]])] + \
    [lambda ys: xs[-1](ys) + [xs[-2]] ]

# Optional; function composition (saves writing lambda all the time!)
o = lambda f: lambda g: lambda x: f(g(x))
```

```{.unwrap pipe="tee -a stack.py | root/static/null"}

from random  import randint
from inspect import getargspec
r = lambda *_: randint(-1000, 1000)

def test(f):
  (args,_,_,_) = getargspec(f)
  for _ in range(0, 10):
    vals = map(r, args)
    f(*vals)

@test
def k_test(x, y):
  msg = "K behaves for " + str([x, y])
  assert k([y, x, lambda a: a[0] == y]), msg

@test
def cake_length(x, y):
  out = cake([x, y])
  assert len(out) == 2, "Out length for " + str(out)

@test
def cake_fst(x, y):
  f   = lambda a: a
  out = cake([x, f])[-1]([y])
  assert out == [y, x], "cake_fst 1 " + str([x, y, out])

```

With these functions defined we can create any mapping we like from lists to lists, which means we can express any kind of mutable state we like by putting values in lists and updating them with closures.

### Arguments ###

In a similar vein to stacks, there is another nugget of Computer Science lurking around here known as *de Bruijn indexing*.

Let's say we have a Javascript function like this:

```javascript
(function(a) {
    return (function(b) {
        return function(c) {
            a = a + b;
            b = b * c;
            return a + b + c;
        };
    })(2);
})(1);
```

We can write this in Python using the techniques above, to get the following:

```{.python}
# Direct use of Python
direct = (lambda a:
           (lambda b:
             (lambda c: (
               a.append(a.pop() + b[-1]),
               b.append(b.pop() * c),
               a[-1] + b[-1] + c
             )[-1])
           )([2])
         )([1])

# Using stack manipulation we would need to discard the function above and start
# from scratch. My first attempt gives a function something like this, where q()
# is quotation and the rest of the functions are Kerby's concatenative
# combinators (note, this code probably won't run ;) ):
i    = lambda xs: xs[-1](xs[:-1])
dip  = lambda xs: xs[-1](xs[:-2]) + [xs[-2]]
swap = lambda xs: xs[:-2] + [xs[-1], xs[-2]]
q    = lambda x: [x]

q(
    q(q(q(dup), dip, swap), dip, swap),
    dip,
    dup,
    q(swap, dup, q(i), dip),
    dip,
    swap
), dip, swap, q(
    dup,
    q(q(q(q(dup), dip, swap), dip, swap), dip, swap, i),
    dip
),
dip,
q(q(q(dup),dip, swap), dip),
dip,
dup,
q(i, q(dup), dip),
dip,
i
```

# This takes a stack:

[[lambda xs: cons(xs[-1] + xs[-2])(xs[:-2])],               # Addition
 lambda xs: cons(xs[-1] * xs[-2])(xs[:-2])]),         # Multiplication
 a, b, c]                       # Equivalent to 'a', 'b' and 'c' above

# and gives a stack:
[addition, multiplication, (a+b), (b*c), (a+b+b*c+c)]
```

Stack manipulation can be very powerful, but here we end up doing a lot of shuffling around, since we haven't given ourselves a namespace to define new functions, etc.

There is an alternative, which doesn't require re-working our function-based algorithms, but which closely resembles a stack language. The trick is to look at the function definition and spot that `a`{.python}, `b`{.python} and `c`{.python} are meaningless placeholders. By using letters of the (latin) alphabet, we're effectively indexing our arguments, in the same way that we'd index a list. But we don't use letters to index lists (we'd lose all ability to perform arithmetic, etc.) so why do it for arguments? Let's index our arguments with Natural numbers instead!

A rather direct interpretation might go as follows:

```python
(lambda _0:
    (lambda _1:
        (lambda _2: (
            _0.append(_0.pop() + _1[-1]),
            _1.append(_1.pop() * _2),
            _0[-1] + _1[-1] + _2
        )[-1])
    )([2])
)([1])
```

However, I shall proclaim that this breaks code locality, since we get an implicit dependency on global information (the current argument number). We can aleviate this by reversing the numbers:

```python
(lambda _2:
    (lambda _1:
        (lambda _0: (
            _2.append(_2.pop() + _1[-1]),
            _1.append(_1.pop() * _0),
            _2[-1] + _1[-1] + _0
        )[-1])
    )([2])
)([1])
```

This way, the definitions of the inner functions don't depend on knowledge of their context, at the expense of outer functions requiring knowledge of their internals (which seems fair enough to me). There is effectively an API that the outer functions must provide and the inner functions must consume.

This is the right idea, but these variable names are still opaque to the computer. They only mean something to the programmer (unless we use 'eval', at which point all bets are off). Every time we hide our intentions from the computer, whether intentionally through obfuscation or accidentally through bad design, we're just making life harder for ourselves down the line, since the computer won't be able to help us.

Here's an alternative which uses a list to hold all arguments, but rather than push, pop and apply functions to the list like we did for the stack implementation, we can index it using actual, information- rich  numbers (how many arguments back to go). This is better than using contrived names (eg. if we'd used a dictionary), since the computer understands the names too:

```python
# "pushing" a value in an environment sticks the value on the top of
# the stack
push = lambda env: (lambda x: env.append(x))

# "popping" an environment removes a value from the top of the stack
pop = lambda env: (lambda _: env.pop())

# "peeking" into an environment returns a value from the stack without
# removing it.
# This requires an extra layer of indirection to get around Python's
# call-by-value semantics; instead of returning the value, we return a
# function which will apply the value to its argument. This assumes
# that every value is a function, but that's nothing to worry about
# if we're consistent
peek = lambda env: (lambda n: (lambda x: env[-1 * (n+1)](x)))

# "prepare" is a simple wrapper which turns a regular function into
# one which pushes and pops arguments
prepare = lambda (push, pop):
    (lambda f:
        (lambda x:
            (push(x), f(None), pop(None))[1]))

# "running" a function creates a new environment and passes
# specialised instances of "peek" and "prepare" for that environment
# to the function
run = (lambda env:
    lambda f:
        f((prepare((push(env), pop(env))), peek(env)))
)([])
```

With these helper functions in hand, we can now write anonymous functions which push and pop rather than using named arguments, like the following (note that they must take one argument at a time, but that's fine since we can Curry any n-ary function into nested unary functions):

```python
foo1 = lambda a: lambda b: b(a)

foo2 = run(lambda (f, v):
    f(lambda _: f(lambda _: v(0)(v(1))))
)
```

Here `foo1`{.python} is equivalent to `foo2`{.python}, but there's an awful lot of boilerplate. We can alleviate this if we create a simple Domain-Specific Language (DSL) to abstract it away.

Unfortunately we don't have built-in macros (hygenic or otherwise) or generalised Monads in Python, so I'll make do with a direct representation of syntax trees as lists. Tuples would be nicer, but Python requires awkward postfix commas for empty and singleton tuples, which will just serve to confuse us.

Our syntax requires two main forms of expression: function definition and function application. We'll represent a function definition as a new list, so `[[None]]`{.python} is like `f(lambda _: f(lambda _: None))`{.python}. We'll represent function application by 'consing' values (ie. putting them one-after-another in a list), so `[a, b, c]`{.python} is equivalent to `a(b)(c)`{.python}.

We can also put arbitrary Python expressions where we like, with three caveats:

 - Python's call-by-value semantics will evaluate anything outside a (Python) function definition immediately, which we can work around using lambdas.
 - Python expressions are opaque to our compiler; if we want to re-enter our language, we have to explicitly call our compiler function again.
 - Raw lists are swallowed by the interpreter, so they need thunking.

```python
# A thunk-maker. Actually, this is Schoenfinkel's K combinator
k = lambda x: lambda _: x

# A poor man's approximation to LISP's cond. Takes pairs of functions;
# the head is the test and the tail is the result if the test passes
cond = lambda expr, conds: filter(
    lambda (test, _): test(expr),
    conds + ((k(True), k(None)))
)[0][1](expr)

# A nicer interface to Python's equality methods
eq = lambda x: lambda y: x == y

# A nicer interface to Python's list constructor
cons = lambda x: lambda y: [x] + y

i = lambda e: run(lambda (f, v):         # Our AST interpreter.
    cond(type(e), (                      # First match on the type.
        (eq(type([])),                   # A list is an AST node.
            lambda _: cond(len(e), (     # Match on the length.
                (eq(0),                  # Empty nodes are void thunks
                    lambda _:            # ie. 'lambda _: None'.
                        k(None)),        # Args are ignored, so no f.
                (eq(1),                  # Singletons are functions.
                    lambda _:            # Recurse to interpret the
                        k(f(i(e[0])))),  # value before thunking it.
                (eq(2),                  # Pairs are applications.
                    lambda _:            # Recurse to interpret the
                        f(i(e[0]))(      # function and argument
                            i(e[1]))),   # before applying them.
                (k(True),                # Length of e > 2, we have
                    lambda _:            # chained applications.
                        i(cons(          # Split off the first pair,
                            f(i(e[:2])), # interpret them, cons them
                            e[2:]))))),  # back and recurse.
        (eq(type(0)),                    # Natural numbers are de
            lambda _: lambda _: v(e)     # Bruijin indices
        (k(True),                        # e is not a list, so just
            k(expr))))                   # thunk it and we're done
```

Now we can make pure functions, much more conciseley than with Python's regular syntax:

```python
# Using names. A bit clunky.
foo1 = lambda _2: (
    lambda _1: (
        lambda _0: _2(_1, _0)
    )
)

# Using de Bruijn indices. A bit clunkier, but in a very
# regular, automatable way. f and v are the function wrapper
# and value peeker, curried with a fresh environment
lambda (f, v): f(
    lambda _: f(
        lambda _: f(
            lambda _: v(2)(v(1)(v(0)))
        )
    )
)

# Using de Bruijn indices and the DSL is much more concise
i([[[2, 1, 0]]])
```

We can't write closures using this scheme though, since the stack gets popped when we leave a function. In other words, this won't work:

```python
lambda (f, v): f(
    (lambda _: f(
        (lambda _: f(
            lambda _: (
                v(2).append(v(2).pop() + v(1)[-1]),
                v(1).append(v(1).pop() * v(0)),
                v(2)[-1] + v(1)[-1] + v(0)
            )[-1]
        ))([2])
    ))([1])
)
```

To get around this we need to use a spaghetti stack, but that requires us to stop using Python's built-in lists. I may do that in another post but I won't do it here, since I've added so much stuff already that its publication keeps getting delayed ;)

## Types ##

Now this is all ridiculous, so what's the point? Well, the trick is to see what happens when the assumptions are broken. We're using functions of one variable (which is good, since it means they're curried) and using Natural numbers to peek into a stack, known as de Bruijn indexing. But silly de Bruijn surely missed that there's nothing stopping us from peeking too far back in a stack and running out of arguments! Oh the huge manitee!

In fact, this isn't a bad thing at all! We've already come across variables which don't exist in our argument list; they're *free variables*. When all of our variables are de Bruijn indices, they're implicitly telling us their 'freeness'; in other words, how many wrappers they need in order to resolve.

What if we don't supply enough wrappers, what should we do then? Well, since we can't evaluate the variable, we should leave it unevaluated. There's always a wraper around us, in the form of the language runtime and operating system, so this can interpret it for us. One way they can do such a thing is to treat unevaluated functions as  "type constructor"; functions which cannot be evaluated away.

The simplest type constructor is for the unit value, of unit type. This is sometimes written as `()`{.haskell} but in Python it is known as `None`{.python} (and is of `NoneType`{.python} type). This constructor function is 'nullary', it takes no arguments. If we see a unit constructor anywhere, it is always representing a value, albeit one that's devoid of information.

If a nullary constructor is ever applied to an argument, that's a type error. However, we don't have to care in our case since we never try to evaluate the constructors.

Here's an example of code which accepts anything and spits out a unit value; assuming we're interpreting the first free variable to be a unit type constructor:

```python
i(1)
```

Another simple type constructor is for the Boolean type, with constructors `True`{.python} and `False`{.python}. Again these are nullary; their mere presence denotes a value, although this time there is one bit of information contained in the value. With this said, boolean values are usually nowhere near as useful as boolean functions, which we can define as:

```python
# true
i([[1]])
# false
i([[0]])
```

These take two arguments and spit out the first or the second; their presence acts like an `if`{.haskell}-`then`{.haskell}-`else`{.haskell} expression.

There are other constructors which take arguments, for example tail-recursive lists. They can either be nullary `nil` or binary `cons`:

```python
# nil
i(1)

# cons
i([[2, 0, 1]])
```

I'll wrap this up here, but will revisit some of these topics in later posts. It's interesting to see how inter-related many topics are in Computer Science. Even in a language which takes many implementation choices away from us, like Python, a little hackery can bring them back and allow us to compose our programs in new and interesting ways.

Also, it's always fun to mis-use any language and see what we can get away with ;)

```{.unwrap pipe="sh | root/static/null"}
# Run tests
#python stack.py
```
