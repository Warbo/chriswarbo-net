---
title: "Blog Dump 3: Duck Typing as Constraint Propagation"
---
Another of my unfinished programming musings:

Personally, I really like Python. Python is quick to write, quite readable, but most of all it's dynamic. Many people get caught up on the syntax of languages, usually because that's the easiest part, however the syntax is just a way of expressing the semantics, which is what you actually mean when you code. Let's compare some C code to some Python code:

``` c
int square(int x) {
  return x*x;
}

int mynum = 5;

int mynum_squared = square(mynum);
```

Don't focus on the syntax, I'm keeping such things as simple as possible. This C code simply defines a function called `square`. It says that square acts upon integers ("int") and gives back ("return"s) integers as a result; in this case the result is the input multiplied ("*") by itself. We then make an integer which we call "mynum" and set it to equal to the number 5. We then make another integer which we call "mynum_squared" and we set it equal to the result of the function square acting upon mynum. Since code is just Maths we can simplify (or "run") this to show:

```c
mynum_squared = square(x=mynum)

mynum_squared = {mynum*mynum}

mynum_squared = {5*5}

mynum_squared = 25
```

It's pretty straightforward, just like Maths in school. Let's see how we would write this if we were using Python:

``` python
def square(x):
    return x*x

mynum = 5

mynum_squared = square(mynum)
```

Let's read what this code is doing. We define a function called "square" which acts on something. It also "return"s something as a result. More specifically, what it returns is the input "*" with itself. We then give a name "mynum" to the number 5. We then define another name, which points to the result of the function square acting upon whatever mynum points to. Once again, code is just Maths so we can simplify this too:

```python
mynum_squared = square(mynum)

mynum_squared = square(5)

mynum_squared = 5*5
```

In Python, the asterisk "*", rather than being an easier-to-write-on-a-keyboard multiplication sign like it is in C, is actually syntactic sugar. Syntactic sugar means that it is a shorthand way of writing something that's annoying to write out in full every time. Let's write out the full description of this code, rather than using the convenient shorthand:

```python
mynum_squared = 5*5

mynum_squared = 5.__mul__(5)
```

Now what does this last version say? Well you may recognise the "(5)" as passing the number 5 to a function. The function we're running is called "__mul__", and the "." means that its definition is stored inside the number 5. So what it says is to get the function __mul__ which is stored in the number 5, give it a number 5 and make a pointer to the result called "mynum_squared". Since we can't see the definition here of what the function "__mul__" does, there's no way for us to simplify this code any more.

So how does this massive difference in semantics manifest itself in the syntax of C and Python? In the C code, everything had a type, for example "int mynum = 5;". The name "mynum" cannot store anything that is not an integer*. This allows us to know exactly what the code will do before we run it (as long as we know what the input will be), since we know how everything behaves, although of course it may take us a while to work out exactly what it does (otherwise there's no point writing the program ;) ) and it may take forever (see <a href="http://en.wikipedia.org/wiki/Halting_problem">Halting problem</a>). So where are the types in Python code? Well they're there, but they're a bit redundant. Let's give our Python code some types in a C-style syntax:

```c
object square(object x):

 return x*x

object mynum = 5

object mynum_squared = square(mynum)
```

As you can see, the type of everything is just "object"**, so there's not much point writing it out explicitly for everything. The vagueness of having the type of everything no more specific than object, or essentially "something", makes Python incredibly powerful, since we get a type system called "duck typing". Duck typing comes from the saying "if it walks like a duck, sounds like a duck and looks like a duck then it's a duck".

More formally, what this means is that in our "square" function we take in an object "x" then return "x*x" or, without the syntactic sugar, "x.__mul__(x)". In our C function we take in an int, if we want to use our square function on something else (for example a real number) we need to copy and paste our existing definition and replace all of the "int"s with something else (eg. "float"). In duck typing the type of the input is just 'anything which works', no more and no less.

This is a bit tautological, ie. "square accepts anything which square accepts" but is nevertheless more specific than simply "an object", which is the largest upper bound we could have on the type. If we call the type consisting of all objects T, so that

```python
forall o of type object, o is a member of T of type P(object)
```

We get, through a bastardisation of formal logic, a search over the space of all objects. "T" is the type of object we're looking for, so it is "of type type". At the moment we're just dumping every object we find into T, since we know it acts on objects (although we define it in reverse; we don't put objects into T, we say that T contains it, so that we get a timeless mathematical truth). Now our more specific version we can call T', such that:

```python
forall o of type object, o is a member of T' of type P(object) if and only if square(o) is not a member of errors
```

As I said, this is a bit better, but what we would like to do is replace the use of square(o), since we'd like to get rid of the tautological aspect. We'd also like to use more easily defined concepts than a set of all errors, since if we tried to define the set of all errors we'd end up solving this type problem as part of that, so we've not made our lives easier yet. Let's get rid of those by further refining the type based on what we know about the function, so the more specific T'' is defined such that:

```python
forall o of type object, o is a member of T'' of type P(object) if and only if there exists an f of type function where name(f) = '__mul__' and f is a member of o
```

Now that's gotten rid of the tautology nicely, but our replacement of 'the set of all errors' with 'o must have a function __mul__' is pretty vague since this is still true for objects which would give errors. Thus we can make another refinement and say T''' is defined as:

```python
forall o of type object, o is a member of T''' of type P(object) if and only if there exists an f of type function where name(f) = '__mul__' and f is a member of o and there exists an x of type U where x is a member of arguments(f) and o is a member of U
```

This is getting a bit unweildy, but formal logic has a tendency to do that (and of course I'm using English rather than Unicode symbols, both for convenience of typing and I'd end up translating it to English anyway as an explanation). Essentially our search now contains some undefined bits, most importantly the type U which is the type of the arguments accepted by the function __mul__. We don't just want to make sure that x.__mul__ exists, we also want to make sure that x.__mul__(x) is valid, so we need to know the type of __mul__'s arguments and ensure that our object o is a member of that type.

Now we've nicely defined a type for square, namely T''', using all of the knowledge that we can get from the code without restricting it, but this definition itself is defined by another type U. Of course, we also haven't defined the type "function", but we can do that in a similar way if we know how Python works (a Python function f is an object for which f.__call__ exists), and we can define U if we have the source code for __mul__. Of course, it can become intractable to define types in this way for an entire system, so what we would like is a function which takes in function objects and returns type definitions on its arguments so that we can do this automatically. Of course such a function might never halt, for example if we have a recursive function like:

```python
def recursive(x):
    return recursive(x)
```

Attempting to inspect this in a naïve way would result in endless attempts to find types for both, since it will accept anything and never halt (or in the case of real hardware, the stack depth will be exceeded and the program will exit with an error). Of course, as human programmers we know what this will do, so we could make a more sophisticated system for inspecting our code, which can notice such recursion and tell us that the type of x is just "object" and the return value of recursive(x) is "RuntimeError". This would be necessary to reason about the following perfectly valid Python:

```
def infinity(x):
    return infinity(x)

def buzz_lightyear():
    try:
        infinity(1)
        return "And beyond!"
    except RuntimeError:
        return "Falling with style."
```

The function "buzz_lightyear" tries to run the function "infinity", which breaks when it reaches the maximum recursion depth imposed by the Python interpreter and gives a RuntimeError indicating this. The line 'return "And beyond!"' is never reached, due to this error, and instead we GOTO the "except RuntimeError" line, which tells us what to do if we get a RuntimeError. The line 'return "Falling with style."' is run, so that this function always returns the text string "Falling with style.", and of course it doesn't accept any input. It is possible to reason about this function in a very simple way and figure out that it returns a text string (both of the "return" statements have a text string, regardless of what "infinity" returns). It's slightly harder to work out that it always returns "Falling with style.", but possible. Of course, we could write a nasty bit of code like:

```python
def infinity(x, values):
    x.__class__.getValue = values&#091;0]
    return infinity(x, &#091;values&#091;1], values&#091;0]])

def buzz_lightyear():
    def value1():
        return "Hello world"
    def value2():
        return value2
    try:
        infinity(copyright, &#091;value1, value2])
        return 3
    except RuntimeError:
        return copyright.__class__.getValue()
```

This is a complete bastard, and actually depends heavily on global variables which may have different defaults on different systems, and may be changed during the running of a program. What it does is the following: Define a function called "infinity" which takes two objects, which we call "x" and "values". The first thing we do in this function is change the 'class' of x (the type of object it is) so that its function 'getValue' is the first element of our values argument. We then return the result of running the infinity function on x and a list object containing the first two elements of 'values' in reverse order. In other words, this will run itself over and over, each time swapping the 'getValue' function between those first two elements of 'values'. This will cause a RuntimeException, since it can't recurse infinitely, but we don't know what 'getValue' will be when this happens, just by looking at the code. We then define our function 'buzz_lightyear' which takes no arguments. Inside buzz_lightyear we define two functions, 'value1' and 'value2'. The former takes no arguments and returns a string "Hello world"; the latter simply returns itself as an object (but without running itself recursively). With these in hand we proceed to attempt a call to 'infinity', giving it Python's built-in 'copyright' object and a list object containing our value1 and value2 functions. After running infinity we return the object 3 (which never happens, as a RuntimeError is encountered). What ends up running in the 'except' statement is whatever function the 'getValue' name is pointing to, which is one of value1 or value2, and we return the result. Here there are three possible return types, a number 3 (which never happens), a string "Hello world" or a function value2. Reasoning about such a program in a general way is very tricky, for example the return type of buzz_lightyear could be defined as A where:

```python
forall o of type object, o is a member of A of type P(object) if and only if o is a member of (function unioned with string unioned with number)
```

or as A' where:

```python
forall o of type object, o is a member of A of type P(object) if and only if o is a member of {F, "Hello world"} where F of type P(function)
```

Doing any more than this would be beyond the scope of our code, so we don't really want to take it any further. We can use these types as constraints to work out the types of everything that depends on them, until we've exhausted the information contained directly in the code. The rest we can only work out as the code's running, when we know what exact values these objects have, and thus what type they are. As long as we're always working on lowering the upper bound on the types then we're not imposing any restrictions on the code which were not already present by virtue of Python's semantics. When it comes to runtime the introspection abilities of the language (the ability to know about itself) can be greatly enhanced this way.

Of course, there are other ways of doing type inference which give a lower limit to types, essentially turning pure object functions like our Python "square" function into ones with concrete types, ie. types built up from "atomic types" (or, as I call them, bloody annoying non-object crap). Essentially it takes C-style declarations and puts them in Python, for example <a href="http://www.cosc.canterbury.ac.nz/greg.ewing/python/Pyrex/version/Doc/Manual/basics.html">Pyrex</a>. These allow working with statically/concretely typed C code, but in a backwards way: rather than seeing these restricted C functions as an alternative execution path for Python objects and treating them accordingly (we don't care how it's implemented, just that it works), instead the concrete typing leaks out and we end up hitting artificial walls when we try to program, which are only there due to a leaky implementation (for example, in Python 2.x try to run "hello".foo = 10).

So what's the point of all this? Well firstly, code can only run when we know what type everything has. Treating everything as a generic "object" can slow down execution speed as it becomes very hard to automatically optimise the code. If we knew that something is only ever an integer then we can put number-specific code there to make it much faster, rather than keeping it generic. The simplest way to implement a dynamic language like Python ends up being with an interpreter and a virtual machine, a statically typed program which simulates a computer capable of reading and executing Python line by line***, whilst statically typed languages can instead be compiled (translated) into code which a real computer will understand, and are thus much faster. If we have type information then we can compile Python into machine language and see a massive speed boost whilst remaining duck typed. Since concrete type information can only ever be known at run time, such compilation would need to happen as the program's running: so-called Just In Time compilation.

Current approaches to JIT compiling successfully use type feedback, where a function is run in the regular way in virtual machine, but the specific types it uses are remembered. A compiler then takes the function and the type information and compiles a machine-code version, so that next time the function is run with the same type of input it can use the machine-code version instead. This is nice, but requires all of the work to be done when its needed, so a little push in the right direction from inferred types might reduce this overhead.

The most interesting use, as far as I'm concerned, is with automatically generating specifications. Given enough information about the code it becomes possible to know a lot about what it is capable of doing, without running it. This allows some code to become declarative, meaning that the description of the code specifies exactly what it does, and the code itself just becomes one particular way of implementing that specification. Comparing specifications (which is not possible in the general case, but we can still try opportunistically, eg. with a timeout) allows us to automatically replace the code (implementation) with alternatives that do the same thing. This is useful for building compilers, and if we want to make languages work together (although of course we just shift the problem, since we can have competing, incompatible specification languages).

Another useful feature of this specification-carrying-code has been implemented by Hesam Samimi in an extension to Java, where the programmer can supply a specification for each block of code, and the virtual machine can check if the supplied code actually does implement the specification or not. This makes debugging easier, as long as you can get your specifications right, but interestingly it can be input-dependent. For example, a function has a specification, and when that function is called the code is checked against the specification to see whether it matches, given the input. If it does then great, but if not then an alternative can be used. This makes handling of edge-cases and corner-cases much nicer. Here's a literal example of edge- and corner-case handling code; We define a function which takes four arguments, a "grid", a function called "process" and an x any y coordinate. We want to run "process" on the square in the grid at position (x,y), along with every neighbouring square, and return a list of the results. This function looks like the following:

```
define my_function(grid, process, x, y):
    if x == 0 or x == 99 or y == 0 or y == 99:
        if 0 <p>:
            if y == 0:
                return map(process, &#091;grid&#091;x-1]&#091;y], grid&#091;x-1]&#091;y+1], grid&#091;x]&#091;y], grid&#091;x]&#091;y+1], grid&#091;x+1]&#091;y], grid&#091;x+1]&#091;y+1]])
            else:
                return map(process, &#091;grid&#091;x-1]&#091;y-1], grid&#091;x-1]&#091;y], grid&#091;x]&#091;y-1], grid&#091;x]&#091;y], grid&#091;x+1]&#091;y-1], grid&#091;x+1]&#091;y]])
        elif 0 <p>
            if x == 0:
                return map(process, &#091;grid&#091;x]&#091;y-1], grid&#091;x]&#091;y], grid&#091;x]&#091;y+1], grid&#091;x+1]&#091;y-1], grid&#091;x+1]&#091;y], grid&#091;x+1]&#091;y+1]])
            else:
                return map(process, &#091;grid&#091;x-1]&#091;y-1], grid&#091;x-1]&#091;y], grid&#091;x-1]&#091;y+1], grid&#091;x]&#091;y-1], grid&#091;x]&#091;y], grid&#091;x]&#091;y+1]])
        else:
            if y == 0 and x == 0:
                return map(process, &#091;grid&#091;x]&#091;y], grid&#091;x]&#091;y+1], grid&#091;x+1]&#091;y], grid&#091;x+1]&#091;y+1]])
            elif y == 0 and x == 99:
                return map(process, &#091;grid&#091;x-1]&#091;y], grid&#091;x-1]&#091;y+1], grid&#091;x]&#091;y], grid&#091;x]&#091;y+1]])
            elif y == 99 and x == 0:
                return map(process, &#091;grid&#091;x]&#091;y-1], grid&#091;x]&#091;y], grid&#091;x+1]&#091;y-1], grid&#091;x+1]&#091;y]])
            else:
                return map(process, &#091;grid&#091;x-1]&#091;y-1], grid&#091;x-1]&#091;y], grid&#091;x]&#091;y-1], grid&#091;x]&#091;y]])
        else:
        return map(process, &#091;grid&#091;x-1]&#091;y-1], grid&#091;x-1]&#091;y], grid&#091;x-1]&#091;y+1], grid&#091;x]&#091;y-1], grid&#091;x]&#091;y], grid&#091;x]&#091;y+1], grid&#091;x+1]&#091;y-1], grid&#091;x+1]&#091;y], grid&#091;x+1]&#091;y+1]])
```

This looks pretty messy, but what is it doing? Well it handling the neighbours: if a square is on the edge of the grid (here we say it's on the edge if its coordinate is 0 or 99) then it won't have any neighbours on one side. The first four "return" lines handle the bottom, top, left and right edges respectively. Likewise, if we're in a corner then we have no neighbours on two sides, so the next four "return" lines handle the bottom left, bottom right, top left and top right corners. The bottom line, the final "return" statement, will work for every square which isn't a corner or edge case, ie. one that is surrounded by neighbours. This type of code is really ugly, and very prone to making mistakes like a plus or minus the wrong way around. A lot of my current Physics project requires code like this, except in three dimensions where there are 26 neighbours to each cube, 6 face cases, 12 edge cases and 8 corner cases. Needless to say, I've often spent a whole day trying to track down a single misplaced minus sign. What makes specifications nice is that we can replace this whole lot with the following:


define my_function(grid, process, x, y):
    return map(process, &#091;grid&#091;x-1]&#091;y-1], grid&#091;x-1]&#091;y], grid&#091;x-1]&#091;y+1], grid&#091;x]&#091;y-1], grid&#091;x]&#091;y], grid&#091;x]&#091;y+1], grid&#091;x+1]&#091;y-1], grid&#091;x+1]&#091;y], grid&#091;x+1]&#091;y+1]])
```

Which is just that last line from before. This will work the majority of the time, but if we're on an edge or in a corner then it won't match the required specification and won't be used. In our 2D example there are four corner cases, four edge cases and one regular case. In a relevant 100x100 grid there are 4 corner squares, 392 edge squares and 9604 regular squares. That means, only counting return statements, 11% of our code is handling 96% of the data, 44% of the code is handling 3.9% of the data and 44% of the code is handling 0.04% of the data. The larger the input, the less significant the edges and corners become.

Corners completely handle the 1x1 and 2x2 grids, but then remain constant regardless of grid length. Edges come into play in the 3x3 grid, already equally important as the corners, then increase linearly with the grid length. The regular, non-edge case also starts in the 3x3 grid, but goes up with the square of the grid length, until by the 7x7 grid they are more significant than the edges and corners combined. After that the edge cases aren't worth mentioning.

Not only does getting rid of edge cases make the code far more readable and understandable, but the code becomes simpler for the machine to run. In our case the edge case code does about as much as a results verifier would, so there's probably no speedup achievable here, but if a costly algorithm is used by a function to handle every possibility, where the vast majority of cases can be fulfilled by a quick algorithm, then simplifying it in this way can give speed gains. For example, speeding up the non-edge case of our grid function, for a 100x100 grid is worth 96/4=24 times as much as a speed increase in the edge case code. Thus if we double the speed of the regular case we can get away with an order of magnitude slow-down in the edge case and still get an improvement.

The obvious question becomes: what happens if we get an edge case? If our code comes with specifications then we don't need to care about the edge cases, since an adequate function can be created to handle them automatically by looking through the specifications of everything else and building the required functionality out of the available components. In fact, we don't even need to write the regular function, that can be automatically generated too! Writing the function by hand just becomes an optimisation, with the regular case being the most obvious candidate for optimisation in this example. Since automatically synthesised code can be expensive to produce and inefficient to run, it is a good idea to do such optimisation, but not at all necessary. The specification language becomes the language to program in, and the computer does the rest (as long as sufficient components are written with implementations, or else no code can be generated).

So why am I writing this long and boring post? Well, firstly I am a great believer in the need for correctness proofs: code is just a fancy Mathematical notation, and without a proof of what it does then it's no better than engineering or a social science. Rules of thumb (eg. design patterns) are all well and good for day-to-day tasks, but a rule-of-thumb mindset isn't going to produce anything truly new, truly insightful or truly robust: the second law of Thermodynamics is not "hot stuff tends to cool down and cold stuff tends to heat up", that's a rule of thumb; it is in fact proved from Atomic Theory using Statistical Mechanics, whilst Atomic Theory is proved from Quantum Mechanics and Statistical Mechanics is Statistics and Probability Theory, which also underlies Quantum Theory along with Guage Theory, Topology and so on. Whilst there can never be a complete proof of anything, we can narrow down the number of assumptions we have to make.

* In reality it can store anything, since everything in a computer is the same old binary, but since it will treat what you give it as an integer you'll get corrupted data unless your data behaves exactly like an integer, in which case you may as well use an integer :P Note that a program doesn't particularly care if your data is corrupt, since it will carry on using it, happily working its way down the execution path, all the while fucking up whatever you thought it was meant to be doing because you didn't understand what program you just wrote.

** Functions are actually a bit of a strange type, since we give them things to act on. Python functions are a kind of object, but when we run them we're doing a funny thing called Currying. For example, our "square" function in Python has the type object->object, since we give it an object and get an object back. If run a function which makes a list out of two objects we have a type of object->object->object. We can give it an object, like 2 for example, and it gives back something of the type object->object, which is also a function, which in this case takes an object and gives out an object. If we put in the object 78 to this new function then we would get out a list object &#091;2, 78] (based on the definition we gave). In Python we can't directly Curry functions, ie. we can't turn an object->object->object into an object->object, we can only go straight from one end to the other, but since this is all Maths it doesn't stop us from thinking about it :)

*** Actually the raw Python is turned into bytecode, a much simpler language which is faster to execute, and it is this which is interpreted in the virtual machine.
