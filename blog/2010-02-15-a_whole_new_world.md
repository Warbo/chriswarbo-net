---
title: A Whole New World
---
I've found a couple of programming language projects which introduce the concept of "Worlds" as first-class citizens, although in slightly different ways.

In the land of Object Oriented programming, ie. Smalltalk and its derivatives, the concept of worlds is used to encapsulate the state of a program. All computation happens in a world, getting the current world is a simple function call, new worlds can be spawned from other worlds (OO-style inheritance applies to the worlds) and any world can be "committed to", ie. moving the rest of the computation into that world where it carries on. Essentially it gives programs an internal "undo" system, which can be used to try many possible execution paths safely. Some use cases are the following:

```python
We want to do *goal*, and there are X ways we could go about doing it.
We split our computation into X new worlds, all running concurrently.
In each world we attempt a different way of fulfilling *goal*.
Once *goal* is achieved, all worlds except the current are deleted.
The program continues to run in whichever world managed to satisfy *goal* first.
```

This way we guarantee that we reach *goal* using the fastest method (since we use every method), but this is achieved in an inherently distributed way: running multiple, concurrent attempts on one CPU in one machine may often give slower results than arbitrarily choosing one attempt and waiting for it to finish, even if it's not the fastest way possible. However, the nice thing about this use of worlds is that concurrent processing power can be utilised in an easy way without conflicts, and the thought required to understand it is very minimal; there's no scheduling, preempting, breadth-first/depth-first tradeoffs or anything like that. Just try everything and the fastest one will win by virtue of being the fastest, then move on to the next part of the program.

Another simple way of using worlds is to deal with potential errors (ie. exceptions). This is useful since exception handling is notoriously annoying to get right. In many programming languages and paradigms, when something fails (like attempting to contact a remote server for example) then the computation stops and an "exception" gets "thrown"/"raised", which means that whatever asked for that computation to happen is given an "exception" rather than whatever it requested. Receiving an exception is essentially a "GOTO" which jumps to whichever bit of code is assigned to deal with that type of exception (for example there might be SyntaxError, IOError, DivisionByZeroError, etc.). The problem with exceptions is that the catch-all nature of their handlers makes it difficult to determine where the exception came from. For example, we might write (in Python):

```python
import urllib2
import time
page1 = urllib2.urlopen('http://www.identi.ca/warbo')
time.sleep(60)
page2 = urllib2.urlopen('http://www.identi.ca/warbo')
rate = post_difference(page1, page2)
print str(rate)+" posts per minute"
```

This downloads my Identi.ca timeline and gives it the name `page1`, then waits for a minute and does it again for `page2`. Some arbitrary function called `post_difference` is run, presumably to count the difference in Identi.ca posts between `page1` and `page2`, and the result is called `rate`. We then write out a string of `rate` along with units. A problem with this is that the download may fail, and we haven't given any code to handle this, which means the program will exit with an error if this happens. To prevent this we can do:

``` python
import urllib2
import time
try:
  page1 = urllib2.urlopen('http://www.identi.ca/warbo')
  time.sleep(60)
  page2 = urllib2.urlopen('http://www.identi.ca/warbo')
  rate = post_difference(page1, page2)
  print str(rate)+" posts per minute"
except:
  del page1
  del page2
  del rate
```

Now if we get any type of exception in the three lines following `try:` then execution will jump to the `except:` block, otherwise the `except:` block will just be skipped. Since having one page is useless without the other, and keeping such useless objects accessible prevents the garbage collector from freeing up the memory they take up, we use this exception block to simply delete the `page1`, `page2` and `rate` objects. Unfortunately there's actually a problem with this code too! Since we delete `page1`, `page2` and `rate`, and the exception might happen before those objects are defined (for example if an exception occurs whilst trying to download `page1`, then the download of `page2` and calculation of `rate` will never occur), then we might be asking to delete things which don't exist, which causes an exception! The example I've chosen here is meant to be slightly pathological, since the two dangerous operations we're doing (downloading my Identi.ca timeline) are exactly the same apart from binding name and line number. This means we can't, for example, have one `except:` block to deal with network errors and another to deal with some other exception type, since they both have the same exception possibilities. There are various ways of doing this correctly (using multiple `try:`/`except:` blocks, using temporary booleans to store whether each bit succeeded or not, etc.), but none of them are as intuitive as the way shown above. Using worlds, however, makes things easy, since we can encapsulate all of the side-effects of our attempts in a child world and simply delete the world if we fail, ie. something like the following (although I've made up the syntax since it doesn't exist for Python ;):

```python
import urllib2
import time
child_world = get_world().spawn()
use_world(child_world)
try:
  page1 = urllib2.urlopen('http://www.identi.ca/warbo')
  time.sleep(60)
  page2 = urllib2.urlopen('http://www.identi.ca/warbo')
  rate = post_difference(page1, page2)
  print str(rate)+" posts per minute"
  child_world.commit()
except:
  del child_world
```

Here we know that every side-effect (including the definition of new objects) will be contained in `child_world`, so if we fail we can simply delete it. Of course, there's nothing to stop a language using worlds directly for its exception handling (say by destroying world after world until one is reached that handles the exception, rather than just destroying stack frames), but Python couldn't do this as it would break its behaviour and thus existing code.

Another suggestion for using worlds is for *namespaces* and module systems. In Python, code from other files is made accessible to a program by using a line like:

```python
import xyz
```

Which makes the contents of the module `xyz` available to the program, so that you can run, say:

```python
import xyz
xyz.some_function(a, b, xyz.something)
```

The `xyz` module has its own *namespace*, so that everything it contains is only accessible if you put `xyz.` before the name (and namespaces can be nested, so we could say `xyz.abc.something.foo()` ). It's similar to a Web site address but using full stops instead of slashes. This makes sure that whatever `xyz` contains, it won't mess up your program, since you can call something `a` and `xyz` can call something `a`, but they won't conflict in your program since your `a` is called `a` and `xyz`'s `a` is called `xyz.a`. There is also *namespace injection*, which takes things from a module and puts them in the current namespace, for example:

```python
from xyz import a, b, some_function
```

Now those three things we've imported will be available without the `xyz.` in front of them. However, this is acceptable since we've explicitly defined which bits of `xyz` we want, so it should be obvious to us if this will cause any conflicts without having to know what's in `xyz`. However, in Python there is a dangerous and frowned-upon wildcard for namespace injection, which looks like:

```python
from xyz import *
```

This will make everything from `xyz` available in your program without having to put `xyz.` before it. Since this line doesn't specify any names, the only way we could know what this is going to do would be to manually read the code in `xyz` or do:

```python
import xyz
dir(xyz)
```

But this would still only work for that particular version of `xyz` you're using. Other people might be using different versions, and future updates might change which names it uses internally. This is often called* polluting the namespace*, since you end up with crap that gets in the way.

By defining modules in their own worlds, we essentially get the advantage of namespaces (ie. worlds can be used as a namespace implementation), plus we can make and destroy namespaces at any time. For example:

```python
import xyz
abc = xyz.some_function(12)
g = abc+2
switch_world(get_world().spawn_world())
import abc
abc.some_other_function(g)
destroy_world()
print str(abc)
```

The final line will display the value we called `abc` in the second line, regardless of what importing a module called `abc` did to mess up our namespace later on.

Functionally

In the world of functional programming, there is no need to encapsulate side-effects, since all purely functional programs are side-effect free (ie. if something is true somewhere, then it is true everywhere, like regular Maths notation). The concept of worlds here is slightly different, since worlds are passed around as arguments to functions rather than acting like a stack, but the idea of encapsulating state remains the same.

In a functional language a function, let's call it `increment_answer`, can be given another function as an argument. This is known as a *higher-order function*, and as far as the language is concerned, `increment_answer` is a perfectly valid function just waiting to act on something. To put some tangible code for this, we can say:

```python
square(x) ::= x*x
increment(x) ::= x+1
increment_answer(f, x) ::= increment(f(x))
square_plus_one(x) ::= increment_answer(square, x)
```

Even though I've made up this syntax, I'm sure it is pretty straightforward. The interesting thing to note, however, is that at no point in the above is `x` ever defined. We can assume that `*` and `+` are defined somewhere in this made-up language, but `x` only acts as a placeholder; it can stand for anything and the definitions given above always be valid, no matter what the functions do. So far so good, and if you've used a language like Ruby or Python you probably know of higher order functions already. So let's spice things up a bit:

```python
max(x, y) ::= x if x > y else y
allow_positive(x) ::= max(0, x)
```

This probably seems straightforward too: `max` will give the larger of its two arguments whilst `allow_positive` will return its argument if it's positive, or else zero. Here we've been implying numbers, however. So let's look at what happens if we give everything a type:

```python
square(number x) ::= x*x
type(square) = number -> number
increment(number x) ::= x+1
type(increment) = number -> number
increment_answer(number -> number f, number x) ::= increment(f(x))
type(increment_answer) = number -> number -> number -> number
max(number x, number y) ::= x if x > y else y
type(max) = number -> number -> number
allow_positive(number x) ::= max(0, x)
type(allow_positive) = number -> number
```

The type notation I've used means that the left-hand-side is the input and the right-hand-side is the output, so that the type `number -> number` is a function that, when given a `number`, will return a `number`. Easy, yes? Well what about the type of `increment_answer`? It has two arguments, the first is a function which must accept a `number` (since we pass it `x`) and return a `number` (since `increment` takes a `number`), so the type of its first argument must be a function `number -> number`. Its second argument is just a `number`. Giving it both of these will return a `number`, so its type is `number -> number -> number -> number`. That may seem a little confusing (where does "left" end and "right" begin?), so to consider a slightly simpler, non-higher order example let's take a look at `max`.

There are two arrows in the type of `max`, and it's not completely clear that both of the left ones are the inputs `x` and `y`. Why have I made the confusing mistake of putting `number -> number -> number` instead of something like `(number -> number) -> number`? Well it's because of *Currying*. The type of `max` is `number -> number -> number`, with the left as input and the right as output, as I said earlier, but attempting to segregate arguments and returns, for example with brackets, is futile: thanks to Currying it's completely up to you what you consider to be the left and right! The obvious type from the function definition would be two `number`s in to get one `number` out, but it's also valid to say that it takes a single `number` in and gives out a `number -> number`. But what the hell kind of return value is that? Well, if you look at the other types I've scattered around there, it should be obvious that `number -> number` is a function which takes a `number` and returns a `number`. So `max(5)` is a perfectly valid function call, and will return a function for us! So what does such a function do? In this case it will give the larger of its argument or 5, so we can say:

```python
five_or_above ::= max(5)
type(five_or_above) = number -> number
five_or_above(12) = 12
five_or_above(3) = 5
type(10) = number
type(five_or_above(10)) = number
```

It basically acts like `max` but with the first argument 'filled in' with a 5. This actually allows us to use a simpler definition of our `allow_positive` function, since we can say:

```python
allow_positive ::= max(0)
type(allow_positive) = number -> number
```

This will behave in exactly the same way as the previous definition.

What about the other way around? What happens if we give `max` an argument of `number -> number` (ie. a function from `number`s to `number`s)? Do we get the remaining, right-most `number` as a return? Not quite. It's perfectly valid to call `max` with a function, for example:

```python
max(number x, number y) ::= x if x > y else y
type(max) = number -> number -> number
increment(number x) ::= x+1
type(increment) = number -> number
result ::= max(increment)
type(result) = number -> number
```

Hmm, we seem to have received a function back rather than a `number`. Why's that? Well it's because the function we gave to `max`, namely `increment` (for lack of a better example), requires an argument in order to do anything. Calling `max` of `increment` only makes sense when we give `increment` something to add one to! Thus `max(increment)` gives us a function which can be thought of as follows:

```python
max(increment) = x if x > x+1 else x+1
type(max(increment)) = number -> number
```

In other words it takes a `number` in for the first argument of `max`, namely `x`, then sends that to `increment` to find out what to use for `y` (which will be `x+1`, since that's what `increment` returns), then runs `max` of `x` and `y`, ie. of `x` and `x+1` (which incidentally will always behave like `increment` because I used silly examples, as long as `>` is defined as we would expect of course ;) ). I know that's quite long-winded, but re-read it a few times and you'll soon pick it up!

So what of the beast `increment_answer` with its type `number -> number -> number -> number`? Well it's the same thing again: we can give it a single `number` to get a function `number -> number -> number` (the same type as `max`) which essentially has the first argument 'already filled in'. We can give it two `number`s to get a function `number -> number` which behaves as if the first two arguments are 'already filled in', or we could give it a function `number -> number` which will 'fill in' the first argument as required and fill in the second argument with that function applied to the first. We can keep doing this, replacing values with functions and functions with values, making for a program that's applicable to a very general set of problems. As you might be thinking, Currying is a very powerful technique indeed! It's one of the reasons people get so enthused about functional programming and languages like Haskell.

OK, so that's a little functional programming basics, let's move on to more interesting things than numbers! Our programs can contain functions that do anything we want, and our entire program itself is just a function as well. So what type do programs have? `void -> void`. This is a rather rubbish type, which basically means they take in nothing and return nothing. What makes this especially worse is that a large number of programs spend a lot of time initialising themselves to start with, and backing up their calculations before they close.

The functional idea of worlds is to make programs have the type `world -> world`, which gives them an input which they can get started straight away with, rather than having to faff around with loading and parsing and such (which is made all the more difficult due to the stateless nature of the program, which is somehow accomplished by monads even though I still can't get my head around what they do), and at the end they can dump their computations straight out to the operating system for storage, rather than having to mess with IO, and all of the complexities, edge cases and breakages that entails.

Thus in this system a program to update a clock on the desktop can be as simple as:

```python
update_clock(world w) ::= new_world_from_existing(w, "clock", w.clock+1)
```

This is my own made-up syntax, but the the point is that it takes in a world, whatever that happens to be, and outputs a new world which is the same as the input except that its "clock" is '1 higher'. The point is that this is an entirely self-contained program (given the function `new_world_from_existing`, which I only abstracted away here since I can't be arsed to specify a structure for these worlds, since it's arbitrary); the OS gives this program a world when it starts and saves the new world it passes back when it's done.

As far as the OS is concerned the worlds are just black boxes, it doesn't need to know or care what they are. When an event occurs, an OS daemon will trigger the associated programs (imagine something like Unix's cron), and these can of course trigger actions themselves (for example a clock tick can trigger a time update program, an updated time can trigger a program to render the new clock face image and having a new clock face image can trigger a screen refresh). Essentially it allows unrolling of main loops into one OS-level mechanism, and allows the programs to be short, simple, cooperative and to-the-point.

A difference between the stateful and functional world systems is that in the former, computation happens *inside* worlds, which capture all of the side-effects to prevent them wreaking havoc. In the latter, computation happens *to* worlds and new worlds are generated based on the effects of the computation; worlds are immutable and unchanging, but you can make new ones with whatever properties you like (worlds never get "replaced", although it is valid for the operating system to pass a world to a program and store the new world that is returned in the same place as the old world, as long as no other programs use the old world, but this isn't so much mutability or replacement as much as it is garbage collection; the old world is not needed any more so it is deleted, whilst some space is required for the new world which may as well be the space previously occupied by the old world).

However, the observant amongst you will have spotted by now where I'm going with this. If you've not spotted it yet then it's that these two concepts of *Worlds* are the same! The advantage to using worlds in an imperative, stateful language is that side-effects can be tamed, kept isolated and the APIs kept pure and side-effect free to allow more scalability and predictability. With everything wrapped up in *world* sandboxes, assertions about behaviour can be made which only rely upon the individual pieces of code and the little world in which they live. In other words, this stateful code behaves like pure functions which are Curried with the worlds they return!

In a functional language, worlds allow state to be passed around, so that the same small, simple functions can be used for a variety of behaviours based on the world that they take as input. In other words these pure functions behave like imperative code, just with the state being passed around explicitly!

So what does this mean? Well firstly, if we can make pure, side-effect-free imperative code then we get the awesomeness of functional code in our non-functional languages. This, admittedly, would only occur in a coarse-grained way, since there's no guarantee that anything below the API level behaves purely, but it's enough to let us build non-leaky abstractions in the way that functional languages get for free. If we have correct abstractions then we can treat them as black boxes and do all sorts of cool analysis on them, like automatic translation, optimisation, compilation, program synthesis, inter-language calls, etc.

Some of this is going on in the Viewpoints Research Institute's *STEPS* project, with their *kernel abstractions*, essentially treating function calls like virtual machines, implemented as on-the-fly domain-specific language compilation in separate worlds, which makes each layer in the call stack become more sandboxed, more secure, more abstract, simpler and more specialised at its job. It's an interesting project with some cool papers floating around (although I wish they'd release their publications more often ;) )

Anyhoo, I thought I'd write a blog entry to test out KDE's Blogilo program, since Google's fail Web app POS doesn't like me, and I'm getting rather carried away with it :) Hope this works!
