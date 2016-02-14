---
title: A Whole New World
---
I've found a couple of programming language projects which introduce the concept of "Worlds" as first-class citizens, although in slightly different ways.

In the land of Object Oriented programming, ie. Smalltalk and its derivatives, the concept of worlds is used to encapsulate the state of a program. All computation happens in a world, getting the current world is a simple function call, new worlds can be spawned from other worlds (OO-style inheritance applies to the worlds) and any world can be "committed to", ie. moving the rest of the computation into that world where it carries on. Essentially it gives programs an internal "undo" system, which can be used to try many possible execution paths safely. Some use cases are the following:

<code>We want to do <em>goal</em>, and there are X ways we could go about doing it.</code>

<code>We split our computation into X new worlds, all running concurrently.</code>

<code>In each world we attempt a different way of fulfilling <em>goal</em>.</code>

<code>Once <em>goal</em> is achieved, all worlds except the current are deleted.</code>

<code>The program continues to run in whichever world managed to satisfy <em>goal</em> first.</code>

This way we guarantee that we reach <em>goal</em> using the fastest method (since we use every method), but this is achieved in an inherently distributed way: running multiple, concurrent attempts on one CPU in one machine may often give slower results than arbitrarily choosing one attempt and waiting for it to finish, even if it's not the fastest way possible. However, the nice thing about this use of worlds is that concurrent processing power can be utilised in an easy way without conflicts, and the thought required to understand it is very minimal; there's no scheduling, preempting, breadth-first/depth-first tradeoffs or anything like that. Just try everything and the fastest one will win by virtue of being the fastest, then move on to the next part of the program.

Another simple way of using worlds is to deal with potential errors (ie. exceptions). This is useful since exception handling is notoriously annoying to get right. In many programming languages and paradigms, when something fails (like attempting to contact a remote server for example) then the computation stops and an "exception" gets "thrown"/"raised", which means that whatever asked for that computation to happen is given an "exception" rather than whatever it requested. Receiving an exception is essentially a "GOTO" which jumps to whichever bit of code is assigned to deal with that type of exception (for example there might be SyntaxError, IOError, DivisionByZeroError, etc.). The problem with exceptions is that the catch-all nature of their handlers makes it difficult to determine where the exception came from. For example, we might write (in Python):

<code>import urllib2</code>

<code>import time</code>

<code>page1 = urllib2.urlopen('http://www.identi.ca/warbo')</code>

<code>time.sleep(60)</code>

<code>page2 = urllib2.urlopen('http://www.identi.ca/warbo')</code>

<code>rate = post_difference(page1, page2)</code>

<code>print str(rate)+" posts per minute"</code>

This downloads my Identi.ca timeline and gives it the name <code>page1</code>, then waits for a minute and does it again for <code>page2</code>. Some arbitrary function called <code>post_difference</code> is run, presumably to count the difference in Identi.ca posts between <code>page1</code> and <code>page2</code>, and the result is called <code>rate</code>. We then write out a string of <code>rate</code> along with units. A problem with this is that the download may fail, and we haven't given any code to handle this, which means the program will exit with an error if this happens. To prevent this we can do:

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

Now if we get any type of exception in the three lines following <code>try:</code> then execution will jump to the <code>except:</code> block, otherwise the <code>except:</code> block will just be skipped. Since having one page is useless without the other, and keeping such useless objects accessible prevents the garbage collector from freeing up the memory they take up, we use this exception block to simply delete the <code>page1</code>, <code>page2</code> and <code>rate</code> objects. Unfortunately there's actually a problem with this code too! Since we delete <code>page1</code>, <code>page2</code> and <code>rate</code>, and the exception might happen before those objects are defined (for example if an exception occurs whilst trying to download <code>page1</code>, then the download of <code>page2</code> and calculation of <code>rate</code> will never occur), then we might be asking to delete things which don't exist, which causes an exception! The example I've chosen here is meant to be slightly pathological, since the two dangerous operations we're doing (downloading my Identi.ca timeline) are exactly the same apart from binding name and line number. This means we can't, for example, have one <code>except:</code> block to deal with network errors and another to deal with some other exception type, since they both have the same exception possibilities. There are various ways of doing this correctly (using multiple <code>try:</code>/<code>except:</code> blocks, using temporary booleans to store whether each bit succeeded or not, etc.), but none of them are as intuitive as the way shown above. Using worlds, however, makes things easy, since we can encapsulate all of the side-effects of our attempts in a child world and simply delete the world if we fail, ie. something like the following (although I've made up the syntax since it doesn't exist for Python ;):

<code>import urllib2</code>

<code>import time</code>

<code>child_world = get_world().spawn()</code>

<code>use_world(child_world)</code>

<code>try:</code>

<code>  page1 = urllib2.urlopen('http://www.identi.ca/warbo')</code>

<code>  time.sleep(60)</code>

<code>  page2 = urllib2.urlopen('http://www.identi.ca/warbo')</code>

<code>  rate = post_difference(page1, page2)</code>

<code>  print str(rate)+" posts per minute"</code>

<code>  child_world.commit()</code>

<code>except:</code>

<code>  del child_world</code>

Here we know that every side-effect (including the definition of new objects) will be contained in <code>child_world</code>, so if we fail we can simply delete it. Of course, there's nothing to stop a language using worlds directly for its exception handling (say by destroying world after world until one is reached that handles the exception, rather than just destroying stack frames), but Python couldn't do this as it would break its behaviour and thus existing code.

Another suggestion for using worlds is for <em>namespaces</em> and module systems. In Python, code from other files is made accessible to a program by using a line like:

<code>import xyz</code>

Which makes the contents of the module <code>xyz</code> available to the program, so that you can run, say:

<code>import xyz</code>

<code>xyz.some_function(a, b, xyz.something)</code>

The <code>xyz</code> module has its own <em>namespace</em>, so that everything it contains is only accessible if you put <code>xyz.</code> before the name (and namespaces can be nested, so we could say <code>xyz.abc.something.foo()</code> ). It's similar to a Web site address but using full stops instead of slashes. This makes sure that whatever <code>xyz</code> contains, it won't mess up your program, since you can call something <code>a</code> and <code>xyz</code> can call something <code>a</code>, but they won't conflict in your program since your <code>a</code> is called <code>a</code> and <code>xyz</code>'s <code>a</code> is called <code>xyz.a</code>. There is also <em>namespace injection</em>, which takes things from a module and puts them in the current namespace, for example:

<code>from xyz import a, b, some_function</code>

Now those three things we've imported will be available without the <code>xyz.</code> in front of them. However, this is acceptable since we've explicitly defined which bits of <code>xyz</code> we want, so it should be obvious to us if this will cause any conflicts without having to know what's in <code>xyz</code>. However, in Python there is a dangerous and frowned-upon wildcard for namespace injection, which looks like:

<code>from xyz import *</code>

This will make everything from <code>xyz</code> available in your program without having to put <code>xyz.</code> before it. Since this line doesn't specify any names, the only way we could know what this is going to do would be to manually read the code in <code>xyz</code> or do:

<code>import xyz</code>

<code>dir(xyz)</code>

But this would still only work for that particular version of <code>xyz</code> you're using. Other people might be using different versions, and future updates might change which names it uses internally. This is often called<em> polluting the namespace</em>, since you end up with crap that gets in the way.

By defining modules in their own worlds, we essentially get the advantage of namespaces (ie. worlds can be used as a namespace implementation), plus we can make and destroy namespaces at any time. For example:

<code>import xyz</code>

<code>abc = xyz.some_function(12)</code>

<code>g = abc+2</code>

<code>switch_world(get_world().spawn_world())</code>

<code>import abc</code>

<code>abc.some_other_function(g)</code>

<code>destroy_world()</code>

<code>print str(abc)</code>

The final line will display the value we called <code>abc</code> in the second line, regardless of what importing a module called <code>abc</code> did to mess up our namespace later on.

Functionally

In the world of functional programming, there is no need to encapsulate side-effects, since all purely functional programs are side-effect free (ie. if something is true somewhere, then it is true everywhere, like regular Maths notation). The concept of worlds here is slightly different, since worlds are passed around as arguments to functions rather than acting like a stack, but the idea of encapsulating state remains the same.

In a functional language a function, let's call it <code>increment_answer</code>, can be given another function as an argument. This is known as a <em>higher-order function</em>, and as far as the language is concerned, <code>increment_answer</code> is a perfectly valid function just waiting to act on something. To put some tangible code for this, we can say:

<code>square(x) ::= x*x</code>

<code>increment(x) ::= x+1</code>

<code>increment_answer(f, x) ::= increment(f(x))</code>

<code>square_plus_one(x) ::= increment_answer(square, x)</code>

Even though I've made up this syntax, I'm sure it is pretty straightforward. The interesting thing to note, however, is that at no point in the above is <code>x</code> ever defined. We can assume that <code>*</code> and <code>+</code> are defined somewhere in this made-up language, but <code>x</code> only acts as a placeholder; it can stand for anything and the definitions given above always be valid, no matter what the functions do. So far so good, and if you've used a language like Ruby or Python you probably know of higher order functions already. So let's spice things up a bit:

<code>max(x, y) ::= x if x &gt; y else y</code>

<code>allow_positive(x) ::= max(0, x)</code>

This probably seems straightforward too: <code>max</code> will give the larger of its two arguments whilst <code>allow_positive</code> will return its argument if it's positive, or else zero. Here we've been implying numbers, however. So let's look at what happens if we give everything a type:

<code>square(number x) ::= x*x</code>

<code>type(square) = number -&gt; number</code>

<code>increment(number x) ::= x+1</code>

<code>type(increment) = number -&gt; number</code>

<code>increment_answer(number -&gt; number f, number x) ::= increment(f(x))</code>

<code>type(increment_answer) = number -&gt; number -&gt; number -&gt; number</code>

<code>max(number x, number y) ::= x if x &gt; y else y</code>

<code>type(max) = number -&gt; number -&gt; number</code>

<code>allow_positive(number x) ::= max(0, x)</code>

<code>type(allow_positive) = number -&gt; number</code>

The type notation I've used means that the left-hand-side is the input and the right-hand-side is the output, so that the type <code>number -&gt; number</code> is a function that, when given a <code>number</code>, will return a <code>number</code>. Easy, yes? Well what about the type of <code>increment_answer</code>? It has two arguments, the first is a function which must accept a <code>number</code> (since we pass it <code>x</code>) and return a <code>number</code> (since <code>increment</code> takes a <code>number</code>), so the type of its first argument must be a function <code>number -&gt; number</code>. Its second argument is just a <code>number</code>. Giving it both of these will return a <code>number</code>, so its type is <code>number -&gt; number -&gt; number -&gt; number</code>. That may seem a little confusing (where does "left" end and "right" begin?), so to consider a slightly simpler, non-higher order example let's take a look at <code>max</code>.

There are two arrows in the type of <code>max</code>, and it's not completely clear that both of the left ones are the inputs <code>x</code> and <code>y</code>. Why have I made the confusing mistake of putting <code>number -&gt; number -&gt; number</code> instead of something like <code>(number -&gt; number) -&gt; number</code>? Well it's because of <em>Currying</em>. The type of <code>max</code> is <code>number -&gt; number -&gt; number</code>, with the left as input and the right as output, as I said earlier, but attempting to segregate arguments and returns, for example with brackets, is futile: thanks to Currying it's completely up to you what you consider to be the left and right! The obvious type from the function definition would be two <code>number</code>s in to get one <code>number</code> out, but it's also valid to say that it takes a single <code>number</code> in and gives out a <code>number -&gt; number</code>. But what the hell kind of return value is that? Well, if you look at the other types I've scattered around there, it should be obvious that <code>number -&gt; number</code> is a function which takes a <code>number</code> and returns a <code>number</code>. So <code>max(5)</code> is a perfectly valid function call, and will return a function for us! So what does such a function do? In this case it will give the larger of its argument or 5, so we can say:

<code>five_or_above ::= max(5)</code>

<code>type(five_or_above) = number -&gt; number</code>

<code>five_or_above(12) = 12</code>

<code>five_or_above(3) = 5</code>

<code>type(10) = number</code>

<code>type(five_or_above(10)) = number</code>

It basically acts like <code>max</code> but with the first argument 'filled in' with a 5. This actually allows us to use a simpler definition of our <code>allow_positive</code> function, since we can say:

<code>allow_positive ::= max(0)</code>

<code>type(allow_positive) = number -&gt; number</code>

This will behave in exactly the same way as the previous definition.

What about the other way around? What happens if we give <code>max</code> an argument of <code>number -&gt; number</code> (ie. a function from <code>number</code>s to <code>number</code>s)? Do we get the remaining, right-most <code>number</code> as a return? Not quite. It's perfectly valid to call <code>max</code> with a function, for example:

<code>max(number x, number y) ::= x if x &gt; y else y</code>

<code>type(max) = number -&gt; number -&gt; number</code>

<code>increment(number x) ::= x+1</code>

<code>type(increment) = number -&gt; number</code>

<code>result ::= max(increment)</code>

<code>type(result) = number -&gt; number</code>

Hmm, we seem to have received a function back rather than a <code>number</code>. Why's that? Well it's because the function we gave to <code>max</code>, namely <code>increment</code> (for lack of a better example), requires an argument in order to do anything. Calling <code>max</code> of <code>increment</code> only makes sense when we give <code>increment</code> something to add one to! Thus <code>max(increment)</code> gives us a function which can be thought of as follows:

<code>max(increment) = x if x &gt; x+1 else x+1</code>

<code>type(max(increment)) = number -&gt; number</code>

In other words it takes a <code>number</code> in for the first argument of <code>max</code>, namely <code>x</code>, then sends that to <code>increment</code> to find out what to use for <code>y</code> (which will be <code>x+1</code>, since that's what <code>increment</code> returns), then runs <code>max</code> of <code>x</code> and <code>y</code>, ie. of <code>x</code> and <code>x+1</code> (which incidentally will always behave like <code>increment</code> because I used silly examples, as long as <code>&gt;</code> is defined as we would expect of course ;) ). I know that's quite long-winded, but re-read it a few times and you'll soon pick it up!

So what of the beast <code>increment_answer</code> with its type <code>number -&gt; number -&gt; number -&gt; number</code>? Well it's the same thing again: we can give it a single <code>number</code> to get a function <code>number -&gt; number -&gt; number</code> (the same type as <code>max</code>) which essentially has the first argument 'already filled in'. We can give it two <code>number</code>s to get a function <code>number -&gt; number</code> which behaves as if the first two arguments are 'already filled in', or we could give it a function <code>number -&gt; number</code> which will 'fill in' the first argument as required and fill in the second argument with that function applied to the first. We can keep doing this, replacing values with functions and functions with values, making for a program that's applicable to a very general set of problems. As you might be thinking, Currying is a very powerful technique indeed! It's one of the reasons people get so enthused about functional programming and languages like Haskell.

OK, so that's a little functional programming basics, let's move on to more interesting things than numbers! Our programs can contain functions that do anything we want, and our entire program itself is just a function as well. So what type do programs have? <code>void -&gt; void</code>. This is a rather rubbish type, which basically means they take in nothing and return nothing. What makes this especially worse is that a large number of programs spend a lot of time initialising themselves to start with, and backing up their calculations before they close.

The functional idea of worlds is to make programs have the type <code>world -&gt; world</code>, which gives them an input which they can get started straight away with, rather than having to faff around with loading and parsing and such (which is made all the more difficult due to the stateless nature of the program, which is somehow accomplished by monads even though I still can't get my head around what they do), and at the end they can dump their computations straight out to the operating system for storage, rather than having to mess with IO, and all of the complexities, edge cases and breakages that entails.

Thus in this system a program to update a clock on the desktop can be as simple as:

<code>update_clock(world w) ::= new_world_from_existing(w, "clock", w.clock+1)</code>

This is my own made-up syntax, but the the point is that it takes in a world, whatever that happens to be, and outputs a new world which is the same as the input except that its "clock" is '1 higher'. The point is that this is an entirely self-contained program (given the function <code>new_world_from_existing</code>, which I only abstracted away here since I can't be arsed to specify a structure for these worlds, since it's arbitrary); the OS gives this program a world when it starts and saves the new world it passes back when it's done.

As far as the OS is concerned the worlds are just black boxes, it doesn't need to know or care what they are. When an event occurs, an OS daemon will trigger the associated programs (imagine something like Unix's cron), and these can of course trigger actions themselves (for example a clock tick can trigger a time update program, an updated time can trigger a program to render the new clock face image and having a new clock face image can trigger a screen refresh). Essentially it allows unrolling of main loops into one OS-level mechanism, and allows the programs to be short, simple, cooperative and to-the-point.

A difference between the stateful and functional world systems is that in the former, computation happens <em>inside</em> worlds, which capture all of the side-effects to prevent them wreaking havoc. In the latter, computation happens <em>to</em> worlds and new worlds are generated based on the effects of the computation; worlds are immutable and unchanging, but you can make new ones with whatever properties you like (worlds never get "replaced", although it is valid for the operating system to pass a world to a program and store the new world that is returned in the same place as the old world, as long as no other programs use the old world, but this isn't so much mutability or replacement as much as it is garbage collection; the old world is not needed any more so it is deleted, whilst some space is required for the new world which may as well be the space previously occupied by the old world).

However, the observant amongst you will have spotted by now where I'm going with this. If you've not spotted it yet then it's that these two concepts of <em>Worlds</em> are the same! The advantage to using worlds in an imperative, stateful language is that side-effects can be tamed, kept isolated and the APIs kept pure and side-effect free to allow more scalability and predictability. With everything wrapped up in <em>world</em> sandboxes, assertions about behaviour can be made which only rely upon the individual pieces of code and the little world in which they live. In other words, this stateful code behaves like pure functions which are Curried with the worlds they return!

In a functional language, worlds allow state to be passed around, so that the same small, simple functions can be used for a variety of behaviours based on the world that they take as input. In other words these pure functions behave like imperative code, just with the state being passed around explicitly!

So what does this mean? Well firstly, if we can make pure, side-effect-free imperative code then we get the awesomeness of functional code in our non-functional languages. This, admittedly, would only occur in a coarse-grained way, since there's no guarantee that anything below the API level behaves purely, but it's enough to let us build non-leaky abstractions in the way that functional languages get for free. If we have correct abstractions then we can treat them as black boxes and do all sorts of cool analysis on them, like automatic translation, optimisation, compilation, program synthesis, inter-language calls, etc.

Some of this is going on in the Viewpoints Research Institute's <em>STEPS</em> project, with their <em>kernel abstractions</em>, essentially treating function calls like virtual machines, implemented as on-the-fly domain-specific language compilation in separate worlds, which makes each layer in the call stack become more sandboxed, more secure, more abstract, simpler and more specialised at its job. It's an interesting project with some cool papers floating around (although I wish they'd release their publications more often ;) )

Anyhoo, I thought I'd write a blog entry to test out KDE's Blogilo program, since Google's fail Web app POS doesn't like me, and I'm getting rather carried away with it :) Hope this works!
