---
title: Diet Python and some ranting
---
Long time no post (in fact, I haven't posted at all since starting my fourth
year at Sheffield Uni!). I keep writing big ranty things, meaning to post them,
but haven't got around to it yet. Ah well, maybe soon (exams at the moment
though :( )

As part of my biannual exam revision procrastination I've started to rejig my
Diet Python language. The idea behind Diet Python is pretty simple: take all of
the syntactic sugar out of Python. Thus Diet Python is just a subset of Python,
which is just as powerful as the complete language. Diet Python isn't meant to
be programmed in, however; it exists to simplify Python programs so that they
can be handled more easily by automated tools. For example, in Python the
following two lines are equivalent:

``` python
a + b
a.__add__(b)
```

So the Diet Python equivalent is this:

```python
a.__add__(b)
```

It is thus completely compatible with Python, but there is no need to bother
with "+" (or the associated nodes in the Abstract Syntax Tree). The same applies
to other Python syntax such as:

``` python
my_list[index]
my_list.__getitem__(index)
```

In Diet Python we can get rid of the `[]` subscription notation, without losing
the ability to grab elements from lists (or any other type of object which
implements the `[]` syntax, which is done via the `__getitem__` method). Thus if
we have a Python implementation (CPython, Jython, PyPy, IronPython, etc.) then
it is also a Diet Python implementation (plus some extra stuff that Diet Python
won't use), but more interestingly if we implement Diet Python then we've
actually implemented the whole of Python in terms of features, just not the
syntax. This can be overcome easily by using a translator to turn Python's nice,
sugary syntax into Diet Python's awkward, canonical syntax, which is exactly
what I've done.

Diet Python originally started as a simple test case for my Python pretty
printer (or "decompiler"), which turns a Python Abstract Syntax Tree, produced
by Python 2.x's built-in "compiler" module, into valid Python code which
implements the AST's functionality (ie. compile some Python into an AST, stick
that into the decompiler to get some Python code, then compile that to an AST
and the two ASTs should be the same (as long as every transformation is
reversible, is reversed, doesn't lose information and is done naïvely, that is
;) ).

The decompiler itself was an experiment to get used to the PyMeta pattern
matching framework (which I've since used in a University project to test my
code), and since PyMeta, as an implementation of
[Alessandro Warth's](http://www.tinlizzie.org/~awarth/) OMeta, should be
nicely extensible via subclassing, I wanted both an experiment in PyMeta and an
experiment in extending my experiment in PyMeta to really get to grips with it.

Unfortunately subclassing in PyMeta has proven difficult, which might be a bug
in the implementation (I'll have to check up on that). Making a pattern matcher,
for example to decompile Python ASTs, in PyMeta allows anyone to make a similar
pattern matcher based on it quite easily through Python's object system. For
example if you want to get rid of every "print" statement in some code, you take
the decompiler (which is a Python class object), then you write down the grammar
rules which differ from the original (in this example every Print and Printnl
node should be translated into '' (ie. an empty string, and thus no code)), then
you tell the decompiler to make a grammar out of your rules, and it will give
you a new Python class which implements a pattern matcher using the decompiler's
rules + your new ones (where the new ones override the decompiler's ones in case
of conflicts).

This is all well and good, however the REALLY cool thing about OMeta and thus
PyMeta is that their operation, ie. turning written rules into parsers for those
rules, is written in (O/Py)Meta (which is why they are Meta). Thus it is
possible to take OMeta and, by writing some OMeta rules, change the way that
OMeta works, we could call it OMeta'. Now OMeta' can be changed by writing rules
in either OMeta or OMeta', to produce another pattern matcher which we can call
OMeta'', and so on. This, however, doesn't seem to work in PyMeta, despite
trying multiple ways and looking through the source code (which is written in
OMeta) over and over again. Sad face :(

Ah well, this limitation has resulted in a bit of hackiness when it comes to the
Python decompiler and Diet Python translators. Firstly, PyMeta has no syntax for
comments, which is annoying. It should be simple to subclass PyMeta to make a
PyMeta' which supports comments, but since I can't subclass PyMeta without
losing its bootstrapping, I'm stuck with using Python to remove comments before
passing the rules to PyMeta. The second hack is that doing tree operations
requires recursion. Whilst PyMeta has recursion built in, it's not available in
the most suitable way for my experiments. Once again, subclassing PyMeta should
solve this, but I can't, so I've had to monkey-patch the AST nodes (ie. pollute
their namespaces with functions and attributes) then call these from inside the
grammar. What this results in is every node instantiating their own pattern
matcher on themselves, which happens recursively down the trees.

Unfortunately the "type" system of Python 2.x rears its ugly head here, where
historical implementation decisions have left Python with 2 object hierarchies
(which, I believe, was one of the main motivations for making Python 3). The
object system which is the most familiar, since it's used in Python code, has
the class "object" as the eventual ancestor of everything, such that every class
is a subclass of object, or a subclass of a subclass of object, etc. This would
be a "pure" object system, except that "everything" isn't quite everything. Many
core pieces of Python, for example text strings, are not descendents of "object"
at all, and are not subclasses of anything, or indeed classes. Instead they are
"types", where each "type" seems to be isolated from everything else, written by
hand in C, utterly inextensible, cannot be subclassed, and basically brings to
mind those nightmarish things that Java programmers call "basic types"
(*shudders*, *washes mouth out with soap*). Since they are in their own little
statically-compiled-C world there is no way to monkey patch them with the
required functions and attributes, so that every string, number, None and
probably more require custom code in the pattern matchers. Shit. This also
brings with it that great friend of everybody who loves to waste time known as
combinatorial explosion. In other words, instead of doing a substitution like:

```python
apply_recursively ::= <anything>:a => a.recurse()
```

(`apply_recursively` is defined as taking anything and calling it `a`, then
outputting the value of running `a.recurse()`)

We have to do something like:

```python
apply_recursively ::= <anything>:a ?(not issubclass(a.__class__, Node)) => a
                    | <anything>:a => a.recurse()
```

(`apply_recursively` is defined as taking anything and calling it `a`, as long
as it is not descended from `Node`, and outputting it's value, or else taking
anything and calling it `a` and outputting the value of `a.recurse()`)

And of course, since this is our friend combinatorial explosion, we cannot
write:

```python
apply_recursively ::=<anything>:a1 <anything>:a2 <anything>:a3 <anything>:a4 => a1.recurse() + a2.recurse() + a3.recurse() + a4.recurse()
```

Oh no, if there's the chance that any of those might be "types" (*winces*) then
we are forced to write instead:

```python
apply_recursively ::= <anything>:a1 <anything>:a2 <anything>:a3 <anything>:a4 ?(not (issubclass(a1__class__, Node) or issubclass(a2__class__, Node) or issubclass(a3__class__, Node) or issubclass(a4__class__, Node))) => a1 + a2 + a3 + a4
                    | <anything>:a1 <anything>:a2 <anything>:a3 <anything>:a4 ?(not (issubclass(a1.__class__, Node) or issubclass(a2.__class__, Node) or issubclass(a3.__class__, Node)) and issubclass(a4.__class__, Node)) => a1 + a2 + a3 + a4.recurse()
                    | <anything>:a1 <anything>:a2 <anything>:a3 <anything>:a4 ?(not (issubclass(a1.__class__, Node) or issubclass(a2.__class__, Node) or issubclass(a4.__class__, Node)) and issubclass(a3.__class__, Node)) => a1 + a2 + a3.recurse() + a4
                    | <anything>:a1 <anything>:a2 <anything>:a3 <anything>:a4 ?(not (issubclass(a1.__class__, Node) or issubclass(a2.__class__, Node)) and issubclass(a3.__class__, Node) and issubclass(a4.__class__, Node)) => a1 + a2 + a3.recurse() + a4.recurse()
                    | <anything>:a1 <anything>:a2 <anything>:a3 <anything>:a4 ?(not (issubclass(a1.__class__, Node) or issubclass(a4.__class__, Node) or issubclass(a3.__class__, Node)) and issubclass(a2.__class__, Node)) => a1 + a2.recurse() + a3 + a4
                    | <anything>:a1 <anything>:a2 <anything>:a3 <anything>:a4 ?(not (issubclass(a1.__class__, Node) or issubclass(a3.__class__, Node)) and issubclass(a2.__class__, Node) and issubclass(a4.__class__, Node)) => a1 + a2.recurse() + a3 + a4.recurse()
                    | <anything>:a1 <anything>:a2 <anything>:a3 <anything>:a4 ?(not (issubclass(a1.__class__, Node) or issubclass(a4.__class__, Node)) and issubclass(a3.__class__, Node) and issubclass(a2.__class__, Node)) => a1 + a2.recurse() + a3.recurse() + a4
                    | <anything>:a1 <anything>:a2 <anything>:a3 <anything>:a4 ?(not (issubclass(a1.__class__, Node)) and issubclass(a2.__class__, Node) and issubclass(a3.__class__, Node) and issubclass(a4.__class__, Node)) => a1 + a2.recurse() + a3.recurse() + a4.recurse()
                    | <anything>:a1 <anything>:a2 <anything>:a3 <anything>:a4 ?(not (issubclass(a4.__class__, Node) or issubclass(a2.__class__, Node) or issubclass(a3.__class__, Node)) and issubclass(a1.__class__, Node)) => a1.recurse() + a2 + a3 + a4
                    | <anything>:a1 <anything>:a2 <anything>:a3 <anything>:a4 ?(not (issubclass(a3.__class__, Node) or issubclass(a2.__class__, Node)) and issubclass(a1.__class__, Node) and issubclass(a4.__class__, Node)) => a1.recurse() + a2 + a3 + a4.recurse()
                    | <anything>:a1 <anything>:a2 <anything>:a3 <anything>:a4 ?(not (issubclass(a4.__class__, Node) or issubclass(a2.__class__, Node)) and issubclass(a3.__class__, Node) and issubclass(a1.__class__, Node)) => a1.recurse() + a2 + a3.recurse() + a4
                    | <anything>:a1 <anything>:a2 <anything>:a3 <anything>:a4 ?(not (issubclass(a2.__class__, Node)) and issubclass(a1.__class__, Node) and issubclass(a3.__class__, Node) and issubclass(a4.__class__, Node)) => a1.recurse() + a2 + a3.recurse() + a4.recurse()
                    | <anything>:a1 <anything>:a2 <anything>:a3 <anything>:a4 ?(issubclass(a1.__class__, Node) and issubclass(a2.__class__, Node) and not (issubclass(a3.__class__, Node) or issubclass(a4.__class__, Node))) => a1.recurse() + a2.recurse() + a3 + a4
                    | <anything>:a1 <anything>:a2 <anything>:a3 <anything>:a4 ?(not (issubclass(a3.__class__, Node)) and issubclass(a2.__class__, Node) and issubclass(a1.__class__, Node) and issubclass(a4.__class__, Node)) => a1.recurse() + a2.recurse() + a3 + a4.recurse()
                    | <anything>:a1 <anything>:a2 <anything>:a3 <anything>:a4 ?(issubclass(a1.__class__, Node) and issubclass(a2.__class__, Node) and issubclass(a3.__class__, Node) and not (issubclass(a4.__class__, Node))) => a1.recurse() + a2.recurse() + a3.recurse() + a4
                    | <anything>:a1 <anything>:a2 <anything>:a3 <anything>:a4 ?(issubclass(a1.__class__, Node) or issubclass(a2.__class__, Node) or issubclass(a3.__class__, Node) or issubclass(a4.__class__, Node)) => a1.recurse() + a2.recurse() + a3.recurse() + a4.recurse()
```

Which, even if you've never programmed before, should look like a bloody stupid
hoop to have to jump through.

So, there's a little insight into how even high-level, meta, abstract things can
be hindered by ancient, low-level implementation artifacts, and possibly an
insight into the angry posts I was making to Indenti.ca whilst writing this
stuff six months ago ;)

My code, as always, is in [git](/projects/repos), and now that I've turned the
Diet Python translator into a tree transform it should be much easier to strip
away more and more layers of unnecessary Python (and thus pave the way for some
interesting programming experiments!)
