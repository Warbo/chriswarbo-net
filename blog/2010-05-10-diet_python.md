---
title: Diet Python
---

I've talked about this before, but have been doing some more work on Diet Python (available in my [Python Decompiler](/git/python-decompiler) ).

Diet Python is a sub-set of Python which is just as expressive. The PyPy project, for example, uses a subset of Python called RPython which is restricted to exclude the really dynamic stuff, and Google's Go language does a similar thing. They exist to keep handy things found in Python, eg. the garbage collection, but throw away bits that are hard to implement in machine code.

That's not the goal of Diet Python. In Diet Python we don't really care about readability or programmer-friendliness, and the syntax can be as awkward as we like as long as it's still valid Python. What Diet Python does is throw away the redundant bits of Python that can be replaced with other Python that does exactly the same thing.

As a simple example, `a + b` does the same thing as `a.__add__(b)`, but if we're making a Python interpreter we have to handle both `+` and `.__add__`. Diet Python throws away the `+`, since it's less general than the function call, so that Diet Python implementations only have to care about `.__add__`, which they would have to handle anyway since it's a function call and function calls are everywhere in Python. Diet Python has a 'compiler' which translates Python into Diet Python, so that we can write normal Python (eg. `a + b`) and then strip away the fat to get the `__add__`  equivalent. This is "Diet Python", but since it's also perfectly valid Python it will still run fine in a standard Python interpreter.

There are two approaches I'm taking with Diet Python. The first is to remain 100% compatible, so that there is no change between the semantics of the Python input and the Diet Python output, and both will run the same in a standard Python interpreter. The other goal is to see just how much of Python we can throw away, without losing any functionality. Some of this relies on incompatibilities with the standard CPython interpreter, but only due to limitations in the interpreter.

For example, in Python we can do conditional branches like this:

``` python
if a:
  print b
else:
  print c
```

Whilst in Smalltalk the same thing is done with a message send (function call) like:

``` smalltalk
a ifTrue: &#091;Transcript put b] ifFalse:&#091;Transcript put c]
```

So, can we throw away Python's keywords and use a Smalltalk-style message-send? Well, this would look something like the following:

``` python
a.__if__(lambda:print b, lambda:print c) # Stupid function use, for demonstration only
```

But this would require a bit of support in the form of:

```python
True.__if__ = lambda first, second: first()

False.__if__ = lambda first, second: second()
```

Now, this all looks fine and dandy until CPython tells you off for trying to modify the `True` and `False` objects, which are written in C without any of Python's dynamic capabilities. To me this is a bug in the implementation, however since the only standard for Python is what the CPython interpreter does, this is actually the expected, valid behaviour of Python :( This is the reason Diet Python has two approaches: one does everything we can, given the inherent limitations of CPython's internal structure, whilst the other sees what we can do with a language if it were 'Python all the way down'.

Given enough of each type of translation, compatible and uncompatible, I hope to able to get Python code down to a bare minimum of syntax. At the moment I believe that at least all operators can be done away with, since the only ones left are bit shifts which I've not bothered translating yet since I've never used them. After that I think every keyword can be thrown away; `elif` is sugar for `else: if:`, whilst all of the hard work in `if:` and `else:` conditions is actually just working out the truth value of the condition, which it does itself, so we can thus give `True` and `False` methods like the above. I'm undecided about `for` loops at the moment, I may try a `map` approach or I may convert them to a `while` using the `__iter__` method. `while` itself may require a translation to Continuation Passing Style which would be interesting, but that would also get rid of `try:`, `except:`, `finally:`, `yield`, `pass`, `break`, `continue` and `return` for us. Function and class definition can be achieved with `__new__`, and if we change the behaviour of `globals()` and `locals()` to define mutable return values then we can do away with assignment, imports, numbers, strings and other such things.

Once we've done this, we have a tiny target which Python implementations must handle. Everything else can be implemented by asking Diet Python to translate it away :)
