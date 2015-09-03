---
title: PyMeta Stacks
---
As you may know if you've been following my blog, I've been working on a project for a few years called, rather glamorously, "python decompiler". I've released it into the Public Domain and you can find it on <a href="https://gitorious.org/python-decompiler">gitorious</a>.<br /><br />The idea is that compiling software (translating it from one form to another) is a mathematical operation, and that for every mathematical operation we would like to know its inverse. Version 2 of <a href="http://python.org/">Python</a> contains a <a href="http://docs.python.org/library/compiler.html">compiler</a>, which creates <a href="http://en.wikipedia.org/wiki/Abstract_syntax_tree">Abstract Syntax Trees</a> (ASTs) out of Python code, but there is no reverse transformation to be found. That's what python decompiler does; it takes an AST and produces Python code that will compile into that AST, thus compile(decompile(compile(code))) is the same as compile(code). We can't quite make decompile(compile(code)) match the original code since the compiler throws away information ((((((like redundant brackets)))))).<br /><br />So how does it work? I've used the <a href="https://launchpad.net/pymeta">PyMeta</a> library, a Python implementation of the <a href="https://gitorious.org/python-decompiler">OMeta</a> pattern matching system. This is applied to the AST, and pattern-matches against AST node objects. The only difficulty with doing this is that a OMeta is designed for pattern-matching against an iterable object, such as a string of text, a list of objects, etc. and I want to use it to pattern-match recursively inside objects, preferably without blowing those objects apart into a serialised form beforehand.<br /><br />The original way I did this was to instantiate a new pattern matcher on every child object, so for example the Python code "1 + 2 * 3 / 4 - 5" will produce an AST a bit like "Sub(Add(Const(1),Div(Mul(Const(2),Const(3)),Const(4)),Const(5))". The classical way to pattern-match such a tree is to make a rule that matches "Sub(A,B)" where "A" and "B" are themselves other rules, and so on. This recursion of rules is great for digging out structures like the AST I've written out as text above, however in Python we've got a structured system of objects that a) doesn't need constructing (it's already structured) and b) has encapsulated innards, which makes pattern-matching against the structure tedious (although Python's introspection makes it reasonable, at least). Since Python is a dynamic language we would also like to avoid having to look at the inside of objects, since that would make the code less flexible than if it implies the contents. By making new pattern matchers, rather than defining new rules, we get arbitrary nesting ability and don't have to look inside objects (they look inside themselves, recursively). The drawback to this is that every single node in the AST needs its own pattern matcher, which takes ages to run (since OMeta pattern matchers are bootstrapped from the ground up in themselves) and eats up tons of RAM.<br /><br />I received a couple of emails about python decompiler the other day, so it seems that people may be playing with it after all. To that end, I decided to have a bit of a think about improving its efficiency, and realised that I could get the required recursion if I turn the input stream into a stack. This is suprisingly easy to do, and straightforward to use (as long as you make sure the stack will be clean when you're finished, regardless of if your rules match or not!). We add the following function to a newly constructed grammar (which is a class in PyMeta):<br /><br />def ins(self, val):<br /> ...self.input.data.insert(self.input.position+1, val)<br />...self.input.tl = None<br />...self.input.memo = {}<br />grammar.ins = ins<br /><br />(I've written "..." to represent indentation. Silly blogger HTML editor.) 5 lines is all it takes, but now we can push on to a pattern matcher's input stream from within the grammar. We do this by calling Python code from within a rule, by wrapping it as !(code). Thus we just have to call !(self.ins(a)) and "a", whatever it is, will be the next thing on the input stream, so we can match against it. This turns the input stream into a stack, and makes OMeta recursive whilst only needing 1 pattern matcher object.<br /><br />I've now converted python decompiler to use this form. I've run some quick tests by getting python decompiler to compile, decompile and recompile its own test file, and check the results for validity. The previous method managed, using the "time" utility and KDE's system monitor, take 44 seconds and 202MB of RAM. This is a completely stupid amount of resources for text processing, even in an interpreted language. After the change it now takes 38 seconds and 129MB RAM. That's not a great speed improvement (14%), but the memory usage is much better (a 36% reduction). I'm sure I can get this down even more with a little playing :)