---
title: Does Polymorphism Make Code More Flexible?
---

This is based on [my answer](http://stackoverflow.com/a/25188864/884682) to [a Stack Overflow question](http://stackoverflow.com/questions/25182186/how-does-polymorphism-make-my-code-more-flexible) about subtype-polymorphism in Object Oriented Programming.

Here's my summary of the question:

> *Head First Object Oriented Design* explains polymorphism with this example:
>
> ```java
> Airplane plane = new Airplane();
> Airplane plane = new Jet();
> Airplane plane = new Rocket();
> ```
>
> You can write code that works on the superclass, like an `Airplane`{.java}, but will work with any of the subclasses.
>
> Now I am not getting it. I need to create a sublass of an `Airplane`{.java}. For example: I create a class, `Randomflyer`{.java}. To use it I will have to create its object. So I will use:
>
> ```java
> Airplane plane = new Randomflyer();
> ```
>
> How does using a superclass save me from making extra changes to the rest of my code?

It's completely correct that sub-classes are only useful to those who instantiate them. This was summed up well by Rich Hickey:

> ...any new class is itself an island; unusable by any existing code written by anyone, anywhere. So consider throwing the baby out with the bath water.

It is still possible to *use* an object which has been instantiated *somewhere else*. As a trivial example of this, any method which accepts an argument of type `Object`{.java} will probably be given an instance of a sub-class.

There is another problem though, which is much more subtle. *In general* a sub-class (like `Jet`{.java}) *will not work* in place of a parent class (like `Airplane`{.java}). Assuming that sub-classes are interchangable with parent classes is the cause of a *huge* number of bugs.

This property of interchangability is known as the [Liskov Substitution Principle](http://en.wikipedia.org/wiki/Liskov_substitution_principle), and was originally formulated as:

> Let `q(x)` be a property provable about objects `x` of type `T`. Then `q(y)` should be provable for objects `y` of type `S` where `S` is a subtype of `T`.

In the context of your example, `T` is the `Airplane`{.java} class, `S` is the `Jet`{.java} class, `x` are the `Airplane`{.java} instances and `y` are the `Jet`{.java} instances.

The "properties" `q` are the results of the instances' methods, the contents of their properties, the results of passing them to other operators or methods, etc. We can think of "provable" as meaning "observable"; ie. it doesn't matter if two objects are *implemented* differently, if there is no difference in their results. Likewise it doesn't matter if two objects will behave differently 'after' an infinite loop, since that code can never be reached.

Defining `Jet`{.java} as a sub-*class* of `Airplane`{.java} is a trivial matter of syntax: `Jet`{.java}'s declaration must contain the `extends Airplane`{.java} tokens and there mustn't be a `final`{.java} token in the declaration of `Airplane`{.java}. It is trivial for the compiler to check that objects obey the rules of sub-classing. However, this doesn't tell us whether `Jet`{.java} is a sub-*type* of `Airplane`{.java}; ie. whether a `Jet`{.java} can be used in place of an `Airplane`{.java}, as Liskov requires. Java will allow it, but that doesn't mean it will work.

One way we can make `Jet`{.java} a sub-type of `Airplane`{.java} is to have `Jet`{.java} be an empty class; all of its behaviour comes from `Airplane`{.java}. However, even this trivial solution is problematic: an `Airplane`{.java} and a trivial `Jet`{.java} will behave differently when passed to the `instanceof`{.java} operator. Hence we need to inspect all of the code which uses `Airplane`{.java} to make sure that there are no `instanceof`{.java} calls. Of course, this goes completely against the ideas of encapsulation and modularity; there's no way we can inspect code which may not even exist yet!

Normally we want to sub-class in order to do something *differently* to the superclass. In this case, we have to make sure that none of these differences are observable to any code using `Airplane`{.java}. This is even more difficult than syntactically checking for `instanceof`{.java}; we need to know what all of that code *does*.

That's impossible due to [Rice's theorem](http://en.wikipedia.org/wiki/Rice%27s_theorem), hence there's no way to check sub-typing automatically, and hence the amount of [bugs it causes](http://okmij.org/ftp/Computation/Subtyping/).

For these reasons, [many](http://asserttrue.blogspot.co.uk/2009/02/inheritance-as-antipattern.html) see sub-class polymorphism as an [anti-pattern](http://c2.com/cgi/wiki?AntiPattern). There are other forms of polymorphism which don't suffer these problems though, for example [Parameteric polymorphism](http://en.wikipedia.org/wiki/Parametric_polymorphism) (referred to as "generics" in Java).
