---
title: Turtle Viewer
---
TurtleViewer is a very silly 1-bit image format. Given any image, it will generate instructions for a LOGO turtle that will draw a black-and-white version of that image. These instructions are given out as a Python file that uses Python's "turtle" module to run the LOGO instructions.

Whilst this may sound like a complete waste of time, it can actually serve as a decent demonstration of Algorithmic Information Theory. One of the most obvious uses of Information Theory is compression, and image compression makes this problem nice and tangible since visualising it is easy (obviously!). If we had an inert data format, like PNG, with a program to interpret it, then we can use Shannon's original version of Information Theory, which deals with the probability of certain inputs; those which are highly probable should take fewer bits to express, at the sacrifice of lower probability inputs taking more bits to express. However, by making the image out of code, we have to face the much more brain-damaging world of Algorithmic Information Theory; what is the shortest program that will give us the output we want? The length of this program is known as the Kolmogorov complexity of the output, and it can never be known (ie. it's undecidable) due to the halting problem; getting as close as possible is certainly very cool though!

Consider, you are given a sequence of LOGO instructions, what can you do to make them simpler? There are all sorts of tricks we can do, like "forward x; forward y;" -> "forward x+y;"; notice that we only need left or right, not both: "left x;" -> "right 360-x;", "right x;" -> "left 360-x;", and of course "left x; left y;" -> "left x+y;" and "right x; right y;" -> "right x+y;".

TurtleViewer can serve as a test-bed for lots of this kind of experimentation. At the time of writing, it's actually a big cheat and uses a "goto" command to move the turtle around, rather than good old-fashioned "forward", "left/right", "penup" and "pendown".

The code, as always, is in the Public Domain and can be found on [GitHub](https://github.com/warbo/turtleviewer)
