---
title: Active Code Updates
---

I've updated my [active code](/essays/activecode) setup to fix a problem which has caused me much chagrin: handling of stderr.

## Active Code Overview ##

"Active code" means:

 1. Writing a document in a markup format (HTML, LaTeX, Markdown, etc.) which contains snippets of code
 1. Having that code automatically executed during the document rendering process
 1. Having the results of that execution affect the document which is produced

For example, we might be writing [a document](/blog/2014-07-23-fib.html) which compares the execution time of different algorithms. There are two problems we might encounter, which active code can help us avoid:

 - The code examples shown in our document must be kept in sync with the code we're measuring the performance of. Active code makes this trivial: the code we show in our document *is* the code we're measuring!
 - The graphs we show in our document must correspond to the code shown in the document. Again, active code makes this trivial: by generating the graphs automatically during rendering, the correspondence always holds.
    - We don't need, for example, a directory full of images, named according to some convention.
    - Even if we *want* a directory full of images, eg. if they take a long time to render, we can still use active code to automatically *select* the correct image, rather than relying on a human to follow the convention.

## My Setup ##

I solve the three requirements of active code using three interacting programs:

 - [Pandoc](http://johnmacfarlane.net/pandoc/) is an excellent document conversion program, supporting many input and output formats. I tend to write in Markdown and render to HTML or PDF (via LaTeX). Pandoc fulfils the first requirement because it has the concept of *code blocks* ("dumb" source code listings).
 - [PanPipe](/git/panpipe) is a script I made to allow execution of Pandoc code blocks. In fact, it solves a much simpler and more general problem: processing the contents of code blocks using arbitrary shell commands. By choosing a program interpreter as our shell command (eg. `bash`, `php`, `python`, `runhaskell`, etc.) then we fulfil the second requirement.
 - [PanHandle](/git/pan-handler) fulfils the third requirement by extracting the contents of code blocks and splicing them into the document tree. Without PanHandle, the results (standard output) of our shell commands are locked inside their associated code blocks.

## The Problem ##

Until now, PanPipe has avoided handling the standard *error* stream of our shell commands. If the command triggers an error (exits with a non-zero status) then the rendering is aborted, but we can't see *why*.

I've been working around this by redirecting stderr to stdout and overriding exit codes, so that error messages appear inside the resulting document. Not only is this dangerous (there is no indication that the rendering failed unless we read through the document looking for error messages, if there are any), but it's also incredibly inconvenient.

Finally, I've fixed the problem at the source: PanPipe now echoes stderr from shell commands to its own stderr, whether or not it succeeds or fails. This makes it much easier to debug the active code embedded in our documents.
