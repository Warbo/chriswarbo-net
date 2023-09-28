---
title: Active Code
packages: [ 'imagemagick', 'ghc', 'php' ]
---
"Active code" is the term used by the [Babel][babel] system, part of
[Emacs's][emacs] [Org-mode][org]. It refers to authoring systems which can
execute code embedded in the documents they're rendering. This page documents
the active code system I use to write articles, most notably the HTML on
[my Web site](/).

<!-- Why not zoidberg? -->

```{pipe="tr -d '\n' > img.b64"}
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
```

```{.unwrap pipe="sh | pandoc -f html -t json"}
printf '<img src="data:image/png;base64,'
cat img.b64
printf '" alt="Why not Babel?" style="width: 300px;" />'
```

Babel, Org mode and Emacs are all wonderful things, however there are a few
reasons why we may want to avoid them, which can be summarised by saying
"they're not UNIX":

 - Emacs is a large dependency; it would be nice to have a small binary or
   script which perform *only* the conversion process
 - Org mode is packed full of features; Babel alone has various ways of handling
   input, output, sessions, variables, interpreting results, etc. The UNIX
   approach would use a few simple, general principles rather than exposing all
   of these separately.
 - Babel, Org mode, etc. are moving targets; it would be nice to avoid an
   upgrade treadmill if possible.

## The Alternative: Pandoc ##

[Pandoc][pandoc] is a great document conversion program by John MacFarlane. It
can convert between various markup languages, including HTML, LaTeX and
Markdown. We can also mix and match the formats, for example embedding a mixture
of HTML and LaTeX in a Markdown document and rendering it to a PDF.

In particular, most of the [source][git] of [my Web site](/) is written in
[Markdown][markdown] and converted to HTML using Pandoc. I use [Nix][nix] to
orchestrate the process.

### Embedded Code ###

Most of the following examples are written in
[Pandoc's Markdown format][pandocmarkdown], but they can also be used with other
formats supported by Pandoc (e.g. HTML).

Pandoc supports *code blocks*, which can be written in three different ways in
Markdown:

```bash
`echo "Inline code"`
```

```bash
    echo "Indented code"
```

````bash
```
echo "Fenced code"
```
````

As you can see, by default these get rendered in monospaced fonts. The "inline"
form, as the name suggests, gets rendered as part of any surrounding text
`like this`. The other two forms make "blocks", which get rendered like separate
paragraphs.

Code blocks can have "attributes", "classes" and an "id". In markdown, these
look like:

````bash
```{#SomeID .SomeClass .AnotherClass Attribute1="Value1" Attribute2="Value2"}
Some content
```
````

This lets us manipulate blocks, for example if we're rendering to HTML we might
use these IDs, classes and attributes from some associated Javascript. Pandoc
also uses classes to apply syntax-highlighting, based on language descriptions
from [Kate][kate].

For those who don't want to write markdown, here's the equivalent HTML input:

```
<code>echo "inline code"</code>

<pre>
  echo "Code block"
</pre>

<pre id="SomeID" class="SomeClass AnotherClass" Attribute1="Value"
     Attribute2="Value2">
  Some content
</pre>
```

Whether markdown, HTML or anything else, these are the standard, off-the-shelf
ways to embed code snippets in a document.

However, such code is not *active*.

### PanPipe ###

[PanPipe][panpipe] is a Pandoc [filter][walk] which walks the document tree
looking for code (inline or blocks) annotated with a `pipe` attribute, like
this:

`sh`{pipe="cat > pipe"}

`echo "Hello world!"`{pipe="cat > body"}

````{.markdown pipe="sh | tee pp.md"}
echo -n '```{pipe="'
cat pipe | tr -d '\n'
echo '"}'
cat body
echo ""
echo '```'
````

When such code is found, the following steps take place:

 - The value of the `pipe` attribute (`cat pipe`{pipe="sh" .bash}) is executed
   as a shell command.
 - The element's content (`cat body`{pipe="sh" .bash}) is removed and piped into
   that shell command's standard input.
 - The standard output of the command
   (`pandoc -f markdown -t markdown --filter panpipe pp.md`{pipe="sh"}) is
   inserted into the element as its new body.

For example, running the above through `pandoc --filter panpipe`{.bash} gives:

```{.unwrap pipe="sh"}
pandoc -t json --filter panpipe pp.md
```

Note that the `pipe` attribute is *not* a "label" telling PanPipe "which
language to use", or anything to that effect. It's a shell command: nothing
more, but also nothing less. For example, rendering:

```{pipe="sh > /dev/null"}
echo -e '```{pipe="tr l L | sed -e '\''s/ /_/g'\''"}\nHello world!\n```' > pp2.md
```

```{pipe="cat pp2.md"}
```

Yields a document containing:

```{.unwrap pipe="pandoc -f markdown -t json --filter panpipe < pp2.md"}
```

### PanHandle ###

[PanHandle][panhandler] is a Pandoc filter which looks for code in an `unwrap`
class. It extracts the code, which is assumed to be in 'Pandoc JSON' format, and
splices it into the surrounding document.

We can turn any Pandoc-supported format into Pandoc JSON by piping it through
`pandoc -t json`{.bash pipe="tee json.sh"}

```{pipe="sh > /dev/null"}
chmod +x json.sh
(source "$stdenv/setup" && patchShebangs .)
```

For example, if we take the JSON for this Markdown table:

```{pipe="tee bool.md"}
X NOT(X)
- ------
T F
F T
```

and wrap it in an `unwrap` code block, we get:

<div style="overflow-x: scroll;">

````{.markdown pipe="sh | tee bool2.md"}
echo '```{.unwrap}'
pandoc -t json bool.md
echo '```'
````

</div>

When we send our document through `pandoc --filter panhandle`{.bash}, the table
will be spliced into the document, like this:

<div class="summarise">
  <span class="summary">PanHandle example</span>

```{.unwrap pipe="sh | pandoc -t json"}
cat bool2.md
```

</div>

On its own, PanHandle is pretty useless. The Pandoc JSON format is just a
program-specific, non-standard, rather ugly intermediate format; not something
we should be writing our documents in. Besides which, if we want to embed
something like a table in our documents, we should just go ahead and put the
damned thing where it's supposed to be; rather than encoding it into Pandoc
JSON, sticking it in a code block, then using PanHandle to unwrap it and decode
it again!

The point of PanHandle isn't to unwrap *hard-coded* strings of JSON, like the
table example above; it's to unwrap *procedurally generated* JSON, i.e. the
output of PanPipe. PanPipe is specifically designed to *only* manipulate the
contents of code blocks: it *cannot* interfere with the rest of the document.
This is a useful restriction, since we may be calling out to arbitrary commands.
By using PanHandle, we have a single, simple, predictable and opt-in way to
splice generated content into our documents.

## Examples ##

Some non-toy examples of this system in action:

### [This Site](/) ###

[This whole site](/) is static HTML generated from Markdown with these tools.
Not every page takes advantage of these capabilities, but it's nice to know
they're available when I need them. You may like to browse
[this page's source][this] to see how the example output is derived straight
from the examples themselves (note that this requires meta-programming, which
complicates things a little).

### Fibonacci Post ###

I wrote PanPipe and PanHandle after trying and failing to integrate Babel into
my site's build process. My [Fibonacci Sequence in PHP][fib] post was an
experiment with Babel, so porting that post over to Pandoc was the motivating
use-case for these scripts. Thankfully the port was a success, and that post is
now rendered by Pandoc like the rest of the site.

If you compare it to [the source][fibsource] you'll see a few of the required
features which influenced my thinking:

 - A temporary directory for downloading dependencies
 - Rendering, executing, or rendering *and* executing code snippets
 - Concatenating code snippets together in a source file ("tangling")
 - Rendering code output verbatim or interpreted (eg. as a table or image)
 - Hidden blocks for writing helper functions and unit tests
 - Aborting rendering when unit tests fail

## Useful Tricks ##

These simple scripts let us call out to the UNIX shell from our documents. This
lets us recreate many of the active code features of Babel, just by piping
between programs and reading/writing files. Here are some common tasks you may
want to solve:

### Hiding Output ###

You may want a code block to execute, but not show up in the output. The easiest
way is to pipe the output to `/dev/null`, or an actual file if we plan to use it
later:

````{.markdown pipe="tee hide.md"}
```{pipe="sh > /dev/null"}
ls /
```

```{pipe="sh > contents"}
ls /
```
````

This works well for HTML, and results in:

````{.html pipe="sh | pandoc --filter panpipe"}
cat hide.md
````

```{pipe="cat > dash"}
echo -n '`'
cat
echo -n '`'
```

```{pipe="sh > /dev/null"}
chmod +x dash
(source "$stdenv/setup" && patchShebangs .)
```

```{pipe="sh > inline_hidden"}
echo 'ls /' | ./dash
echo '{pipe="sh > /dev/null"}'
```

Sometimes these empty elements may have undesirable effects, e.g. interacting
badly with some styling rule. If this is the case, you might try using inline
snippets instead, e.g. `cat inline_hidden`{pipe="sh"}, which gives
`cat inline_hidden`{pipe="sh | pandoc --filter panpipe"}.

#### Splicing Nothing ####

To eliminate the code block takes a little more effort, but might be necessary
in some cases. To remove a code block, we can use `panhandle` to splice an empty
document in its place.

Remember that `panhandle` accepts JSON, which we can generate using `pandoc`:

````{pipe="cat > blanker"}
```{.unwrap pipe="sh | echo '' | pandoc -t json"}
ls /
```
````

```{pipe="sh"}
cat blanker
```

Here's the result when converting to HTML:

```{pipe="sh"}
pandoc --filter panpipe --filter panhandle -t html < blanker
```

Ta da! If our code block has any extra attributes, etc. then a `div` will be
left behind to catch them, for example:

````{pipe="cat > blank_attrs"}
```{.unwrap pipe="sh | echo '' | pandoc -t json" myattr="myvalue"}
ls /
```
````

```{pipe="sh"}
cat blank_attrs
```

This gives:

```{pipe="sh"}
pandoc --filter panpipe --filter panhandle -t html < blank_attrs
```

#### Format-specific ####

If you're targetting a specific output format, you can use techniques specific
to that format.

For example, if you're rendering to HTML, you can hide code blocks with CSS:

````{pipe="cat > hideCss"}
```{pipe="sh" style="display: none;"}
ls /
```
````

```{pipe="sh"}
cat hideCss
```

This results in:

```{.html pipe="sh | pandoc --filter panpipe -t html"}
cat hideCss
```

If you're using LaTeX you can use `if` statements to skip over the block (it
will still be executed, but the result won't be rendered):

````
\iffalse

```{pipe="sh"}
ls /
```

\fi
````

### Showing Code *and* Output ###

We can use `tee` to save a copy of our code into a file, then run it in another
code block:

````{.markdown .php pipe="tee both.php"}
```{.php pipe="tee script.php"}
<?php
echo 10 + 20;
```
````

````{.markdown pipe="tee both.sh"}
```{pipe="sh"}
php script.php
```
````

This results in:

<div class="php">

```{.unwrap pipe="sh | pandoc -t json --filter panpipe"}
cat both.php
echo ""
echo ""
cat both.sh
```

</div>

### Tangling ###

Use `tee -a` to append to a file. Make sure to include extra newlines as needed:

````{.markdown pipe="tee tangled.md"}
```{.haskell pipe="tee -a tangled.hs"}
foo = "Hello"

```

```{.haskell pipe="tee -a tangled.hs"}
bar = "World"

```

```{.haskell pipe="ghci -v0"}
:load tangled.hs
print (foo ++ " " ++ bar)
```
````

This gives:

```{.unwrap pipe="sh"}
pandoc -t json --filter panpipe tangled.md
```

### Execution Order ###

PanPipe executes code in the order it appears in the source document (although
it uses two passes: one for code blocks and one for inline code, so it's a bad
idea to rely on execution order between the two).

We can change the order that results are *displayed* in by capturing their
output to files and dumping them out later. For example, to show a program
listing *after* its results:

````{.markdown pipe="tee order.sh"}
```{pipe="cat > code.sh"}
echo "Hello"
echo "World"
```

```{pipe="sh"}
sh code.sh
```

```{.bash pipe="sh"}
cat code.sh
```
````

This produces:

```{.unwrap pipe="sh | pandoc --filter panpipe -t json"}
cat order.sh
```

### Procedural Documents ###

We can generate content using PanPipe, send it through Pandoc to get JSON, then
use PanHandle to splice it into the document. For example:

````{.markdown .php pipe="tee proc.md"}
```{.unwrap pipe="php | pandoc -t json"}
<?php
foreach (range(1, 10) as $x) {
  echo " - Element $x\n";
}
```
````

This produces:

```{.unwrap pipe="sh | pandoc --filter panpipe --filter panhandle -t json"}
cat proc.md
```

### Importing Sub-Documents ###

We can use PanPipe to dump the contents of files and PanHandle to combine them
together. We can even call Pandoc recursively:

````{.markdown}
```{.unwrap pipe="sh"}
pandoc -t json header.md
```

```{.unwrap pipe="sh"}
pandoc -t json footer.md
```
````

### Including Images ###

We can obtain image files using PanPipe, then encode them in data URIs.
PanHandle will splice these into the document:

````{.markdown .php pipe="tee image_php.md"}
```{pipe="php > carpet.pbm"}
<?php
$scale = 5;
$dim   = pow(3, $scale);
$max   = ($dim * $dim) - 1;

function carpet($x, $y) {
  if ($x % 3 == 1 && $y % 3 == 1) return 0;
  return ($x || $y)? carpet(intval($x / 3),
                            intval($y / 3))
                   : 1;
}

$colour = function($c) use ($dim) {
  $x =  $c       % $dim;
  $y = ($c - $x) / $dim;
  return carpet($x, $y);
};

echo "P1 $dim $dim\n";
echo implode("\n", array_map($colour, range(0, $max)));
```

```{.unwrap pipe="sh | pandoc -t json"}
convert carpet.pbm carpet.png
echo -n '<img alt="Sierpinski Carpet" src="data:image/png;base64,'
base64 -w 0 carpet.png
echo -n '" />'
```
````

This results in:

```{.unwrap pipe="sh | pandoc --filter panpipe --filter panhandle -t json"}
cat image_php.md
```

### Handling Errors ###

In general, errors should abort the rendering. We would rather have no document
than an erroneous one.

If you want to trigger an error from a command, just have it return a non-zero
exit code:

````{.markdown}
```{pipe="sh"}
if [ ! -e "foo" ]
then
  exit 1
fi
cat foo
```
````

If you want to carry on rendering in the presence of errors, you must implement
some kind of error handling to ensure your command exits with a success code.
For example, in shell scripts:

````{.markdown}
```{pipe="sh"}
./dodgyCommand || echo "dodgyCommand didn't work; oh well!"
```
````

Anything printed to stderr by a shell command will appear in the stderr of
PanPipe. Likewise, when used as a Pandoc filter, PanPipe's stderr will appear in
Pandoc's stderr. Note that [Pandoc may buffer the stderr stream][pandocbuffer],
which prevents content showing immediately (eg. progress bars and such). To
prevent this, you can use
`pandoc -t json | panpipe | panhandle | pandoc -f json` rather than
`pandoc --filter panpipe --filter panhandle`.

[nix]: https://nixos.org/nix
[markdown]: http://commonmark.org/
[pandoc]: http://johnmacfarlane.net/pandoc/
[emacs]: http://www.gnu.org/software/emacs/
[org]: http://orgmode.org/
[babel]: http://orgmode.org/worg/org-contrib/babel/
[web]: http://en.wikipedia.org/wiki/WEB
[represearch]: http://reproducibleresearch.net/
[git]: /git/chriswarbo-dot-net
[walk]: http://johnmacfarlane.net/pandoc/scripting.html
[ast]: https://hackage.haskell.org/package/pandoc-types-1.8/docs/Text-Pandoc-Definition.html#t:Block
[panpipe]: /git/panpipe/branches/master/README.md
[panhandler]: /git/panhandle/branches/master/README.md
[this]: /git/chriswarbo-net/branches/master/projects/activecode/index.md
[elpa]: http://orgmode.org/elpa.html
[pandocmarkdown]: http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html
[kate]: http://kate-editor.org/
[fib]: /blog/2014-07-23-fib.html
[fibsource]: /git/chriswarbo-net/branches/master/blog/2014-07-23-fib.md
[pandocbuffer]: https://github.com/jgm/pandoc/issues/2729
