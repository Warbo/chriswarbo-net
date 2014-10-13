---
title: Active Code
---
"Active code" is the term used by the [Babel][babel] system, part of
[Emacs's][emacs] [Org-mode][org]. It refers to authoring systems which can
execute code in our documents.

## Scripts ##

Babel relies on the 363kg gorilla that is Emacs, but I'd rather avoid such a
large dependency for generating my site. Instead, I've written a few scripts to
reproduce active code features outside Babel.

### Pandoc ###

[Pandoc][pandoc] is a great document conversion program by John MacFarlane. I
use it to compile the [Markdown][markdown] [source][git] of my site into static
HTML (via [Hakyll][hakyll]).

[Pandoc's Markdown][pandocmarkdown] supports code blocks, which are written like
this:

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

As you can see, these are rendered in monospaced fonts. Syntax-highlighting uses
language descriptions from [Kate][kate].

### PanPipe ###

[PanPipe][panpipe] is a Pandoc [filter][walk] which looks for code blocks
annotated with a `pipe` attribute, like this:

`sh`{pipe="tee pipe > /dev/null"}

`echo "Hello world!"`{pipe="tee body > /dev/null"}

````{.bash pipe="sh | tee pp.md"}
echo -n '```{pipe="'
cat pipe | tr -d '\n'
echo '"}'
cat body
echo ""
echo '```'
````

The command given in the `pipe` attribute (`cat pipe`{pipe="sh" .bash}) is
executed in a temporary directory; the body of the block
(`cat body`{pipe="sh" .bash}) is sent to that command's stdin, and its stdout
(`pandoc -f markdown -t markdown --filter panpipe pp.md`{pipe="sh"}) is put
inside the block.

For example, running the above through
`pandoc --filter panpipe`{.bash} gives:

```{.unwrap pipe="sh"}
pandoc -t json --filter panpipe pp.md
```

### PanHandle ###

[PanHandle][panhandler] is a Pandoc filter which looks for code in an `unwrap`
class. It extracts the code, which is assumed to be 'Pandoc JSON', and splices
it into the surrounding document.

We can create Pandoc JSON using `pandoc -t json`{.bash pipe="tee json.sh"}

```{pipe="sh > /dev/null"}
chmod +x json.sh
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

````{.javascript pipe="sh | tee bool2.md"}
echo '```{.unwrap}'
pandoc -t json bool.md
echo '```'
````

</div>

When we send our document through `pandoc --filter panhandle`{.bash}, the table
will be spliced into the document, like this:

```{.unwrap pipe="sh | pandoc -t json"}
cat bool2.md
```

## Examples ##

These simple scripts let us call out to the UNIX shell from our documents. This
lets us recreate many of the active code features of Babel, just by piping
between programs and reading/writing files.

### This ###

Take a look at [this page's source][this] to see how the results are generated
from the examples.

### Fibonacci Post ###

I wrote PanPipe and PanHandle after trying and failing to integrate Babel into
my site's build process. My [Fibonacci Sequence in PHP][fib] post was an
experiment with Babel, so porting that post over to Pandoc was the motivating
use-case for these scripts. Thankfully the port was a success, and that post is
now managed by Hakyll like the rest of the site.

If you compare it to [the source][fibsource] you'll see a few of the required
features:

 - A temporary directory for downloading dependencies
 - Rendering, executing, or rendering *and* executing code snippets
 - Concatenating code snippets together in a source file ("tangling")
 - Rendering code output verbatim or interpreted (eg. as a table or image)
 - Hidden blocks for writing helper functions and unit tests
 - Aborting rendering when unit tests fail

Here are some of the techniques I came up with for these tasks.

### Hiding Output ###

To hide the output of a code block, just pipe it to `/dev/null`:


````{.bash pipe="tee hide.md"}
```{pipe="sh > /dev/null"}
ls /
```
````

The resulting HTML is:

````{.html pipe="sh | pandoc --filter panpipe"}
cat hide.md
````

### Showing Code *and* Output ###

We can use `tee` to save a copy of our code into a file, then run it in another
code block:

````{.php .fullphp pipe="tee both.php"}
```{.php pipe="tee script.php"}
<?
echo 10 + 20;
```
````

````{.bash pipe="tee both.sh"}
```{pipe="sh"}
php script.php
```
````

This results in:

<div class="fullphp">

```{.unwrap pipe="sh | pandoc -t json --filter panpipe"}
cat both.php
echo ""
echo ""
cat both.sh
```

</div>

### Tangling ###

Use `tee -a` to append to a file. Make sure to include extra newlines as needed:

````{pipe="tee tangled.md"}
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

Blocks are executed from the top of a document to the bottom. We can change
the order they're *displayed* in by capturing their output to files and dumping
them out later. For example, to show a program listing *after* its results:

````{.bash pipe="tee order.sh"}
```{pipe="tee code.sh > /dev/null"}
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

````{.php .fullphp pipe="tee proc.md"}
```{.unwrap pipe="php | pandoc -t json"}
<?
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

````{.bash}
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

````{.php .fullphp pipe="tee image_php.md"}
```{pipe="php > carpet.pbm"}
<?
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
````

````{.bash pipe="tee image_sh.md"}
```{.unwrap pipe="sh | pandoc -t json"}
convert carpet.pbm carpet_big.png
pngcrush -brute carpet_big.png carpet.png
echo -n '<img alt="Sierpinski Carpet" src="data:image/png;base64,'
base64 -w 0 carpet.png
echo -n '" />'
```
````

This results in:

```{.unwrap pipe="sh | pandoc --filter panpipe --filter panhandle -t json"}
cat image_php.md
echo ""
echo ""
cat image_sh.md
```

### Aborting ###

A non-zero exit code will abort the rendering:

````{.php .fullphp}
```{pipe="php"}
<?
if (!$success) exit(1);
```
````

[hakyll]: http://jaspervdj.be/hakyll/
[markdown]: http://commonmark.org/
[pandoc]: http://johnmacfarlane.net/pandoc/
[emacs]: http://www.gnu.org/software/emacs/
[org]: http://orgmode.org/
[babel]: http://orgmode.org/worg/org-contrib/babel/
[web]: http://en.wikipedia.org/wiki/WEB
[represearch]: http://reproducibleresearch.net/
[git]: https://gitorious.org/chriswarbo-dot-net
[walk]: http://johnmacfarlane.net/pandoc/scripting.html
[ast]: https://hackage.haskell.org/package/pandoc-types-1.8/docs/Text-Pandoc-Definition.html#t:Block
[panpipe]: https://gitorious.org/panpipe/panpipe/source/master:README
[panhandler]: https://gitorious.org/pan-handler/pan-handler/source/master:README
[this]: https://gitorious.org/chriswarbo-dot-net/chriswarbo-dot-net/raw/master:essays/activecode/index.md
[elpa]: http://orgmode.org/elpa.html
[pandocmarkdown]: http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html
[kate]: http://kate-editor.org/
[fib]: /posts/2014-07-23-fib.html
[fibsource]: https://gitorious.org/chriswarbo-dot-net/chriswarbo-dot-net/raw/master:posts/2014-07-23-fib.md
