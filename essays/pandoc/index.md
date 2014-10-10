---
title: Pandoc Scripts
---

## Introduction ##

As I write this (2014-10-09), my Web site is built with [Hakyll][hakyll]. The
content is mostly written in [Markdown][markdown] and compiled to static HTML
files using [Pandoc][pandoc]. Hakyll automates all of this, and also provides a
handy `watch` command which serves a local version of the site, monitoring files
for changes and recompiling them and their dependents.

This works well, but since I do all of my writing in [Emacs][emacs], and most of
my site is about programming, I find myself missing one of the killer features
of [Org Mode][org]: the [Babel][babel] 'active code' system.

### Active Code ###

An active code system lets us write source code in our document. Babel uses the
following syntax:

```
{+begin_src sh}
  echo "Hello, world!"
{+end_src}
```

This code can be *executed* when the document is rendered, the output can be
interpreted in a variety of ways, and spliced back into the document. Of course
regular niceties like syntax highlighting still apply.

Some nifty features offered by Babel:

 - Some languages can be interpreted using their REPL, which persists between
   blocks. This lets us define variables in one block and use them in another.
 - All source blocks can be named, allowing them to be called explicitly in
   other parts of the document.
 - Some languages can be given input variables; if the name of a source block is
   used as the value, the named block will be executed and its result will be
   used as the value (even if they're different languages)
 - A [Web][web]-like macro system can be used for text-replacement. This allows
   text to be spliced into code blocks before they're executed.

The advantage this gives when writing a programming blog (or a paper which uses
code, either as the main focus or in a background, data analysis capacity) is
that we don't have to maintain separate copies of anything:

 - *All* of the necessary code to [replicate][represearch] my results are
   available right in the post itself (or at least in its [source][git])
 - I don't have to develop the code separately, in dedicated source files, and
   copy-paste bits into the document.
 - If I *want* to develop the code separately, I can put scripts in the document
   to fetch and import the code, rather than doing it manually.
 - I don't have to copy-paste snippets/commands out of the blog post into a
   separate file/REPL in order to run it. I can run it right there, in the blog.
 - I don't have to copy-paste results from the REPL/output back into the blog.
 - Since there's no copy-pasting going on, the code and results being presented
   *always* correspond exactly (eg. I can't update some code and forget to
   update the results, or vice versa)

#### Problems With Babel ####

So, why don't I use Babel to write my blog posts? There are a few drawbacks:

 - Dependent on Emacs: Emacs is great, but it's *big*. I'd like the ability to
   clone my blog repo and compile it, without installing such a big dependency.
 - Not easily scriptable: Hakyll's `unixFilter` function can pipe pages through
   external programs for compilation; but Emacs requires a wrapper script to
   convert these pipes to temporary files and back.
 - Awkward to script: Org-mode must be invoked in just the right way, most
   commands are designed to be interactive, the API is unstable (eg.
   `org-export-as-html` vs. `org-html-export-to-html`) and results are too tied
   to incidental editor features (eg. syntax-highlighting based on the current
   Emacs theme). These can all be worked around, but it adds complexity and
   fragility.
 - Templating: Once a page body is rendered, Hakyll splices it into templates
   for titles, menus, CSS, etc. Org-mode outputs whole HTML pages, which I found
   myself trying to parse in order to shoe-horn them into Hakyll.
 - Inconsistent features: Interpreting return values, maintaining a REPL session
   and passing input variables are implemented in Babel on a
   language-by-language basis. This is frustrating, since I'm a fan of obscure
   languages, but even popular languages like PHP have no support at the moment.

After wrestling with this stuff for a week, with nothing to show except an
elaborate house of cards, I decided to go back to basics.

### Hakyll/Pandoc/Markdown ###

Markdown lets us write code blocks using the following syntax:

```
`echo "Inline code"`{.sh}
```

```
    echo "Indented code blocks (4 spaces)"
```

````
```sh
echo "Fenced code blocks"
```
````

Hakyll can send these through Pandoc for rendering, but other than
syntax-highlighting there are no active code facilities.

Thankfully Pandoc provides a nice [Haskell API][walk] for processing its
[ASTs][ast] programatically. The scripts on this page use this API to recreate
some of the active code features found in Babel.

## PanPipe ##

[PanPipe][panpipe] is a pre-processor for Pandoc documents, delivered via stdio.
It looks for code blocks annotated with a `pipe` attribute, like this:

`sh`{.hidden pipe="tee pipe"}

`echo "Hello world!"`{.hidden pipe="tee body"}

````{pipe="sh | tee pp.md"}
echo -n '```{pipe="'
cat pipe | tr -d '\n'
echo '"}'
cat body
echo ""
echo '```'
````

The contents of this attribute, in this case `cat pipe`{pipe="sh"}, will be
executed as a shell command inside a temporary directory. The contents of the
code block, in this case `cat body`{pipe="sh"}, will be taken out of the code
block and piped into the command. The results (which will be
`cat pp.md | panpipe`{pipe="sh"}, if all goes well) will be used as the
new contents of the code block.

Hence, the result of piping the above Markdown through `panpipe` will give:

```{pipe="sh"}
cat pp.md | panpipe
```

If you don't believe me, take a look at [this page's source][this]!

It turns out we can get most of Babel's features by using shell scripts in our
code blocks. For example, if we want to refer to code from another block, we can
pipe it through `tee`, then `cat` it later. This will all be cleaned up by
PanPipe when it deletes the temporary directory.

## PanHandler ##

One problem with PanPipe is that its results are stuck inside code blocks. For
example, if I want to generate a Markdown list with a script like this:

`````{pipe="tee list" .hidden}

```{pipe="php"}
<?
foreach (range(1, 10) as $x) {
  echo " - Element $x\n";
}
```

`````

```{.php pipe="sh"}
cat list
```

Instead, I get the Markdown syntax stuck inside a code block:

```{pipe="sh"}
cat list | panpipe
```

[hakyll]: http://jaspervdj.be/hakyll/
[markdown]: http://commonmark.org/
[pandoc]: http://johnmacfarlane.net/pandoc/
[emacs]: http://www.gnu.org/software/emacs/
[org]: http://orgmode.org/
[babel]: http://orgmode.org/worg/org-contrib/babel/
[web]: http://en.wikipedia.org/wiki/WEB
[represearch]: http://reproducibleresearch.net/
[git]: https://gitorious.org/chriswarbo-dot-net
[walk]: http://hackage.haskell.org/package/pandoc-types-1.10/docs/Text-Pandoc-Generic.html
[ast]: https://hackage.haskell.org/package/pandoc-types-1.8/docs/Text-Pandoc-Definition.html#t:Block
[panpipe]: https://gitorious.org/panpipe/panpipe/source/master:README
[this]: https://gitorious.org/chriswarbo-dot-net/chriswarbo-dot-net/raw/master:essays/pandoc/index.md
