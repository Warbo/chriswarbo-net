---
title: Standard Syntax Trees
---

After much mucking around with programming languages, I've come to the opinion
that programmers will resort to using all sorts of dirty hacks, making their
code unparseable without their exact combination of OS, preprocessors, install
paths, etc. such that code "written in X" only has one thing in common: it runs
when given to some "X" compiler/interpreter.

With this in mind, we find that initiatives like `haskell-src-exts` and even the
GHC API aren't enough to parse Haskell code. The only thing which will always
work is parsing whatever's given to GHC, after being generated, preprocessed,
etc.

That's why [AstPlugin](http://chriswarbo.net/projects/repos/ast-plugin.html) is
written as a GHC plugin, after my efforts to parse the contents of Hackage with
the GHC API and `haskell-src-exts` failed.

More generally, I think that programming language definitions should include a
standard parsed format in addition to their human-friendly syntax. This can just
be an s-expression representation of the normal, concrete syntax, without any
transformations applied. This way, we don't have to build language-specific
rules for precedence, offside/significant-whitespace, fixity, etc. into every
tool for that language.

Implementations (compilers and interpreters) should include tools/modes for
converting between these two representations, and should support running
programs written in the parsed format in addition to those written in the
human-friendly syntax. This isn't asking much: the difficult part is parsing the
human-friendly syntax, which implementations must already do.

The benefit is that we don't end up with a mismatch between what the
compilers/interpreters accept, and what the other tooling accepts (linters,
formatters, doc generators, static analysers, search engines,
syntax highlighters, use-finders, go-to-definition, refactoring tools, etc.).

This would also make it much easier to make new tools, extend existing ones, and
improve practices, e.g. like syntax-aware diffing, standard code formatting,
version-control-friendly representations, tree-based editors, more powerful
navigation in editors and IDEs, etc.

In this regard, I think that the overwhelming rejection of s-expressions by
developers has thrown the baby out with the bath water: they're really good for
manipulating code programatically, which makes many tools much simpler and more
reliable to build, and can quickly bootstrap a powerful ecosystem of tooling
around a language (e.g. think of all the tooling which comes "for free" if you
define a new data format as JSON or XML). Just because someone doesn't want to
read and write code as s-expressions doesn't mean their compilers, interpreters,
IDEs, VCSs, etc. can't use them.

I think another contributor to the status quo has been the conflation of
s-expressions with Lispisms like macros, eval, quasiquoting, dynamism, etc. when
they're actually quite orthogonal ideas. It seems like Lisp *requires*
s-expressions (or equivalent), since macros allow ad-hoc syntax; I may be wrong,
but I've not seen any syntax which can represent all Lisp programs, but isn't
equivalent to s-expressions. Hence Lispers, who *do* care about s-expressions,
are justifiably *against* compiling some alternative syntax down to
s-expressions: it either means giving up some macro abilities, or being limited
to minor cosmetic changes (e.g. sweet expressions).

On the other hand, those who *don't* care about Lispisms like macros *do* create
specific, non-general surface syntaxes (e.g. C, Go, Haskell, Java, Dart, Rust,
etc.), yet to a first approximation they *don't* particularly care about
s-expressions. Rather than treating their surface syntax as a human-friendly
representation of some underlying, canonical s-expression-like parse tree,
instead the human-readable syntax is treated as the canonical form and things
like parse trees are treated as machine-friendly representations.

Unfortunately this can lead to a lot of duplicate work, as each tool (e.g. GHC,
Hugs, HLint, Haddock, hindent, etc.) operate directly on the human syntax, each
accepting slight variants of the language, with language updates breaking old
tools, ambiguous parses depending on language version, unnecessary fragility
(e.g. parsing the whole language just to find calls matching `map _ (map _ _)`),
etc. with the lowest common denominator being the compiler, i.e. if the compiler
can't parse it then the code's wrong, if some other tool can't parse it then
that tool's wrong.

Note that, as I've said, such automation, tooling, etc. *doesn't* have to work
"inside" the language, i.e. with a macro system: s-expressions don't imply Lisp;
you can make an s-expression language without macros, without eval, with
whatever approach to syntax, typing, etc. you like since it's just a
representation which can be used for syntax trees.

Note that "lisp-flavoured-X" isn't a solution to this problem, since their lispy
features only work on the lispy code; the existing ecosystem is just a mess of
strings.
