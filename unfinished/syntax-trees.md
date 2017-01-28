---
title: Standard Syntax Trees
---
I certainly think that language definitions should include a standard parsed format in addition to their human-friendly syntax. This can just be an s-expression representation of the normal, concrete syntax, without any transformations applied. This way, we don't have to build language-specific rules for precedence, offside/significant-whitespace, fixity, etc. into every tool for that language.

Implementations (compilers and interpreters) should include tools/modes for converting between these two representations, and should support running programs written in the parsed format in addition to those written in the human-friendly syntax. This isn't asking much: the difficult part is parsing the human-friendly syntax, which implementations must already do.

The benefit is that we don't end up with a mismatch between what the compilers/interpreters accept, and what the other tooling accepts (linters, formatters, doc generators, static analysers, search engines, syntax highlighters, use-finders, go-to-definition, refactoring tools, etc.).

This would also make it much easier to make new tools, extend existing ones, and improve practices, e.g. like syntax-aware diffing, standard code formatting (i.e. tools like this one), version-control-friendly representations, tree-based editors, more powerful navigation in editors and IDEs, etc.

In this regard, I think that the overwhelming rejection of s-expressions by developers has thrown the baby out with the bath water: they're really good for manipulating code programatically, which makes many tools like this much simpler and more reliable to build, and can quickly bootstrap a powerful ecosystem of tooling around a language (e.g. think of all the tooling which comes "for free" if you define a new data format as JSON or XML). Just because someone doesn't want to read and write code as s-expressions doesn't mean their compilers, interpreters, IDEs, VCSs, etc. can't use them.

I think another contributor to the status quo has been the conflation of s-expressions with Lispisms like macros, eval, quasiquoting, dynamism, etc. when they're actually quite orthogonal ideas. It seems like Lisp *requires* s-expressions (or equivalent), since macros allow ad-hoc syntax; Lispers, who *do* care about s-expressions, are justifiably *against* compiling some alternative syntax down to s-expressions: it either means giving up some macro abilities, or being limited to minor cosmetic changes (e.g. sweet expressions). Hence they

On the other hand, those who don't care about Lispisms like macros *do*

------------------------------------------------

The beauty of using s-expressions for a language is that you automatically get all of these alternative representations for free: programmers just need to add the relevant foo->s-expression preprocessor to their Makefile, or write a bash alias, or use an alternative shebang, or whatever. In principle, the same thing can also be applied to output like error messages, etc. if they were represented structurally.

The language itself only needs to support one representation, although multiple could be included, e.g. via a flag; it just-so-happens that 's-expressions with parentheses' is the most common choice, so there's no real need to change it.

It's also easy enough to create a custom, non-general syntax (i.e. something less general than s-expressions/sweet expressions/etc.) that parses into the s-expression representation of some particular s-expression language.

In general you can't use an off-the-shelf syntax like, say, Python, because its s-expression representation would have different tokens and structure than that expected by the compiler, and trying to fix it up by inspecting tokens breaks the "reading without parsing" idea. But it's easy enough to make syntaxes which are "Python-like", "C-like", etc. which do parse into correct programs; especially if the s-expression language lets you transform the results using macros.

I think the only reason this isn't done more is that s-expressions and Lisps are too tightly coupled in programmers' minds. Sweet expressions are a nice reminder that Lisp doesn't imply s-expressions, but I think it's even more important that s-expressions don't imply Lisp; you can make an s-expression language without macros, without eval, with whatever approach to syntax, typing, etc. you like since it's just a representation which can be used for ASTs.

As far as I'm aware, Lispers must stick to a general, s-expression-equivalent syntax, since more specific syntaxes can't represent all of the structures which a Lisper may want to implement with macros (I may be wrong, but I've not seen any syntax which can represent all Lisp programs, but isn't equivalent to s-expressions).

Non-Lispers who don't need such generality do create specific, non-general surface syntaxes, like Haskell, Go, Dart, etc. Yet, to a first approximation, they don't particularly care about s-expressions. Rather than treating their surface syntax as a human-friendly representation of some underlying, canonical s-expression-like parse tree, instead the human-readable syntax is treated as the canonical form and things like parse trees are treated as machine-friendly representations.

Unfortunately this can lead to a lot of duplicate work, as each tool (e.g. GHC, Hugs, HLint, Haddock, hindent, etc.) operate directly on the human syntax, each accepting slight variants of the language, with language updates breaking old tools, ambiguous parses depending on language version, unnecessary fragility (e.g. parsing the whole language just to find calls matching map _ (map _ _)), etc. with the lowest common denominator being the compiler, i.e. if the compiler can't parse it then the code's wrong, if some other tool can't parse it then that tool's wrong.

Whilst I accept that many people don't want to write code directly as s-expressions, I do think it would be an improvement if the first step in such language's 'build pipeline' translates into some s-expression format (or equivalent, but might as well standardise), in such a way that it can be manipulated arbitrarily by intermediate steps (i.e. fragments of optimised AST scattered around some compiled binary's memory layout don't count!)

---------------------------------------------------------------------------



    So the missing link is an AST->s-expr generator?

This part is actually pretty trivial, as long as the AST datatypes allow generic programming. For example, I wrote a Core->s-expression pretty-printer quite easily, despite having no prior experience with generic programming.

    Well, we already have the Haskell->AST parser, don't we?

Not really. We have haskell-src-exts, which is very flexible and does a decent job of parsing "pristine" Haskell code, but it's not suitable for much Haskell code "in the wild", in git repos, Hackage tarballs, etc., since we don't know what language extensions, preprocessors, etc. must be used (CPP is common for cross-version compatibility).

We also have the GHC API, but that's even more picky about the exact details of the code (e.g. setting all the right DynFlags, to avoid crashing with "the impossible happened").


-------------------------------------------------------------------------



    I totally want a lispy flavor of Haskell with a solid lispy macro system, but for me it needs to be basically just a preprocessor that ultimately feeds into ghc.

The problem with lisp flavoured layers on top of other systems is that the lispy features only work on the lispy code; the existing ecosystem is just a mess of strings. At the moment, it's far too difficult to even parse Haskell source code into an AST (e.g. due to widespread use of preprocessors, which rely on Cabal to invoke them), let alone query or manipulate it.

I'd rather have the opposite system to what you describe: have GHC accept (annotated) AST s-expressions, and implement Haskell as a preprocessor which just parses the familiar syntax into s-expressions. This way, we can use the Haskell->s-expression preprocessor to query/manipulate any Haskell code, not just some lispy sub-set, and we can send all sorts of programatically-generated ASTs straight into GHC.
