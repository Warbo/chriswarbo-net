Purely-Functional Self-Modifying Code
* Introduction

#+begin_src emacs-lisp
(defun compile-haskell () (shell-command "ghc *.hs"))
(add-hook 'org-babel-post-tangle-hook 'compile-haskell)
#+end_src

#+begin_src haskell :session haskell :result silent
:load selfmod.hs
:module Test.QuickCheck
:module Control.Applicative
#+end_src

#+RESULTS:

#+begin_src haskell :tangle yes
  {-# LANGUAGE RankNTypes, FlexibleContexts #-}
  module SelfMod where

  import Test.QuickCheck
  import Control.Monad.State
  import Control.Applicative

  -- Runs tests with output suitable for the Web
  check :: Testable prop => prop -> IO String
  check p = fmap output $ quickCheckWithResult stdArgs {chatty = False} p
#+end_src

One of my main research interests is self-improving code. Clearly such algorithms must be self-modifying, or else they'd never be able to perform improvements. The trouble with self-modifying code is that it tends to be implemented imperatively. Since imperative code is difficult to reason about, this makes self-improvement particularly hard.

Those rare times when self-modification is used, it's behaviour is usually known in advance, eg. for compression or to evade malware scanners without altering semantics. Those times when it's not, the runtime is sandboxed to prevent any high-risk (and potentially high-reward) operations.

I'm interested in truly novel, unpredicted, emergent modifications, but don't want to crudely restrict the available operations. Instead, I'd rather have an unrestricted language and allow any and all modifications, iff they're justified by a sound logical argument.

The most obvious way to implement such a system is via strong types. We can define a type to represent program safety and ensure our programs only propose modifications which are also safe. Clearly, such strong types are only plausible when writing purely functional code, so we hit the conundrum of how to implement self-modification in a purely-functional way.
