---
title: Project Skeleton Creation
---

We have commands like `cabal init`, and similar for other languages and
frameworks, which set up a project with the required dependencies, etc. needed
to use that tool/framework.

Can we do something more generic, for some arbitrary project?

I've been using a little script called `trackGit` to add projects to my git
infrastructure (pushed to my laptop, mirrored on my server, github and IPFS),
so I though that was a decent place to add more functionality.

The extra features I'd like include:

 - Local and remote bare git clones (`trackGit` already does this)
 - IPNS key generated, installed and backed up
 - Continuous integration, e.g. a `release.nix` containing `{}`
 - Declarative continuous integration config, e.g. JSON for Hydra
 - ASV benchmarking setup, with `benchmarks` dir, `default.nix` providing a
   wrapped `python` and an `installer` in the config which uses it.

Not sure what the best interface to some of this might be.

I've debated making a Nix function which spits out an asv.conf.json file, etc.
but I'm not sure if that's too practical. asv-nix already runs benchmarks using
Nix sandboxes, so there seems little point sandboxing the asv command itself.
