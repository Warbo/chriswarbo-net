---
title: Quantum Encryption
---

I enjoy watching
[PBS Spacetime](https://www.youtube.com/channel/UC7_gcs09iThXybpVgjHZ_7g), and
their recent
[video on quantum cryptography](https://www.youtube.com/watch?v=pi7YwxxZQ5A)
had some interesting stuff (especially the way BB84 and entangled particles are
used for key sharing; the random choice of measurement basis was the "trick"
that I hadn't realised before).

However, some of the computer science was a little off though, especially some
mixing up between symmetric and asymmetric encryption, e.g. talking about
"sharing a private key" ("private" means never shared).

Symmetric encryption uses the same key to encrypt and decrypt, like a lock which
needs the key to be engaged or released. This requires both parties to have the
same key, hence it's a "shared key" (or "shared secret"). The words "public" and
"private" don't apply here.

Asymmetric encryption uses different keys for encrypting and decrypting. The
encrypting key is called "public" since it doesn't need to be secret; it's like
a padlock that anyone can use to lock a package. The decrypting key is called
"private" since we never need to share it with anyone, ever; it's like the key
which opens a padlocked package.

Even though private keys are never shared, asymmetric encryption is still
vulnerable to a man-in-the-middle when the public keys are obtained.

As comments on the video have said, prime factoring isn't particularly special;
any "one way function" (easy to do but hard to undo) will work. A bunch of
alternative methods have been invented which seem to withstand quantum
computing.

Plus, I'd also point out that AFAIK there's no proof that quantum computers are
faster than classical computers at any task (the
["quantum supremacy" problem](
https://en.wikipedia.org/wiki/Quantum_supremacy)).
We know that prime factorisation is easy for quantum computers (using
[Shor's algorithm](https://en.wikipedia.org/wiki/Shor%27s_algorithm)); but we
don't know whether it's actually hard for a classical computer, or if we just
haven't yet found the "trick" to making it easy. In other words, prime
factorising *might* be broken in many ways; quantum computing is just the only
way we know at the moment.

In fact, we *might* even discover a fast classical algorithm for simulating a
quantum computer; in which case quantum and classical computers would be
equivalent (although one may be more practical or efficient to build & operate).
