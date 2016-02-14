---
title: MELPA breakage
---

I'm not sure how long this has been an issue, but I recently started getting an error when loading Emacs's package manager (`M-x list-packages`), similar to [this one](http://lists.gnu.org/archive/html/bug-gnu-emacs/2014-02/msg01646.html):

    Lisp error: (void-function package-desc-vers)

Eventually I found [a forum thread](https://bbs.archlinux.org/viewtopic.php?pid=1468945) which linked it to MELPA (a particular Emacs package repository); apparently MELPA is now enabled automatically by Emacs, whereas my configuration was still using a package to enable it. The solution, as mentioned in that thread, is to uninstall the "melpa" package. Of course, since the package manager's broken, we have to do this manually by removing its files, which in my case were `~/.emacs.d/elpa/melpa*` (ELPA is the original Emacs package repo, which MELPA was inspired by).

Once we've done this, we need to restart Emacs. However, then I was getting an error about no such package "melpa-"!

This turns out to be caused by [Emacs Prelude](http://batsov.com/prelude/), the pre-built configuration I'm using. That contains a file `~/.emacs.d/core/prelude-packages.el` which checks for a bunch of packages and installs those it can't find. Since that list included `melpa`, it was being looked for every time I started Emacs. Removing `melpa` from `package-archives` and `prelude-packages` and restarting Emacs solved it for me!
