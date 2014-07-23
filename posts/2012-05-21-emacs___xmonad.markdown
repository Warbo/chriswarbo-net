---
title: Emacs & XMonad
---
I've started using [XMonad] [1] as my window manager, but I've had to jump through a few hoops to get it playing nicely with [Emacs] [2]. They both use the idea of a "mod" key for their keyboard shortcuts; for example "mod+j" switches to the next window in XMonad, and "mod+x" opens an Emacs command prompt. The trouble is, they both define mod to be the Alt key by default!

[1]: http://www.xmonad.org
[2]: http://www.gnu.org/software/emacs/

This is simple enough to change, by making a configuration file called ~/.xmonad/xmonad.hs. Mine contains the following:

```haskell
import XMonad

main = xmonad defaultConfig
         { modMask = mod4Mask
         }
```

Restarting XMonad with Alt+q will load this new configuration file. However, it turns out that
 1) You need the ghc-xmonad-dev package installed in order to compile against XMonad, otherwise you get an error with the "import XMonad" line *even though you can import XMonad in GHCi!*
 2) You can't use the GNU gold linker, or you'll get an "unknown option" error. Uninstall the binutils-gold package if you have it.

Now the above configuration works, and XMonad is using the Super ("windows") key instead of Alt :)

PS: The reason I've started to use a tiling window manager is because I've now got 2 **huge** widescreen monitors at work. I've rotated them both to be portrait, but there's too much "screen real estate" (I hate that phrase) to keep traversing with a mouse. After trying a few tiling window managers for micro$oft windows (boo :( ) I've found [Python Windows Tiler] [3] to be the most reliable, and it works just like XMonad :)

[3]: http://code.google.com/p/python-windows-tiler/

PPS: Don't worry, I've not sold out completely. Even though my machine at work has to run a proprietary OS, I only use it as a dumb terminal to a RedHat Enterprise Linux server, using [Cygwin/X] [4] over [SSH] [5] to display the graphics locally ;)

[4]: http://x.cygwin.com/
[5]: http://en.wikipedia.org/wiki/Secure_Shell
