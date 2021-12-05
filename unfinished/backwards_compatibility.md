---
title: The Backwards Compatibility of Text
---

I was watching some of [Matt Parker's Stand-up Maths
videos](https://www.youtube.com/user/standupmaths) recently, and a few of them
involved quirks of the modern world which are work-arounds for problems that no
longer exist. In particular, [the staggered layout of computer
keyboards](https://www.youtube.com/watch?v=Mf2H9WZSIyw&t=2s) (to avoid the
levers of a typewriter from hitting each other), and [the 29.97Hz framerate of
NTSC video](https://www.youtube.com/watch?v=3GJUM6pCpew&t=779s) (to prevent
colour information interfering with black-and-white receivers).

These reminded me of the rabbit-hole of complexity underlying one of the
seemingly-simplest data formats we have on modern computers: plain text.

## UTF-8 (modern text) ##

The standard, modern representation of text is UTF-8, which has [some cool
features](https://en.wikipedia.org/wiki/UTF-8#Comparison_with_other_encodings):

 - It's variable-width: characters can take up one, two, three or four bytes
 - The single-byte characters are exactly the same as the older ASCII standard
 - UTF-8 bytes begin either:
  - `0` (for single-byte characters)
  - `110` and `10` (for two-byte characters)
  - `1110`, `10` and `10` (for three-byte characters)
  - `11110`, `10`, `10` and `10` (for four-byte characters)
 - This pattern ensures UTF-8 text is "prefix-free" and "self-synchronising":
  - No character appears inside another
  - There's no ambiguity, e.g. between a single long character vs a pair of
    shorter ones
  - We can start reading a UTF-8 stream part-way-through (just ignore any
    initial bytes beginning `10`)
 - The first bits tell us a character's width, which is good for skipping ahead
   (`0` = skip one byte; `110` = skip two bytes; `1110` = skip three; `11110` =
   skip four)

## ASCII (older latin/"western" text) ##

ASCII encodes the latin alphabet (A-Z, upper- and lower-case) as well as digits,
some punctuation and "control codes":

 - ASCII is 7-bit
  - Bytes only standardised on 8 bits later ([mostly due to IBM using
    EBCDIC](https://www.youtube.com/watch?v=ixJCo0cyAuA))
  - Most computers prefix ASCII characters with a `0` to make 8-bit bytes
  - Such zero-padded ASCII characters are exactly the one-byte UTF-8 characters
 - ASCII is an extension of [older teleprinter/teletypewriter
   standards](https://en.wikipedia.org/wiki/ASCII#Bit_width) dating back to the
   19th century
  - Hence ASCII includes control codes like 'line feed', 'carriage return' and,
    of course, 'ring the bell'!
   - Modern computers can simulate a virtual typewriter bell, e.g. beeping,
     flashing the screen, etc. 
   - A major incompatibility between operating systems is that UNIX (Linux, OSX)
     ends lines with 'line feed', but Windows uses 'carriage return' then 'line
     feed' (i.e. the incompatibility comes from their simulated typewriters!)
 - [ASCII was often used on punched
   tape](https://en.wikipedia.org/wiki/Punched_tape#Minicomputers)
  - ASCII character `0000000` is `NUL`, used to end a string of text; i.e. stop
    reading once we hit the un-punched part of a tape
   - Fun fact: `NUL`-terminated strings lead to ["Shlemiel the Painter's
     Algorithm"](https://www.joelonsoftware.com/2001/12/11/back-to-basics)
  - [Character `1111111` is
    `DEL`](https://en.wikipedia.org/wiki/Delete_character), used to skip a
    character; i.e. we can't un-punch a tape, but we can skip our mistakes by
    punching out the rest of their spaces.

There are some good videos about
[EBCDIC](https://www.youtube.com/watch?v=FUIqtevjod4) (and [earlier
BCD](https://www.youtube.com/watch?v=RDoYo3yOL_E)) on Computerphile. Those
standards are more focused on (decimal) numerals and arithmetic, rather than
'proper' text, although they do descend from [Hollerith tabulators, which
Stand-up Maths also has a video
on](https://www.youtube.com/watch?v=YBnBAzrWeF0).

## The Teletypewriter Interface (TTY) ##

The early use of teletypewriters & punched tape doesn't just show up in modern
data formats, it's also pervasive in command-line user interfaces, modern
operating systems and programming languages:

 - Once screens replaced teletypewriters, they were used as 'video terminals',
   which *emulate* a teletypewriter; e.g. line feed shifts up the screen's
   image,    simulating a piece of paper being moved.
 - Once computers could draw 'proper' pixel-based graphics, programs like Xterm
   appeared, which are 'terminal emulators': they emulate a video terminal
   (often a VT100), which itself is an emulation of a teletypewriter!
  - Terminal emulators don't just *look* like video terminals; they're true
    emulations: we can feed them the same ASCII control codes as a real video
    terminal or teletypewriter.
  - Terminal emulators don't interact with the user directly: they attach a
    [pseudo-teletypewriter device](https://en.wikipedia.org/wiki/Pseudoterminal)
    to the operating system, which acts like a serial line!
 - The Hello World program uses `print` since it's sending real print commands
   over a (pseudo) serial connection to an (emulated) video terminal, which is
   emulating a teletypewriter device!

## Video Extensions ##

Once video terminals and terminal emulators appeared, many extensions of ASCII
appeared, to do fancy things like bold text, colours, etc.

 - Control codes for these effects are sent "in-band", which causes a garbled
   mess if the sender and receiver aren't set up the same.
 - The most widely used formatting standards are
   [ANSI](https://en.wikipedia.org/wiki/ANSI_escape_code)

We can even send image data over a teletypewriter connection! The most popular
format is [Sixel](https://en.wikipedia.org/wiki/Sixel), which is actually
control codes for the pins of a dot-matrix print head.

Hence when we show a Sixel image in a terminal emulator, we're using an emulated
serial line, which itself is emulating a telegraph line, to connect to an
emulated video terminal, which itself is emulating a teletypewriter, which
happens to be equipped with a 6-pin dot-matrix print head and a bell!
