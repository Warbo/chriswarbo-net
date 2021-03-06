---
title: A Decent Compressed Filesystem At Last?
---
I have a 2.4GHz CPU, a 250GB hard drive and I want to store
[29GB of zipped music I just downloaded](http://tracker.modarchive.org/). The
music is in a variety of ProTracker formats, so what's the best way to store it?

I know for a fact that tracker songs are very easy to compress, since the format
originates on computer systems like the Amiga where any compression more
sophisticated than Run Length Encoding takes away valuable clock cycles which
are often needed elsewhere. Such uncompressed, or crudely compressed, data is
ripe for extra compression (hence why it is transported in a zip archive).

Actually playing this music is now nontrivial though, since whilst all of my
music players support ProTracker, not all of them support reading them from zip
files. The obvious thing to do is decompress them, but since trackers are highly
compressible, this means that the 29GB archive will certainly become even more
vast when decompressed, and that's just a waste of storage space. The solution
is to use a compressed filesystem.

For those who are stuck on shit operating systems, I'll give you a little
insight into how your computer works. Let's say we're running Microsoft Windows,
and we insert a CD into our CDROM drive. In "My Computer" we may see our CDROM
is called "D", and if we double click on it we see the things stored on it.
However, we then go back to My Computer and right click on D, then tell it to
eject. We take out the CD and close the drive. Now we tell it to eject again,
and the drive opens. So, what is "D"? It's our CD, since we saw the files on it,
but it's also the drive, since we can tell it to do things when our CD is
nowhere in sight. Which is it really? Well, that depends completely upon what
you want to do, since Windows is a very poor system indeed.

Let's imagine a similar thing on a UNIX system like Debian. Our CDROM will be
something like /dev/hda. We can send it commands, for example "eject /dev/hda"
(or do the same thing in whichever graphical environment you happen to use). Can
we access the files on a disc through /dev/hda? No. What we can do, however, is
take the data given to use by the drive, and reconstruct it somewhere. This is
what we mean by mounting the filesystem. The filesystem is the particular way
the ones and zeroes represent our files and folders (CDs use the standard
ISO9660, which is often extended with certain proprietary formats from Microsoft
to allow longer filenames. Ironic, considering that the original format only had
short 8.3 filenames to make sure it would work on Microsoft's POS crapware),
whilst mounting it means making it available to peruse. Here we can choose where
we want to see things, so we can run a command like "mount /dev/hda /our_cd".
Now if we go to the folder /our_cd the previous contents will be hidden (until
the CD is unmounted) and the contents of the disk is accessible. /our_cd is a
different file to /dev/hda. The same is true of hard drives.

In fact, filesystems can be anything. The FUSE (Filesystems in USErspace) driver
allows regular programs to be written which can be accessed like filesystems.
This is how the Wikipedia filesystem works (mount it somewhere and that folder
becomes filled with files for each article), how the GMail filesystem works
(data is stored online as a series of emails in a GMail account, which can be
retrieved from anywhere and are automatically converted back to their original
state), and many others. There are several FUSE filesystems which transparently
compress and decompress their contents, ie. everything copied into the
filesystem is sent to a compression program and saved somewhere, and everything
read from the filesystem is sent through a decompression program before it
reaches the destination. Thus any program, as long as it can load files, can
load compressed files, and any program, as long as it can save files, can save
compressed files.

Seems cool, but until recently the only ones I'd tested were pretty dire, most
noticably CompFUSEd. INCREDIBLY slow and memory hungry, it was not worth using,
and that was only when a few MB were put into it.

However, FuseCompress has recently been added to Debian, and I'm trying it out
for these tracker modules. Whilst populating the filesystem is taking a while
(I'm having to decompress all 122 thousand songs (since that's how we want them
to be read and therefore written), then move them into the filesystem where
they're recompressed (although using LZO this time, which is damned fast). All I
can say is thank Guido for Python, since it makes automating such things a
breeze :)

I thought I'd finish with a little introduction to compression, and what it
actually is. Compression is completely based on Claude Shannon's Information
Theory (also the basis of using switches to represent Boolean logic, ie.
allowing a physical way of building Alan Turing's theoretical "computers").
Information, measured in bits, is irreducible, you can't throw away any bits
without losing some information (compression which does this is called "lossy"
compression, for example the Vorbis audio codec. However, the algorithms are
crafted in such a way that information is only thrown away when it is
imperceptible to us, in the case of Vorbis we can't hear the difference), but
the key thing to know is that each bit of information is not necessarily mapped
one-to-one with each bit of whatever material is being used to store it (eg.
magnetic domains on a hard drive). A bit of information is required to describe
a situation where there is a 50/50 chance of the next bit being 0 or 1, but this
is only true for random sequences, or those which appear random. A sequence like
"AAAAAAAAAAAAAAAAAAAA" is not random (each bit isn't independent of the
previous), and thus does not require all of the 160 bits that are being used to
describe it. Information theory gives us a lower limit, saying how many bits are
REQUIRED to store the given information, so we want our algorithms to approach
this limit as much as possible.

A really simple type of compression is Run Length Encoding. This looks for
repetition and replaces it with multiplication. For example, the sequence
"ABCBAAAAAAAAABBCCCCCCCCDBDDDDB" could be compressed to "ABCB9ABB8CDB4DB". Our
algorithm here is simply "if you find more than 2 of the same letter in a group
then replace that group with its size followed by the letter", so a stream of
"AAAAAAAAA" becomes "9A". To decompress this we use the algorithm "If you find a
number, put that many of the next letter".

There is a slightly more general form of this, where instead of grouping similar
things, we use a pointer. The pointer is a number which means "I am the same as
whatever is this far behind me". In this way "AAAAAAAAA" can become "A12345678",
where each number is telling us to get the next value from this far back (they
all point back to the first A), however pointers can also point to pointers, so
we could just as easily put "A11111111", since each of those "1"s becomes an
"A", so it's perfectly valid for the next one along to point to it. This is
easily compressible with the runlength encoding seen above (which is just a
subset of this form of compression), but is more powerful. For example
"ABABABABABABABABABABABA" cannot be compressed with runlength encoding, but
using pointers we can compress it to "AB222222222222222222222". Now this is
easily compressible. This works for any size of pattern, although since it is a
search operation it can get quite slow for large search spaces, so files are
usually split into more manageable chunks first.

A final form of compression I'd like to mention involves binary trees. Let's say
we made a survey of the occurances of every letter in a file. We could say, for
example, that "e" was the most common, followed by "a", followed by "s" and so
on. Now we can compress these from their usual 8 bits to a much more compact
form. First we define a binary tree, that is a tree where every non-leaf node
(branch junction) has a left and a right branch. If we meet a "0" we will go
down the left branch and if we meet a "1" we will go down the right branch. Our
tree will begin with two branches, and we can stick "e" at the end of the right
branch. On the left branch we put another node with two children, on the right
we put "a" and on the left we put another node with two children. On the right
of this we put "s" and on the left another node with two children, and so on.
Now we can replace each letter by the path we must take in our tree to reach it
(with our tree we know that "1" marks the end of a letter). Every time we find
an "e" we simply put a "1", since that's how we get to "e" from the top of the
tree (1 means go right) This saves 7/8 of the space every time. Every time we
find an "a" we replace it with "01", which saves us 3/4 of the space, an "s"
with "001", and so on. By constructing optimised trees (which is once again a
search operation) we can get really good compression ratios.

Anyway, rant over since my script has finished moving all of the "A"s :D
