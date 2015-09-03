---
title: Abstraction Fail
---
Loading Javascript in a Web page. Pretty high-level stuff. Isn't it fun to live in a world without endianness, memory allocation, GOTO, compilation, packets, out-of-order message arrival, unreliable streams, network addresses, filesystems and all of that other low-level crap? We can float in our clouds high above it all :)<div><br /></div><div>Except when Facebook Connect doesn't work in IE because of <a href="http://support.microsoft.com/kb/871205">an OS bug in the networking stack</a>. The network layer in Windows XP can throw away incoming data if it is a) "chunked" (ie. split into sections) and b) compressed. Since missing data causes the decompression to fail, this results in compressed data being sent to the browser, which subsequently ignores it as incomprehensible gibberish. Thus Facebook Connect didn't work. Bloody Microsoft &gt;:(</div><div><br /></div><div>If you've been hit by this, there's no solution I know of. As a workaround, you may be able to try swapping from HTTP to HTTPS or HTTPS to HTTP in the URL you're referencing, but of course that will only work if a) the server you're contacting supports HTTP and HTTPS (Facebook does, thankfully), b) the one you switch to doesn't trigger this bug as well and c) you don't give a crap about using SSL.</div>