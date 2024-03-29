---
title: PubSubClient has a home
---
I set up a Gitorious repository for PubSubClient a while ago, but couldn't
actually put any code in there because my SSH key wasn't working with the pass
phrase I remembered (ie. I forgot it :P ). Now I've revoked that key and added a
new one, so I can upload again :D ~You can find the complete source there~
**Gitorious is no more, so I'm now hosting my own [git
repositories](/git/pubsubclient)**. I've also created a [Sourceforge
project](https://sourceforge.net/projects/pubsubclient/) for it, so that I can
give it a free website (Google Code hosting doesn't allow GNU Affero GPL
licensed software :( ). The project is still pending approval, but I feel
confident that it'll get approved since it is pretty novel.

The repository contains the library in its current form, as well as the
ever-improving "browser" test, the currently-stalled "reader" test and the
not-really-started-yet "writer" test. To use the library itself all you need is
lxml and xmpppy, then just stick the "pubsubclient.py" file in the same folder
as your application and you're ready to go. The tests make use of some other
libraries, since I wanted to demonstrate that it is easy to integrate into any
existing toolkit. To use the browser you'll need pygtk and kiwi, the reader
needs pygtk, kiwi and webkit, whilst the writer isn't worth bothering with
ATM.

Have fun :D

PS: In other news Jabber Inc. have been bought by Cisco, which is pretty
cool. There's [some
confusion](http://www.techcrunch.com/2008/09/19/cisco-acquires-jabber-for-enterprise-im/)
though so remember, "Jabber" is just a nickname for the messaging protocol. Its
actual name is XMPP (eXtensible Messaging and Presence Protocol), since it may
have originally been created by Jabber Inc. a decade ago, but for the past few
years it's been looked after by the XMPP Standards Foundation. Jabber Inc. (who
own jabber.com but NOT jabber.org) can of course add to and change the standard,
but they must follow the same proccess as Google, Microsoft, Facebook or anyone
else who wants to do so. In other words: This is a Good Thing(TM) since XMPP is
being adopted heavily by a huge firm like Cisco (who previously used the SIMPLE
messaging standard, which is anything but), and the XMPP standards are still
being kept safe by XSF as they were already. Rock on!
