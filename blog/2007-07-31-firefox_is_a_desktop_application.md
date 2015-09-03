---
title: Firefox is a desktop application
---
<span style="font-family: arial;">There seems to be a continuing theme here at GUADEC. Someone gets up on stage and declares that they know how to improve GNOME fantastically. They then go on about the web, and how GNOME should become Firefox-and-that's-it. OK, I have a real problem with this; Not only is Firefox NOT a part of GNOME (I'd prefer GNOME turning into Konqueror-and-that's-it to be honest. Even Epiphany is getting WebKit support, because Mozilla software is notoriously slow, hugely resource hungry, and doesn't actually follow web standards that well. OK, they are trying, unlike Idiot Exploiter which does whatever the hell it wants and says "Well, I pwn teh Internets, so if your site doesn't work in me that's YOUR problem.", but the implementation they have is incomplete to say the least. Just try viewing an animated SVG. Painful isn't it?), but desktop applications (GNOME applications included) are far more powerful than web applications, which can be proved by simple logic: Firefox is a desktop application!</span><p style="font-family: arial;"><br /></p><p style="font-family: arial;"><br />Seriously, if you think websites are the new, hip, groovy, trendy way of improving more people's lives with better computers and software then that's fine. But we shouldn't throw out everything we have in order to accommodate that view. There are two scenarios here: Either we move completely to a web browser based system, where everything is HTML (+1), CSS (+1), Javascript (+1), AJAX (+1), etc. and we have no GTK (-1000), no QT (-1000), no EFL (-1), no TK (+1), no WX (-100), no WINE (-pi), no anything else, all of that unbelievably massive collection of software that is already here, already working, already solving problems for people and has been doing for years reliably, is thrown in the bin since it can't be used (-10000000) (except for maybe in a VNC applet, which would be such a stupid-ass thing to do that anyone thinking it should be beaten so hard with a clue bat that their hypocrisy starts dribbling out of their ears), or the other scenario is that we carry on using the desktop as it is. With this approach we can use GTK (+1), QT (+1), WX (+1), TK (-1), EFL (+1) and, get this, we can use HTML (+1), CSS (+1), Javascript(+1), AJAX (+1), etc., plus all of the current library of application will still work (+1), and another major advantage is that this scenario can be achieved with absolutely no investment of time, code or anything else, because it is here already. I am writing this blog entry in Gedit, the GNOME text editor built on GTK. I could be writing it in GNOME Blog, another nice GTK program, but there wouldn't be much point because I don't have a connection to teh Internets. So what can I possibly do about that? Nothing, it's not a problem because my work is local to my machine. When I have a connection I'll upload it, but that might not be for a while.</p><p style="font-family: arial;"><br /></p><p style="font-family: arial;"><br />So you want to make HTML/CSS/whatever applications on GNOME? It is perfectly possible to embed a webbrowser into a program, that is what a web browser actually is! Instead of putting a location bar to load arbitrary sites, whether local or remote, you can just load up a single page of your choosing, and then change the properties of the window however you want to, for example, cut out the edges. Stuart Langridge's Jackfield is an attempt at this kind of thing, and his talk was the only one involving web development which actually looked like it was a step forwards. Of course, Mirco's talk on Lowfat was great, and he envisions much of the same stuff as me, with Lowfat being a step towards document centric computing. My only complaint would be that he limits the scope a little too much by tying it to search results only.</p><p style="font-family: arial;"><br /></p><p style="font-family: arial;"><br />On the subject of Lowfat, Mirco proposed an idea about "tagging" files, which was the same kind of idea as an earlier talk. he problem with this idea of "tagging" is that it involves the very web 2.0 concept of chucking a load of random words at a file. This is not proper tagging. Proper tagging involves key:value pairs, such as "Title:Only For The Weak", "Artist: In Flames", "Album: Clayman", but arbitrary, for example "Live:No", "Favourite: No", "Score:3". I raised this point at the metadata talk, but was informed that this sort of data storage (like ID3v2 Tags) is not tagging, it is other metadata. Erm....... right.... Then Jeff Waugh came out with something pretty ridiculous, I mean, I love his green shoes and everything, but the idea of chucking random words on something and making some of them key:value pairs is completely backwards. Tagging systems apply metadata to things, which in themselves already have intrinsic properties. For example, a song has "Length", "Bitrate", "Format", "Frequency range", etc. Adding these tags works ON TOP of these intrinsic properties, to make a metadata retrieval system find structures like "Width:320", "Height:240", "Tags:Holiday, Bob, Funny", adding key:value pairs to such a system would be as ridiculous as using a GTK webbrowser to view GTK application within a VNC java applet, since it would end up with "Tags:Bob, Funny, Holiday, Width:320, Height:240" which is just completely unneeded recursion. Of course adding tags properly is not as immediate as throwing arbitrary words onto something, but it produces much better results, since words have a meaning to the system. Let's say you are looking for pictures of someone called "Cliff Edge", well you're going to get results of geographical features, whereas if you had tagged pictures with "People:Cliff Edge, Bob Doodle, Dave Daveydave", "Location:New York, Central Park, Bench" and "People: Sue Thingiebob", "Location: Cliff edge, Dover, Seaside" for example you would be able to specify that you want to look for people called Cliff Edge. Of course, as with any arbitrary metadata system, the big problems are getting the metadata in the first place (people care about data, not metadata) and making sure the key names match up. Of course, this is where most discussions fall down as nobody can decide on "the" keys to use, but with a fully arbitrary system there can just be a few suggested (but optional) tags, along with a weighted system of tag relationships to keep track of similar-yet-different keys and values. For web systems this might not be enough, but for a desktop it is perfect. Why? Well because each user has complete control over all of their data. For instance looking at Jamendo's Genre: tags is an example of collecting data about the relationships between these values (if many songs tagged with "Heavy Metal" are also tagged with "Thrash" then it is straightforward to deduce that Heavy Metal and Thrash are somehow similar, even though the two phrases, from a lexicographical perspective, are about as related as Classical and Rap. This doesn't fit into the current mandatory, non-arbitrary tag system used within audio files, so when browsing Jamendo from within Rhythmbox, which conforms to this non-arbitrary, one value system, it chooses so many unique genres as to make browsing by genre useless (I don't particularly care if it is Symphonic Melodic Doom Metal or Heavy Orchestral Death Metal if it means I cannot see related things together), however with the music on my drive (including the songs I get from Jamendo when I click on Download Album from within) I can retag myself, to change someone's pretentious "Medieval Metal" genre to a more apt and more generic "Power Metal" value. With multiple values I could use both, and with a weighted relationship system I could relate it instead. In such a world I would not narrow down the listed tracks by clicking on a specific item in a list of genres, but by selecting an area within a web of genres.</p><p style="font-family: arial;"><br /></p><p style="font-family: arial;"><br />Oh well, I suppose "Show me the code" applies here, so I'll take a look at Lowfat's code, and if that doesn't prove fruitful then I'll see what prototypes I can hack together using the Pigment libraries for Python.</p>