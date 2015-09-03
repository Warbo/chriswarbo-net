---
title: QEdje Is Working! :D
---
There are quite a few <a href="http://en.wikipedia.org/wiki/Free_software">Free Software</a> libraries out there for making <a href="http://en.wikipedia.org/wiki/Graphical_user_interface">GUIs</a> (graphical user interfaces). The most famous are probably <a href="http://en.wikipedia.org/wiki/GTK%2B">GTK+</a> (started by the <a href="http://en.wikipedia.org/wiki/GIMP">GIMP</a> and used by <a href="http://en.wikipedia.org/wiki/GNOME">Gnome</a>, <a href="http://en.wikipedia.org/wiki/Xfce">XFCE</a> and <a href="http://en.wikipedia.org/wiki/Lxde">LXDE</a>) and <a href="http://en.wikipedia.org/wiki/Qt_%28toolkit%29">QT</a> (started by <a href="http://en.wikipedia.org/wiki/Trolltech">Trolltech</a> and used by <a href="http://en.wikipedia.org/wiki/KDE">KDE</a>), however there are quite a few more such as <a href="http://en.wikipedia.org/wiki/Gnustep">GNUStep</a>, <a href="http://en.wikipedia.org/wiki/Motif_%28widget_toolkit%29">Motif</a> (used by <a href="http://en.wikipedia.org/wiki/Common_Desktop_Environment">CDE</a>), <a href="http://en.wikipedia.org/wiki/Fltk">FLTK</a> (used by <a href="http://en.wikipedia.org/wiki/EDE">EDE</a>) and <a href="http://en.wikipedia.org/wiki/Enlightenment_Foundation_Libraries">EFL</a> (used by <a href="http://en.wikipedia.org/wiki/Enlightenment_%28window_manager%29">Enlightenment</a>).<br /><br />EFL, the Enlightenment Foundation Libraries, are particularly interesting. They are incredibly lightweight, running quite happily on a mobile phone, yet allow all sorts of animation (as in, proper 2D animation rather than just moving/spinning/stretching things) and are completely themable. The way this works (from what I can find out) is that every EFL program uses the <a href="http://en.wikipedia.org/wiki/Enlightenment_Foundation_Libraries#Evas">Evas</a> canvas ("canvas" is the name given to a widget which allows arbitrary drawing on top, rather than imposing some kind of structure), then <a href="http://en.wikipedia.org/wiki/Enlightenment_Foundation_Libraries#Etk">Etk</a> and <a href="http://en.wikipedia.org/wiki/Enlightenment_Foundation_Libraries#EWL">EWL</a> draw on top (the canvas is created implicitly by Etk and EWL, even if you don't make one explicitly). This is opposite to most toolkits, like GTK+ for example, where the widgets are drawn in the window (which is usually divided up into a rigid structure) and canvases are implemented as widgets.<br /><br />A nice feature of the EFL is called <a href="http://en.wikipedia.org/wiki/Enlightenment_Foundation_Libraries#Edje">Edje</a>. Edje allows an application to be written without worrying about the GUI, instead just requiring that an external file be called. These external files describe the interface, and are entirely <a href="http://en.wikipedia.org/wiki/Declarative_programming">declarative</a> (that is, they say "I want a button" rather than "This is how to draw a button"), think of it like the HTML of a web page, with Edje being the web browser which draws it (actually, this would be a more appropriate description of <a href="http://en.wikipedia.org/wiki/XUL">XUL</a>, but I can't get my head around XUL's seemingly overcomplicated use of CSS, JavaScript and XML :( ).<br /><br />Edje files are compiled into compressed archives (using <a href="http://en.wikipedia.org/wiki/Enlightenment_Foundation_Libraries#EET">EET</a>) which act like incredibly far-reaching themes. This means that a theme doesn't just contain pretty pictures to use as buttons, or the programming to draw the right lines at the right locations, it actually contains the entire user interface. To continue the web page analogy, if Gmail or Facebook used an analogous system then instead of merely being able to change the theming via CSS (which may have to be specifically forced from the browser preferences, because web-app people suck balls), you could actually use a completely different webpage to interact with the underlying application (no more <a href="http://blog.last.fm/2008/07/17/lastfm-the-next-generation">"New look!" announcements</a>, since anybody could use any look they wanted all of the time).<br /><br />Now to address the title of this post ;) As I've described, Edje is a declarative system. An awesome feature of this is that Edje can be completely replaced without the user even noticing, since the point is to say "I want a button" and not care about how it gets done. Well, the developers of <a href="http://en.wikipedia.org/wiki/Canola_%28software%29">Canola</a> looked at moving to QT, since it offers more features than EFL, is more widely developed, developed for and installed. However, they found that Edje was too awesome to leave behind, so they ported it to QT and called it <a href="http://codeposts.blogspot.com/2008/08/presenting-qedje.html">QEdje</a>! What's particularly nice about QEdje is that a) the canvas used instead of Evas, called QZion, is rather abstract in itself, so that different QT systems can be used to do the work (eg. direct drawing to the screen with the QPainter backend, more abstract layout with the QGraphicsView backend or 3D accelerated with the KGameCanvas backend, depending on the environment it is being used in) and b) the huge wealth of QT widgets can be used in the program (this is pretty powerful considering that as well as buttons, labels and tickboxes, QT also has <a href="http://labs.trolltech.com/page/Projects/Internet/WebKit">a whole web browser</a>, <a href="http://labs.trolltech.com/blogs/2008/08/06/opendocument-format/">Open Document Format compatible rich text areas</a> and an abstracted <a href="http://doc.trolltech.com/4.4/phonon-module.html">audio/video multimedia engine</a> (which, so far, uses Gstreamer (Rhythmbox, Totem, etc.), Xine (GXine, xine-ui), VLC, Mplayer, QuickTime (QuickTime Player, etc.) on Mac and DirectShow (Windows Media Player, Media Player Classic, etc.) on Windows).<br /><br />After a little wrangling I've got it to compile and the examples are working. This means I can have a play about, so I'll report on my findings :)