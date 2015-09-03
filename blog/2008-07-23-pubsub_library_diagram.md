---
title: PubSub Library Diagram
---
<a href="http://svgopen.steltenpower.com/2004/svgproxy.php?url=http%3A%2F%2Fwww.freewebs.com%2Fchriswarbo%2FTemporary%2FPictures%2Fpubsub%255Fdiagram.svg">Here's</a> a diagram showing how the library is working at the moment (embedding doesn't work properly in Blogger since it is HTML rather than XHTML :( )<br /><br />As you can see, the application doesn't need to bother doing much. It creates a PubSubClient object like so:<br /><br />self.client = pubsubclient.PubSubClient("warbo@jabber.org", "I'mnotgivingoutmypassword")<br /><br />Then connects to the server:<br /><br />self.client.connect()<br /><br />Now it can run any methods it likes, for example:<br /><br />self.client.subscribe_to_node('blogger.com', 'seriously-this-is-not-worth-reading')<br /><br />This way, however, the program will not receive any notification when a reply comes back. To handle a reply, a function must be passed to the method. For example:<br /><br />def subscription_replied(self):<br />   self.replied = True<br /><br />self.client.subscribe_to_node('blogger.com', 'seriously-this-is-not-worth-reading', subscription_replied)<br /><br />At the moment the callback method doesn't have any arguments (the 'self' argument is special in Python and isn't given when the method is run). This might change, but I would prefer to do it in a generic way which I can't think how to do at the moment. To send data from the library to the application there is an XML tree called server_properties in the PubSubClient class, but I would like to make some getter and setter methods when the core functionality is done.