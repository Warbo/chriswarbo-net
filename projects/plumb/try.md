---
title: Try Plumb
---
[Plumb](/projects/plumb/index.html) has a Javascript implementation, so we can try it right in the browser (as long as it supports Javascript!)

```{pipe="sh > /dev/null"}
# Check out JS Plumb to js/
mkdir -p root/rendered/js
cd root/rendered/js
rm -rf js-plumb
git clone http://chriswarbo.net/git/js-plumb.git js-plumb
rm -rf js-plumb/.git
```

<form id="plumb" action="#" />

<div id="description" style="display: none;">

To try out Plumb, you can write Javascript code in these boxes. For security reasons, only code following the [JSON](https://tools.ietf.org/html/rfc7159) sub-set of Javascript will be accepted, although Plumb itself can work with arbitrary code.

The code in the top box will be given interpreted by `plumb` to produce a Javascript function. The contents of the second box will be sent as an argument to this function, and the return value will appear in the bottom box.

</div>

<script type="text/javascript" src="/js/js-plumb/plumb.js"></script>
<script type="text/javascript">// <![CDATA[
  (function() {
    var container = document.getElementById('plumb');
    var   funcbox = document.createElement('textarea');
    var    argbox = document.createElement('textarea');
    var resultbox = document.createElement('textarea');
    var    button = document.createElement('input');

    button.setAttribute('type', 'submit');
    button.value = 'Run!';

    button.onclick = function() {
      resultbox.value = plumb.plumb(JSON.parse(funcbox.value))
                             .apply(null, JSON.parse(argbox.value));
    };

    document.getElementById('plumb').style.display = 'block';

    container.appendChild(funcbox);
    container.appendChild(document.createElement('br'));
    container.appendChild(argbox);
    container.appendChild(document.createElement('br'));
    container.appendChild(resultbox);
    container.appendChild(document.createElement('br'));
    container.appendChild(button);
  }());
// ]]></script>
