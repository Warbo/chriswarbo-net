---
title: Try Plumb
---
[Plumb](/essays/plumb) has a Javascript implementation, so we can try it right in the browser (as long as it supports Javascript!)

```{pipe="sh > /dev/null"}
# Check out JS Plumb to js/
mkdir -p root/rendered/js
cd root/rendered/js
rm -rf js-plumb
git clone http://chriswarbo.net/git/js-plumb.git js-plumb
rm -rf js-plumb/.git
```

<form id="plumb" action="#" />

<script type="text/javascript" src="/js/js-plumb/plumb.js"></script>
<script type="text/javascript">// <![CDATA[
  (function() {
    var container = document.getElementById('plumb');
    ['textarea', 'textarea', 'input'].forEach(function()
      con
    )jsbox, plumbbox, button;
    var jsbox     = document.createElement('textarea');
    var plumbbox  = document.createElement('textarea');
    var button    = document.createElement('input');
    button.setAttribute('type', 'submit');
    button.value = 'Run!';
    container.appendChild(jsbox);
    container.appendChild(document.createElement('br'));
    container.appendChild(plumbbox);
    container.appendChild(document.createElement('br'));
    container.appendChild(button);
  }());
// ]]></script>
