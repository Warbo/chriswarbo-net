---
title: Try Plumb
---
[Plumb](/plumb) has a Javascript implementation, so we can try it right in the browser (as long as it supports Javascript!)

<form id="plumb" action="#" />

<script type="text/javascript" src="/data/js-plumb/plumb.js"></script>
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
