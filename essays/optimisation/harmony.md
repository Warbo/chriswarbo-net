---
title: Harmony Search
---
## WORK IN PROGRESS ##

<div id="harmony_playfield" style="width: 500px; height: 500px;"></div>
<form action="#" type="get">
<div>
  Best fitness so far: <a href="#" id="harmony_fitness_display"></a>
</div>
<div>
  Fittest program: <a href="#" id="harmony_winner"></a>
</div>
<div>
  Current phase: <a href="#" id="harmony_phase">0</a>
</div>
<div>
  Current machine size: <a href="#" id="harmony_m">1</a>
</div>
<div>
  Current enumeration: <a href="#" id="harmony_this_enum">0</a> (Max so far: <a href="#" id="harmony_enum">0</a>)
</div>
<div>
  <label for="#fill">Fill memory?</label><input type="checkbox" id="fill" checked="checked" />
</div>
<div>
  <label for="hms"></label><input type="range" min="1" max="100" value="" id="hms" />hms
</div>
<div>
  <label for="hmcr"></label><input type="range" min="0" max="10" value="" id="hmcr" />  hmcr
</div>
<div>
  <label for="par"></label><input type="range" min="0" max="10" value="" id="par" />  par
</div>
<div>
  <label for="fw"></label><input type="range" min="0" max="10" value="" id="fw" />  fw
</div>
</form>
<script src="/js/jquery.js"></script>
<script src="/js/jquery_svg.js"></script>
<script src="/js/underscore.js"></script>
<script src="/js/optimisation/harmony_ui.js"></script>
