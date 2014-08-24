---
title: Enumerated maze
---

<div id="maze" style="width: 500px; height: 500px;"></div>
<form action="#">
  <label for="difficulty">Easiness: <a id="difficulty_display">20</a></label>
  <input type="range" min="1" max="20" value="20" id="difficulty" />
</form>
{$JAVASCRIPT_INCLUDE,JAVASCRIPT_UNDERSCORE}
{$JAVASCRIPT_INCLUDE,JAVASCRIPT_JQUERY_SVG}
{$JAVASCRIPT_INCLUDE,JAVASCRIPT_MAZE}
{$JAVASCRIPT_INCLUDE,JAVASCRIPT_MAZE_ENUM}
