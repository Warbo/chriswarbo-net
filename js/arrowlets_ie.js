
if (base2.detect("MSIE")) {
    base2.JavaScript.bind(window);
    base2.DOM.bind(document);
    document.extend({
        "@MSIE": {
            getElementById: function(id) {
                return base2.DOM.bind(this.base(id));
            }
        }
    });
    if (!window.XMLHttpRequest) {
        window.XMLHttpRequest = function() {
            return new ActiveXObject("MSXML2.XMLHTTP.3.0");
        }
    }
    document.addEventListener("DOMContentLoaded", function() {
        document.querySelectorAll("script.show").forEach(function(script) {
            var div = document.createElement("div");
            div.className = "script " + script.className;
            div.innerText = script.text;
            div.setAttribute("style", script.getAttribute("style"));
            script.parentNode.insertBefore(div, script.nextSibling);
        });
    }, false);
}