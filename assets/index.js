// Polyfill for `NodeList.forEach`
if (window.NodeList && !NodeList.prototype.forEach) {
    NodeList.prototype.forEach = function (callback, thisArg) {
        thisArg = thisArg || window;
        for (var i = 0; i < this.length; i++) {
            callback.call(thisArg, this[i], i, this);
        }
    };
}

// Service worker
if (navigator.serviceWorker) {
  navigator.serviceWorker.register("/sw.js")
}

function copyChildren(target, node) {
    var node = document.importNode(node, true);
    node.childNodes.forEach(function(child) {
        target.appendChild(child.cloneNode(true));
    });

}

// Load imports into html
document.addEventListener("DOMContentLoaded", function() {
    var links = document.querySelectorAll('link[rel="import"]')
    for (var i = 0; i < links.length; i++) {
        var link = links[i];
        var target = document.querySelector(link.getAttribute("data-target"));
        var cls_ = "";
        switch(location.pathname) {
            case "/about.html":
                cls_ = "about";
                break;
        }
        if (cls_) {
            target.setAttribute("class", target.getAttribute("class") + " " + cls_)
        }
        if (link.import) {
            // only supported by Chromium
            copyChildren(target, link.import.body);
        } else {
            // fetch the content
            fetch(link.getAttribute('href'))
                .then(function(resp) {return resp.text()})
                .then(function(html) {
                    var parser = new DOMParser();
                    var doc = parser.parseFromString(html, "text/html");
                    copyChildren(target, link.import.body);
                });
        }
    }
});
