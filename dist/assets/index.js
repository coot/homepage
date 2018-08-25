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
