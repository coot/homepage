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
  navigator.serviceWorker
    .register("/sw.js")
    .then(function(reg) {
      reg.addEventListener("updatefound", function(event) {
        // event.srcElement is not supported by Firefox
        if (reg.active === null) return;

        const wrapper = document.getElementById("wrapper");
        wrapper.className = "updating";
        const info = document.getElementById("update-info")
        info.className = "show"

        let trPromise = new Promise(function(resolve, reject) {
          function trResolver (event) {
            wrapper.removeEventListener("transitionend", trResolver);
            resolve();
          }
          wrapper.addEventListener("transitionend", trResolver);
        })

        let sw = reg.installing;
        let swPromise = new Promise(function(resolve, reject) {
          sw.addEventListener("statechange", function(event) {
            // event.srcElement is not supported by Firefox
            if (sw.state == "activated")
              resolve()
          })
        })

        Promise.all([swPromise, trPromise])
          .then(function() {
            return fetch("/index.html")
              .then(function(response) {
                return response.text();
              })
              .then(function(html) {
                for (let i = wrapper.children.length - 1; i >= 0; i--) {
                  wrapper.removeChild(wrapper.children[i])
                }
                const parser = new DOMParser();
                const doc = parser.parseFromString(html, "text/html");
                const newWrapper = doc.getElementById("wrapper");
                for (let i=0; i < newWrapper.children.length; i++) {
                  wrapper.appendChild(newWrapper.children[i].cloneNode(true))
                }
                wrapper.className = ""
                setTimeout(function() {
                  info.className = "";
                }, 2000)
              });
          })
          .catch(function(err) {
            console.error(err)
            wrapper.className = ""
            info.className = ""
          });
        })
      })
      .catch(function(err) {
        console.error(err)
        const wrapper = document.getElementById("wrapper");
        wrapper.className = ""
        const info = document.getElementById("info");
        info.className = ""
        console.error("Unable to register service worker.", err);
      });
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
