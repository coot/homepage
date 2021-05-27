const workbox = require("workbox-build")

workbox.injectManifest({
        globDirectory: "dist",
        globPatterns: [
            "assets/*.{css,js}",
            "images/*.{svg,jpg,png}"
        ],
        swSrc: "sw.js",
        swDest: "dist/sw.js"
    })

