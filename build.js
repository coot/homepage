const workboxBuild = require("workbox-build");

workboxBuild.injectManifest({
  "globDirectory": "./",
  "globPatterns": [
    "header.html",
    "index.html",
    "about.html",
    "posts/*.html",
    "images/*.{svg,jpg,png}",
    "assets/*.{css,js}",
    "bower_components/html5shiv/dist/html5shiv.min.js",
    "bower_components/html5shiv/dist/html5shiv-printshiv.min.js",
  ],
  "swSrc": "./dev/sw.js",
  "swDest": "./sw.js",
  "globIgnores": []
})
