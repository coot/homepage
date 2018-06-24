importScripts(
    "/node_modules/workbox-sw/build/workbox-sw.js"
  , "/node_modules/workbox-routing/build/workbox-routing.dev.js"
  , "/node_modules/workbox-precaching/build/workbox-precaching.dev.js"
);

workbox.routing.registerRoute(
    new RegExp('^http:\/\/netdna\.bootstrapcdn\.com\/font-awesome\/4\.1\.0\/css\/font-awesome\.min\.css')
  , workbox.strategies.staleWhileRevalidate({
    cacheableResponse: {
      statuses: [0, 200, 304]
    }
  })
)

workbox.routing.registerRoute(
    new RegExp('^http:\/\/netdna\.bootstrapcdn\.com\/font-awesome\/4\.1\.0\/fonts\/fontawesome-webfont\.woff.*')
  , workbox.strategies.staleWhileRevalidate({
    cacheableResponse: {
      statuses: [0, 200]
    }
  })
)

workbox.routing.registerRoute(
    new RegExp('https:\/\/fonts\.googleapis\.com\/css.*')
  , workbox.strategies.staleWhileRevalidate({
    cacheableResponse: {
      statuses: [0, 200]
    }
  })
)

workbox.routing.registerRoute(
    new RegExp('https:\/\/fonts\.gstatic.com\/s\/opensans\/.*')
  , workbox.strategies.staleWhileRevalidate({
    cacheableResponse: {
      statuses: [0, 200]
    }
  })
)

workbox.routing.registerRoute(
    new RegExp('https:\/\/cdnjs\.cloudflare\.com\/ajax\/libs\/mathjax\/2\.7\.4\/MathJax\.js.*')
  , workbox.strategies.staleWhileRevalidate({
    cacheableResponse: {
      statuses: [0, 200]
    }
  })
)

workbox.routing.registerRoute(
    new RegExp('/*.html')
  , workbox.strategies.networkFirst()
)

workbox.routing.registerRoute(
    new RegExp('/posts/*.html')
  , workbox.strategies.networkFirst()
)

workbox.precaching.precacheAndRoute([
  {
    "url": "about.html",
    "revision": "7fa96c418cc8a30825e978d241694540"
  },
  {
    "url": "header.html",
    "revision": "681d0b4c1c7cda091ce2c485b591a85c"
  },
  {
    "url": "index.html",
    "revision": "d73532e519f8c29d4bd0e6554fa1dfb5"
  },
  {
    "url": "posts/adts-and-universal-algebra.html",
    "revision": "1340d052ab0931c6287d1441492585f1"
  },
  {
    "url": "posts/freeness.html",
    "revision": "cadff7e3670c395056261901de3842d4"
  },
  {
    "url": "images/indie_hosters.svg",
    "revision": "4f88c2f3ed4cd728ce40926259e9c99e"
  },
  {
    "url": "images/marcinszamotulski-250.jpg",
    "revision": "026773f54cf2287b5bd06a21bbfab74d"
  },
  {
    "url": "images/marcinszamotulski-480.jpg",
    "revision": "55cdddeea070a2520c5c8a8039fae87f"
  },
  {
    "url": "images/vim_icon_20.png",
    "revision": "bb22238b1625e6c10fc966ef30b6000a"
  },
  {
    "url": "assets/index.js",
    "revision": "f23dc1fb6ae887f9412bacdc97407c57"
  },
  {
    "url": "assets/normalize.css",
    "revision": "dca0089e5b1a1579441226245958e80f"
  },
  {
    "url": "assets/print_style.css",
    "revision": "34b729e90eadff3b019e2bead0ef4db3"
  },
  {
    "url": "assets/script.js",
    "revision": "118c50c800146611328cf17106aa8ba7"
  },
  {
    "url": "assets/style.css",
    "revision": "b1056d7240990d37f11e6258aad8cdea"
  },
  {
    "url": "bower_components/html5shiv/dist/html5shiv.min.js",
    "revision": "40bd440d29b3a9371b0c63fec41ee64f"
  },
  {
    "url": "bower_components/html5shiv/dist/html5shiv-printshiv.min.js",
    "revision": "9f03100ab5ce18e0049c25c6c4916802"
  }
]);

self.addEventListener('install', function(event) {
  self.skipWaiting();
});
