importScripts(
    "/node_modules/workbox-sw/build/workbox-sw.js"
  , "/node_modules/workbox-routing/build/workbox-routing.dev.js"
  , "/node_modules/workbox-precaching/build/workbox-precaching.dev.js"
);

workbox.core.setCacheNameDetails({
  prefix: 'coot-homepage',
  suffix: 'v1',
  precache: 'cache',
  runtime: 'cache'
});

console.log(workbox.core.cacheNames)

workbox.routing.registerRoute(
    (function(args) {
      var pathname = args.url.pathname
      var hostname = args.url.hostname
      if (hostname != location.hostname)
        return null
      if (/^\/(\?.*|#.*)?$/.test(pathname))
        return {}
      else if (/^\/assets\//.test(pathname))
        return {}
      else if (/^\/.*\.html/.test(pathname))
        return {}
      else
        return null
    })
  , workbox.strategies.networkFirst()
)

workbox.routing.registerRoute(
    /\/images\//
  , workbox.strategies.staleWhileRevalidate({
    cacheableResponse: {
      statuses: [0, 200]
    }
  })
)

workbox.routing.registerRoute(
    /^http:\/\/netdna\.bootstrapcdn\.com\/font-awesome\/4\.1\.0\/css\/font-awesome\.min\.css/
  , workbox.strategies.staleWhileRevalidate({
    cacheableResponse: {
      statuses: [0, 200, 304]
    }
  })
)

workbox.routing.registerRoute(
    /^http:\/\/netdna\.bootstrapcdn\.com\/font-awesome\/4\.1\.0\/fonts\/fontawesome-webfont\.woff.*/
  , workbox.strategies.staleWhileRevalidate({
    cacheableResponse: {
      statuses: [0, 200]
    }
  })
)

workbox.routing.registerRoute(
    /^https:\/\/fonts\.googleapis\.com\/css.*/
  , workbox.strategies.staleWhileRevalidate({
    cacheableResponse: {
      statuses: [0, 200]
    }
  })
)

workbox.routing.registerRoute(
    /^https:\/\/fonts\.gstatic.com\/s\/opensans\/.*/
  , workbox.strategies.staleWhileRevalidate({
    cacheableResponse: {
      statuses: [0, 200]
    }
  })
)

workbox.routing.registerRoute(
    /^https:\/\/cdnjs\.cloudflare\.com\/ajax\/libs\/mathjax\/2\.7\.4\/MathJax\.js.*/
  , workbox.strategies.staleWhileRevalidate({
    cacheableResponse: {
      statuses: [0, 200]
    }
  })
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
    "revision": "38968ba7a3270900ff08c6fe475fb920"
  },
  {
    "url": "posts/adts-and-universal-algebra.html",
    "revision": "54f58153bbdb1772b1eccbc65f06e0e7"
  },
  {
    "url": "posts/free-monads.html",
    "revision": "5ca8445f4c4c5ae302afabc6b836508b"
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
    "revision": "ccc0c1f2f8b33ab956a8b1ba32edddbd"
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
