importScripts(
    "workbox-sw.prod.v2.0.1.js"
  , "workbox-routing.prod.v2.0.0.js"
  , "workbox-runtime-caching.prod.v2.0.0.js"
);
const workbox = new self.WorkboxSW();

workbox.router.registerRoute(
    new RegExp('^http:\/\/netdna\.bootstrapcdn\.com\/font-awesome\/4\.1\.0\/css\/font-awesome\.min\.css')
  , workbox.strategies.staleWhileRevalidate({
    cacheableResponse: {
      statuses: [0, 200, 304]
    }
  })
)

workbox.router.registerRoute(
    new RegExp('^http:\/\/netdna\.bootstrapcdn\.com\/font-awesome\/4\.1\.0\/fonts\/fontawesome-webfont\.woff.*')
  , workbox.strategies.staleWhileRevalidate({
    cacheableResponse: {
      statuses: [0, 200]
    }
  })
)

workbox.router.registerRoute(
    new RegExp('https:\/\/fonts\.googleapis\.com\/css.*')
  , workbox.strategies.staleWhileRevalidate({
    cacheableResponse: {
      statuses: [0, 200]
    }
  })
)

workbox.router.registerRoute(
    new RegExp('https:\/\/fonts.gstatic.com\/s\/opensans\/.*')
  , workbox.strategies.staleWhileRevalidate({
    cacheableResponse: {
      statuses: [0, 200]
    }
  })
)

workbox.precache([
  {
    "url": "header.html",
    "revision": "59408055841389af8c6fa3a37f45ae33"
  },
  {
    "url": "index.html",
    "revision": "1a033d324a794aa479ede06e3edf63e8"
  },
  {
    "url": "about.html",
    "revision": "7fa96c418cc8a30825e978d241694540"
  },
  {
    "url": "posts/adts-and-universal-algebra.html",
    "revision": "1391ad9d34e15a43792995ba50f4bd4d"
  },
  {
    "url": "posts/freeness.html",
    "revision": "b7d0b10e249bbbe3c3bc0f2fb3c27764"
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
    "revision": "551bd516db5fdd3fc6061c186eba3635"
  },
  {
    "url": "assets/MathJax.js",
    "revision": "27e135ad6e379b9e52682be4a56d1007"
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
