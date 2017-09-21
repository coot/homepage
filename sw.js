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
      statuses: [0, 200]
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
    "url": "index.html",
    "revision": "318e4d4802dcadb9a680b416a6cc6a18"
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
    "url": "assets/normalize.css",
    "revision": "dca0089e5b1a1579441226245958e80f"
  },
  {
    "url": "assets/print_style.css",
    "revision": "e4f43ff0cc0fb0a4a52d18e2acfdd9c7"
  },
  {
    "url": "assets/script.js",
    "revision": "118c50c800146611328cf17106aa8ba7"
  },
  {
    "url": "assets/style.css",
    "revision": "0c84aa296358d95c432fe77e4787ae2d"
  },
  {
    "url": "bower_components/html5shiv/dist/html5shiv.min.js",
    "revision": "3044234175ac91f49b03ff999c592b85"
  },
  {
    "url": "bower_components/html5shiv/dist/html5shiv-printshiv.min.js",
    "revision": "8c3c50c95caa7cef54d2c8720de4db37"
  }
]);
