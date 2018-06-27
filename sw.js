importScripts(
    "/node_modules/workbox-sw/build/workbox-sw.js"
  , "/node_modules/workbox-routing/build/workbox-routing.prod.js"
  , "/node_modules/workbox-precaching/build/workbox-precaching.prod.js"
);

workbox.core.setCacheNameDetails({
  prefix: 'coot-homepage',
  suffix: 'v1',
  precache: 'cache',
  runtime: 'cache'
});

workbox.routing.registerRoute(
    (function(args) {
      var pathname = args.url.pathname
      var hostname = args.url.hostname
      if (hostname != location.hostname)
        return null
      if (/^\/(\?.*|#.*)?$/.test(pathname))
        return {}
      else if (/^\/posts\/latex\//.test(pathname))
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
    /\/pospts\/latex\//
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
    "url": "images/elf-fg-gray-no-background-250.png",
    "revision": "3f3ac922a688358bc891d38a26cd7cdd"
  },
  {
    "url": "images/elf-fg-gray-no-background-480.png",
    "revision": "27f9229890ad9d7ea3ad428fec3e7b7c"
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
    "url": "bower_components/html5shiv/dist/html5shiv.js",
    "revision": "f4d9dea8e0ae8455500862bbb874d63c"
  },
  {
    "url": "bower_components/html5shiv/dist/html5shiv-printshiv.js",
    "revision": "87ab03595191d555da6261d11d2b2e32"
  }
]);

self.addEventListener('install', function(event) {
  self.skipWaiting();
});
