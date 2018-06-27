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

workbox.precaching.precacheAndRoute([]);

self.addEventListener('install', function(event) {
  self.skipWaiting();
});
