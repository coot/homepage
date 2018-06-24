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

workbox.precaching.precacheAndRoute([]);

self.addEventListener('install', function(event) {
  self.skipWaiting();
});
