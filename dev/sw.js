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

workbox.precache([]);

self.addEventListener('install', function(event) {
  self.skipWaiting();
});
