importScripts(
    "/assets/workbox-sw.js"
  , "/assets/workbox-routing.prod.js"
  , "/assets/workbox-precaching.prod.js"
);

workbox.core.setCacheNameDetails({
  prefix: 'coot-homepage',
  suffix: 'v2',
});

workbox.routing.registerRoute(
    /^\.html/
  , workbox.strategies.networkOnly()
)

workbox.routing.registerRoute(
    /\/assets\//
  , workbox.strategies.staleWhileRevalidate({
      cacheableResponse: {
        statuses: [0, 200]
      }
    })
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
    /^https?:\/\/fonts\.gstatic.com\/s\/OpenSans\/.*/
  , workbox.strategies.staleWhileRevalidate({
      cacheableResponse: {
        statuses: [0, 200]
      }
    })
)

workbox.routing.registerRoute(
    /^https?:\/\/cdnjs\.cloudflare\.com\/ajax\/libs\/(math)?jax\/2\.7\.4\/.*/
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
