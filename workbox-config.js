module.exports = {
	globDirectory: 'static/public/',
	globPatterns: [
		'**/*.{css,woff2,_hs,js,svg}'
	],
	swDest: 'static/public/sw.js',
	ignoreURLParametersMatching: [
		/^utm_/,
		/^fbclid$/
	],
  runtimeCaching: [
    {
      urlPattern: /https:\/\/kit\.fontawesome\.com\/e0cb5637ed\.js.*/,
      handler: 'CacheFirst'
    },
    {
      urlPattern: /https:\/\/ka-p\.fontawesome\.com\/.*/,
      handler: 'CacheFirst'
    },
    // Removed rsms.me - using local fonts now
    {
      urlPattern: /https:\/\/cdnjs\.cloudflare\.com\/.*/,
      handler: 'CacheFirst'
    },
    {
      urlPattern: /https:\/\/cdn\.jsdelivr\.net\/npm\/.*/,
      handler: 'CacheFirst'
    },
    {
      urlPattern: /https:\/\/unpkg\.com\/.*/,
      handler: 'CacheFirst'
    }
  ]
};
