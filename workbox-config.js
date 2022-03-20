module.exports = {
	globDirectory: 'static/public/',
	globPatterns: [
		'**/*.{css,woff2,_hs,js,svg}'
	],
	swDest: 'static/public/sw.js',
	ignoreURLParametersMatching: [
		/^utm_/,
		/^fbclid$/
	]
};