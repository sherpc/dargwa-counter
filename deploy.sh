lein clean && lein release
rm dargwa-counter.zip
zip dargwa-counter.zip public/css/* public/fonts/* public/dargwa-* public/js/app.js public/index.html
