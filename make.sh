whalesong/current/whalesong.rkt build stxmat-exercises.rkt
cd build
mv ../stxmat-exercises.xhtml .
sed -i 'tmp' '/<body><\/body>/ r ../js-libs/load-mathjax.xhtml' stxmat-exercises.xhtml
sed -i 'tmp' 's/<body><\/body>//' stxmat-exercises.xhtml
sed -i 'tmp' 's/<\/html>/<\body><\/body><\/html>/' stxmat-exercises.xhtml
cd ..

