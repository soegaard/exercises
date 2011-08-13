cd build
../whalesong/current/whalesong.rkt build ../stxmat-exercises.rkt
sed -i 'tmp' '/<body><\/body>/ r ../load-mathjax.xhtml' stxmat-exercises.xhtml
sed -i 'tmp' 's/<body><\/body>//' stxmat-exercises.xhtml
sed -i 'tmp' 's/<\/html>/<\body><\/body><\/html>/' stxmat-exercises.xhtml

