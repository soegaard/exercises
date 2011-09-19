which racket &>/dev/null
[ $? -eq 0 ] || export PATH=$PATH:/Applications/Racket/bin/
whalesong/current/whalesong.rkt build stxmat-exercises.rkt
cd build
mv ../stxmat-exercises.html .
mv ../stxmat-exercises.js .
sed -i '.tmp' '/<body>/ r ../js-libs/load-mathjax.xhtml' stxmat-exercises.html
sed -i '.tmp' 's/<body>//' stxmat-exercises.html
sed -i '.tmp' 's/<\/body>/<body><\/body>/' stxmat-exercises.html
cd ..

