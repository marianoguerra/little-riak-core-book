
build:
	rst2html5 --pretty-print-code --traceback --title "Little Riak Core Talk" --jquery --reveal-js --reveal-js-opts theme=serif --stylesheet-path style.css talk.rst > index.html

pdf:
	rst2html5 --pretty-print-code --traceback --title "Little Riak Core Talk" --jquery --reveal-js --stylesheet-path style.css --reveal-js-opts theme=serif,printpdf=true talk.rst > index.html

serve:
	python -m SimpleHTTPServer

open:
	firefox http://localhost:8000/
