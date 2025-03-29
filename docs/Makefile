IMAGEMAGICK=convert

all: overview.png projects.png ast-module.png sail-to-nanosail-module.png nanosail-to-microsail-module.png

%.png: %.pdf
	${IMAGEMAGICK} -quality 200 -density 200 $< -alpha remove $@

%.pdf: %.tex
	pdflatex $<

clean:
	rm *.pdf *.png *.log *.aux
