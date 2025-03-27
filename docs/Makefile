IMAGEMAGICK=convert

all: overview.png projects.png

overview.png: overview.pdf
	${IMAGEMAGICK} -quality 200 -density 200 $< -trim $@

projects.png: projects.pdf
	${IMAGEMAGICK} -quality 200 -density 200 $< -trim $@

%.pdf: %.tex
	pdflatex $<

clean:
	rm *.pdf *.png *.log *.aux
