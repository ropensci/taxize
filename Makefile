vignettes: 
	cd inst/stuff;\
	Rscript -e 'library(knitr); knit("taxize_infotable.Rmd"); knit("taxize_vignette.Rmd")'

vign2pdfhtml:
	cd inst/stuff;\
	pandoc -H margins.sty taxize_infotable.md -o taxize_infotable.pdf;\
	pandoc -H margins.sty taxize_infotable.md -o taxize_infotable.html;\
	pandoc -H margins.sty taxize_vignette.md -o taxize_vignette.html;\
	pandoc -H margins.sty taxize_vignette.md -o taxize_vignette.pdf

move:
	mv inst/stuff/taxize_* vignettes

cleanup:
	cd inst/stuff;\
	rm -rf cache