vignettes: 
	cd vignettes;\
	Rscript -e 'library(knitr); knit("taxize_infotable.Rmd"); knit("taxize_vignette.Rmd")'

vign2pdfhtml:
	cd vignettes;\
	pandoc -H margins.sty taxize_infotable.md -o taxize_infotable.pdf;\
	pandoc -H margins.sty taxize_infotable.md -o taxize_infotable.html;\
	pandoc -H margins.sty taxize_vignette.md -o taxize_vignette.html;\
	pandoc -H margins.sty taxize_vignette.md -o taxize_vignette.pdf