vignettes: 
	cd vignettes;\
	Rscript -e 'library(knitr); knit("taxize_infotable.Rmd"); knit("taxize_matching.Rmd"); knit("taxize_vignette.Rmd"); knit("taxize_workflow.Rmd")'

vign2pdf:
	cd vignettes;\
	pandoc -H margins.sty taxize_infotable.md -o taxize_infotable.pdf;\
	pandoc -H margins.sty taxize_matching.md -o taxize_matching.pdf;\
	pandoc -H margins.sty taxize_vignette.md -o taxize_vignette.pdf;\
	pandoc -H margins.sty taxize_workflow.md -o taxize_workflow.pdf