all: move pandoc rmd2md

vignettes: 
		cd inst/vign;\
		Rscript -e 'library(knitr); knit("taxize_infotable.Rmd"); knit("taxize_vignette.Rmd")'

move:
		cp inst/vign/taxize_vignette.md vignettes
		cp inst/vign/taxize_infotable.md vignettes

pandoc:
		cd vignettes;\
		pandoc -H margins.sty taxize_vignette.md -o taxize_vignette.pdf;\
		pandoc -H margins.sty taxize_vignette.md -o taxize_vignette.html;\
		pandoc -H margins.sty taxize_infotable.md -o taxize_infotable.pdf;\
		pandoc -H margins.sty taxize_infotable.md -o taxize_infotable.html

rmd2md:
		cd vignettes;\
		cp taxize_vignette.md taxize_vignette.Rmd;\
		cp taxize_infotable.md taxize_infotable.Rmd

cleanup:
		cd inst/vign;\
		rm taxize_vignette.md taxize_infotable.md