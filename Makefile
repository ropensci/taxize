all: move rmd2md cleanup

vignettes:
		cd inst/vign;\
		Rscript -e 'library(knitr); knit("taxize_infotable.Rmd"); knit("taxize_vignette.Rmd")'

move:
		cp inst/vign/taxize_vignette.md vignettes
		cp inst/vign/taxize_infotable.md vignettes

rmd2md:
		cd vignettes;\
		cp taxize_vignette.md taxize_vignette.Rmd;\
		cp taxize_infotable.md taxize_infotable.Rmd

cleanup:
		cd vignettes;\
		rm taxize_vignette.md taxize_infotable.md
