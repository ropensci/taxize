RSCRIPT = Rscript --no-init-file

all: move rmd2md

vignettes:
		cd inst/vign;\
		Rscript -e 'library(knitr); knit("taxize_infotable.Rmd"); knit("taxize_vignette.Rmd")'

move:
		cp inst/vign/taxize_vignette.md vignettes;\
		cp inst/vign/taxize_infotable.md vignettes;\
		cp inst/vign/name_cleaning.md vignettes;\
		cp inst/vign/taxize_case_study.md vignettes

rmd2md:
		cd vignettes;\
		mv taxize_vignette.md taxize_vignette.Rmd;\
		mv taxize_infotable.md taxize_infotable.Rmd;\
		mv name_cleaning.md name_cleaning.Rmd;\
		mv taxize_case_study.md taxize_case_study.Rmd

install: doc build
	R CMD INSTALL . && rm *.tar.gz

build:
	R CMD build .

doc:
	${RSCRIPT} -e "devtools::document()"

eg:
	${RSCRIPT} -e "devtools::run_examples()"

check: build
	_R_CHECK_CRAN_INCOMING_=FALSE R CMD check --as-cran --no-manual `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -f `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -rf ${PACKAGE}.Rcheck
