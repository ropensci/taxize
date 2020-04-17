PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')
RSCRIPT = Rscript --no-init-file

vign_taxize:
		cd vignettes;\
		${RSCRIPT} -e "Sys.setenv(NOT_CRAN='true'); knitr::knit('taxize.Rmd.og', output = 'taxize.Rmd')";\
		cd ..

vign_name_cleaning:
		cd vignettes;\
		${RSCRIPT} -e "Sys.setenv(NOT_CRAN='true'); knitr::knit('name_cleaning.Rmd.og', output = 'name_cleaning.Rmd')";\
		cd ..

vign_use_case:
		cd vignettes;\
		${RSCRIPT} -e "Sys.setenv(NOT_CRAN='true'); knitr::knit('case_study.Rmd.og', output = 'case_study.Rmd')";\
		cd ..

install_vign: doc build
	${RSCRIPT} -e "Sys.setenv(NOT_CRAN = TRUE); library(devtools); document(); install(build_vignettes=TRUE, dependencies=FALSE)"

install: doc build
	R CMD INSTALL . && rm *.tar.gz

build:
	R CMD build .

doc:
	${RSCRIPT} -e "devtools::document()"

eg:
	${RSCRIPT} -e "devtools::run_examples(run = TRUE)"

check: build
	_R_CHECK_CRAN_INCOMING_=FALSE R CMD CHECK --as-cran --no-manual `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -f `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -rf ${PACKAGE}.Rcheck

test:
	${RSCRIPT} -e 'devtools::test()'

readme:
	${RSCRIPT} -e 'knitr::knit("README.Rmd")'

check_windows:
	${RSCRIPT} -e "devtools::check_win_devel(); devtools::check_win_release()"
		
