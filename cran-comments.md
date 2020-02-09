## Test environments

* local OS X install, R 3.6.2 Patched
* ubuntu 16.04 (on travis-ci), R 3.6.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

* I have run R CMD check on the 22 downstream dependencies
(<https://github.com/ropensci/taxize/blob/master/revdep/README.md>);
there was an error in one package (spocc, also maintained by me) but only in the development version on GitHub, which I've fixed and a new version will be submitted soon.

------

This version includes a new function for parsing scientific names, and drops 3 package imports.

Thanks!
Scott Chamberlain
