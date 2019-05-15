## Test environments

* local OS X install, R 3.6.0 Patched
* ubuntu 14.04 (on travis-ci), R 3.6.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

* I have run R CMD check on the 22 downstream dependencies
(<https://github.com/ropensci/taxize/blob/master/revdep/README.md>).
There was a problem with one package, BIOMASS, unrelated to taxize.

------

This version makes a change in the output of the class2tree function, moves the package to using markdown documentation, and fixes encoding problems in test fixtures.

Thanks!
Scott Chamberlain
