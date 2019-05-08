## Test environments

* local OS X install, R 3.6.0 Patched
* ubuntu 14.04 (on travis-ci), R 3.6.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

* I have run R CMD check on the 22 downstream dependencies
(<https://github.com/ropensci/taxize/blob/master/revdep/README.md>).
There was problem with a vignette build for camtrapR - but is unrelated to this package; and there was a problem with tests in BIOMASS, but I checked and even though taxize is in Suggests in their package, they dont use taxize anywhere in the package, and so any errors are unrelated to this package.

------

This version makes a change in the output of the class2tree function, moves the package to using markdown documentation, and fixes encoding problems in test fixtures.

Thanks!
Scott Chamberlain
