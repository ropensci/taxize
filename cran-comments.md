## Test environments

* local OS X install, R 3.5.2 patched
* ubuntu 14.04 (on travis-ci), R 3.5.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* Note about license:
License components with restrictions and base license permitting such:
  MIT + file LICENSE
File 'LICENSE':
  YEAR: 2019
  COPYRIGHT HOLDER: Scott Chamberlain

## Reverse dependencies

* I have run R CMD check on the 21 downstream dependencies
(<https://github.com/ropensci/taxize/blob/master/revdep/README.md>).
There was one problem with a vignette build with the test suite for camtrapR - but is unrelated to this package.

------

This version gains a few new functions, some new parameters, make a function defunct, and fixes many bugs.

Thanks!
Scott Chamberlain
