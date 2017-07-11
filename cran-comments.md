## Test environments

* local OS X install, R 3.4.0 patched
* ubuntu 12.04 (on travis-ci), R 3.4.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* Note about license:
License components with restrictions and base license permitting such:
  MIT + file LICENSE
File 'LICENSE':
  YEAR: 2017
  COPYRIGHT HOLDER: Scott Chamberlain

## Reverse dependencies

* I have run R CMD check on the 18 downstream dependencies
(<https://github.com/ropensci/taxize/blob/master/revdep/README.md>).
All revdep maintainers were notified of the release. There was one
problem with a vignette build with the test suite for myTAI - but is
unrelated to this package.

------

This version fixes a bug - that was causing failed installation for at 
least one user; so may affect others.

Thanks!
Scott Chamberlain
