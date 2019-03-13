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
There was problem with a vignette build for camtrapR - but is unrelated to this package; and there was a problem with tests in BIOMASS, but I checked and even though taxize is in Suggests in their package, they dont use taxize anywhere in the package, and so any errors are unrelated to this package.

------

This version includes a set of new functions for a new data source, now passes user agent strings in all HTTP requests, and makes many bug fixes and small improvements.

Thanks!
Scott Chamberlain
