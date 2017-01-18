## Test environments

* local OS X install, R 3.3.2
* ubuntu 12.04 (on travis-ci), R 3.3.2
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

* I have run R CMD check on the 16 downstream dependencies
(<https://github.com/ropensci/taxize/blob/master/revdep/README.md>).
All revdep maintainers were notified of the release. There was one
problem with a vignette build with the pacakge camtrapR - I have 
notified the maintainer.

------

This version includes two new data sources, and two bug fixes, including 
for a recent CRAN check failure that Kurt contacted me about.

Thanks!
Scott Chamberlain
