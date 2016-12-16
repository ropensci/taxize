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
  YEAR: 2016
  COPYRIGHT HOLDER: Scott Chamberlain

## Reverse dependencies

* I have run R CMD check on the 16 downstream dependencies
(<https://github.com/ropensci/taxize/blob/master/revdep/README.md>).
All revdep maintainers were notified of the release. There was one
problem with the pacakge camtrapR - I have notified the
maintainer earlier this week and said is ready to deploy fixes.

------

This version includes some new functions, removes some functions to instead use those
from an imported package - a number of bug fixes.

Thanks!
Scott Chamberlain
