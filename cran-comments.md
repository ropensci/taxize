## Test environments

* local macOS, R 4.0.3
* ubuntu 16.04 (on travis-ci), R 4.0.3
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

* I have run R CMD check on the 23 downstream dependencies; there were no errors related to taxize.

------

This version makes a few functions defunct for a web service that is no longer up, improves some docs, and fixes a few bugs. In addition, this fixes a failing test due to the latest rredlist on CRAN (an import here).

Thanks!
Scott Chamberlain
