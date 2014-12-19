R CMD CHECK passed on my local OS X install with R 3.1.2 and R development version, Ubuntu running on Travis-CI, and Win builder.

This version is submitted to CRAN in response to Brian Ripley's request to fix examples in \donttest that were either not working or loading libraries not listed in the DESCRIPTION file. Most functions work with web APIs, so are wrapped in \dontrun, but some examples don't do requests for web data, so should work every time.

Thanks! Scott Chamberlain
