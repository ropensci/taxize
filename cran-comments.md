R CMD CHECK passed on my local OS X install with R 3.2.2 and
R development version, Ubuntu running on Travis-CI, and Win builder.

This submission includes some defunct functions; a number of minor 
improvements, and bug fixes.

There is one note on CRAN check results, for r-release-osx-x86_64-mavericks, and r-oldrel-windows-ix86+x86_64:

Result: NOTE 
    gnr_resolve: possible error in nchar(preferred_data_sources, keepNA =
     FALSE): unused argument (keepNA = FALSE)
    ipni_search: possible error in nchar(res, keepNA = FALSE): unused
     argument (keepNA = FALSE)
    phylomatic_format : foo: possible error in nchar(as.character(dd$that),
     keepNA = FALSE): unused argument (keepNA = FALSE) 
     
However, in this submission I require R(>= 3.2.1) in this package now, so users 
will need to have a version of R that will have `keepNA` parameter in the `nchar()` 
function. 

Thanks!
Scott Chamberlain
