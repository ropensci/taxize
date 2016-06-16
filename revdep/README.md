# Setup

## Platform

|setting  |value                                       |
|:--------|:-------------------------------------------|
|version  |R version 3.3.0 Patched (2016-05-09 r70593) |
|system   |x86_64, darwin13.4.0                        |
|ui       |RStudio (0.99.1218)                         |
|language |(EN)                                        |
|collate  |en_US.UTF-8                                 |
|tz       |America/Los_Angeles                         |
|date     |2016-06-16                                  |

## Packages

|package |*  |version    |date       |source                          |
|:-------|:--|:----------|:----------|:-------------------------------|
|covr    |   |2.0.1.9000 |2016-06-16 |Github (jimhester/covr@ba7ef46) |
|taxize  |   |0.7.7.9600 |2016-06-16 |local (ropensci/taxize@NA)      |

# Check results
16 packages

## bdvis (0.2.6)
Maintainer: Vijay Barve <vijay.barve@gmail.com>

0 errors | 0 warnings | 0 notes

## binomen (0.1.0)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropensci/binomen/issues

0 errors | 0 warnings | 0 notes

## brranching (0.2.0)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: http://www.github.com/ropensci/brranching/issues

0 errors | 0 warnings | 0 notes

## camtrapR (0.99.1)
Maintainer: Juergen Niedballa <niedballa@izw-berlin.de>

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Loading required package: sp
rgdal: version: 1.1-10, (SVN revision 622)
 Geospatial Data Abstraction Library extensions to R successfully loaded
 Loaded GDAL runtime: GDAL 1.11.1, released 2014/09/24
 Path to GDAL shared files: /Library/Frameworks/GDAL.framework/Versions/1.11/Resources/gdal
 Loaded PROJ.4 runtime: Rel. 4.9.2, 08 September 2015, [PJ_VERSION: 492]
 Path to PROJ.4 shared files: (autodetected)
 Linking to sp version: 1.2-3 
Quitting from lines 50-54 (DataExtraction.Rmd) 
Error: processing vignette 'DataExtraction.Rmd' failed with diagnostics:
cannot find ExifTool
Execution halted

```

## mapr (0.2.0)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropensci/mapr/issues

0 errors | 0 warnings | 0 notes

## MonoPhy (1.1)
Maintainer: Orlando Schwery <oschwery@vols.utk.edu>

0 errors | 0 warnings | 0 notes

## myTAI (0.4.0)
Maintainer: Hajk-Georg Drost <hgd23@cam.ac.uk>  
Bug reports: https://github.com/HajkD/myTAI/issues

0 errors | 0 warnings | 0 notes

## originr (0.1.0)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropenscilabs/originr/issues

0 errors | 0 warnings | 0 notes

## rbison (0.4.8)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropensci/rbison/issues

0 errors | 0 warnings | 0 notes

## RNeXML (2.0.6)
Maintainer: Carl Boettiger <cboettig@gmail.com>  
Bug reports: https://github.com/ropensci/RNeXML/issues

2 errors | 1 warning  | 0 notes

```
checking examples ... ERROR
Running examples in ‘RNeXML-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: add_basic_meta
> ### Title: Add basic metadata
> ### Aliases: add_basic_meta
> 
> ### ** Examples
> 
> nex <- add_basic_meta(title = "My test title",
+              description = "A description of my test",
+              creator = "Carl Boettiger <cboettig@gmail.com>",
+              publisher = "unpublished data",
+              pubdate = "2012-04-01")
Error in stopc("Unknown column '", i, "'") : Unknown column 'content'
Calls: add_basic_meta -> $ -> $.tbl_df -> stopc
Execution halted

checking tests ... ERROR
Running the tests in ‘tests/test-all.R’ failed.
Last 13 lines of output:
  1. Error: We can serialize the various versions of the ape format (@test_ape.R#40) 
  2. Error: We can read and write NeXML to phylo and back without edge.lengths (@test_ape.R#52) 
  3. Error: Rooted trees remain rooted on conversions (@test_ape.R#65) 
  4. Error: Unrooted trees remain unrooted on conversions (@test_ape.R#74) 
  5. Error: we can extract character matrix with get_characters (@test_characters.R#44) 
  6. Error: we can add characters to a nexml file using a data.frame (@test_characters.R#88) 
  7. Error: We can extract tree and trait data to run fitContinuous and fitDiscrete (@test_comp_analysis.R#9) 
  8. Error: We can serialize tree and trait data for a comparative analysis (@test_comp_analysis.R#25) 
  9. Error: Getting characters (@test_get_characters.R#7) 
  1. ...
  
  Error: testthat unit tests failed
  Execution halted

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Loading required package: ape
Quitting from lines 46-67 (metadata.Rmd) 
Error: processing vignette 'metadata.Rmd' failed with diagnostics:
Unknown column 'content'
Execution halted

```

## rnoaa (0.5.6)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: http://www.github.com/ropensci/rnoaa/issues

0 errors | 0 warnings | 0 notes

## rusda (1.0.8)
Maintainer: Franz-Sebastian Krah <f.krah@mailbox.org>

0 errors | 0 warnings | 0 notes

## spocc (0.5.0)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropensci/spocc/issues

0 errors | 0 warnings | 0 notes

## spoccutils (0.1.0)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropensci/spoccutils/issues

0 errors | 0 warnings | 0 notes

## TR8 (0.9.14)
Maintainer: Gionata Bocci <boccigionata@gmail.com>  
Bug reports: https://github.com/GioBo/TR8/issues

0 errors | 0 warnings | 0 notes

## traits (0.2.0)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: http://www.github.com/ropensci/traits/issues

0 errors | 0 warnings | 0 notes

