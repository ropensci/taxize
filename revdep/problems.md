# Setup

## Platform

|setting  |value                                       |
|:--------|:-------------------------------------------|
|version  |R version 3.3.0 Patched (2016-05-09 r70593) |
|system   |x86_64, darwin13.4.0                        |
|ui       |X11                                         |
|language |(EN)                                        |
|collate  |en_US.UTF-8                                 |
|tz       |America/Los_Angeles                         |
|date     |2016-05-10                                  |

## Packages

|package |*  |version |date       |source                  |
|:-------|:--|:-------|:----------|:-----------------------|
|taxize  |   |0.7.6   |2016-05-10 |local (ropensci/taxize) |

# Check results
1 packages with problems

## camtrapR (0.99.0)
Maintainer: Juergen Niedballa <niedballa@izw-berlin.de>

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Loading required package: sp
rgdal: version: 1.1-9, (SVN revision 617M)
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

