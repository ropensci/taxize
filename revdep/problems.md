# Setup

## Platform

|setting  |value                                       |
|:--------|:-------------------------------------------|
|version  |R version 3.4.3 Patched (2018-01-01 r74017) |
|system   |x86_64, darwin15.6.0                        |
|ui       |X11                                         |
|language |(EN)                                        |
|collate  |en_US.UTF-8                                 |
|tz       |America/Los_Angeles                         |
|date     |2018-02-05                                  |

## Packages

|package |*  |version |date       |source                     |
|:-------|:--|:-------|:----------|:--------------------------|
|taxize  |   |0.9.2   |2018-02-06 |local (ropensci/taxize@NA) |

# Check results

2 packages with problems

|package  |version | errors| warnings| notes|
|:--------|:-------|------:|--------:|-----:|
|aptg     |0.1.0   |      0|        1|     0|
|camtrapR |0.99.9  |      0|        1|     0|

## aptg (0.1.0)
Maintainer: Christophe Benjamin <c.chignac.benjamin@gmail.com>

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...

Retrieving data for taxon 'Canis lupus'

Quitting from lines 32-33 (aptg.Rmd) 
Error: processing vignette 'aptg.Rmd' failed with diagnostics:
Timeout was reached: Connection timed out after 10002 milliseconds
Execution halted

```

## camtrapR (0.99.9)
Maintainer: Juergen Niedballa <niedballa@izw-berlin.de>

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Loading required package: sp
rgdal: version: 1.2-16, (SVN revision 701)
 Geospatial Data Abstraction Library extensions to R successfully loaded
 Loaded GDAL runtime: GDAL 2.1.3, released 2017/20/01
 Path to GDAL shared files: /Library/Frameworks/R.framework/Versions/3.4/Resources/library/rgdal/gdal
 GDAL binary built with GEOS: FALSE 
 Loaded PROJ.4 runtime: Rel. 4.9.3, 15 August 2016, [PJ_VERSION: 493]
 Path to PROJ.4 shared files: /Library/Frameworks/R.framework/Versions/3.4/Resources/library/rgdal/proj
 Linking to sp version: 1.2-5 
Quitting from lines 50-54 (DataExtraction.Rmd) 
Error: processing vignette 'DataExtraction.Rmd' failed with diagnostics:
cannot find ExifTool
Execution halted

```

