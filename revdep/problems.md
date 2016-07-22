# Setup

## Platform

|setting  |value                                       |
|:--------|:-------------------------------------------|
|version  |R version 3.3.1 Patched (2016-07-13 r70908) |
|system   |x86_64, darwin13.4.0                        |
|ui       |RStudio (0.99.1266)                         |
|language |(EN)                                        |
|collate  |en_US.UTF-8                                 |
|tz       |America/Los_Angeles                         |
|date     |2016-07-22                                  |

## Packages

|package |*  |version    |date       |source                     |
|:-------|:--|:----------|:----------|:--------------------------|
|taxize  |   |0.7.8.9999 |2016-07-22 |local (ropensci/taxize@NA) |

# Check results
1 packages with problems

## camtrapR (0.99.2)
Maintainer: Juergen Niedballa <niedballa@izw-berlin.de>

0 errors | 1 warning  | 1 note 

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


checking for unstated dependencies in vignettes ... NOTE
'library' or 'require' call not declared from: ‘raster’
```

