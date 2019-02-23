# BIOMASS

Version: 2.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        |                                                                            
        |======================================================================| 100%
      
        |                                                                            
        |                                                                      |   0%
        |                                                                            
        |======================================================================| 100%
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 503 SKIPPED: 0 FAILED: 3
      1. Failure: CorrectTaxo (@test_00_correctTaxo.R#49) 
      2. Failure: CorrectTaxo (@test_00_correctTaxo.R#52) 
      3. Failure: CorrectTaxo (@test_00_correctTaxo.R#53) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 54-57 (BIOMASS.Rmd) 
    Error: processing vignette 'BIOMASS.Rmd' failed with diagnostics:
    Item 2 has 5 columns, inconsistent with item 1 which has 8 columns. If instead you need to fill missing columns, use set argument 'fill' to TRUE.
    Execution halted
    ```

# camtrapR

Version: 1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Loading required package: sp
    rgdal: version: 1.3-9, (SVN revision 794)
     Geospatial Data Abstraction Library extensions to R successfully loaded
     Loaded GDAL runtime: GDAL 2.1.3, released 2017/20/01
     Path to GDAL shared files: /Users/sckott/github/ropensci/taxize/revdep/library.noindex/camtrapR/rgdal/gdal
     GDAL binary built with GEOS: FALSE 
     Loaded PROJ.4 runtime: Rel. 4.9.3, 15 August 2016, [PJ_VERSION: 493]
     Path to PROJ.4 shared files: /Users/sckott/github/ropensci/taxize/revdep/library.noindex/camtrapR/rgdal/proj
     Linking to sp version: 1.3-1 
    Quitting from lines 50-54 (DataExtraction.Rmd) 
    Error: processing vignette 'DataExtraction.Rmd' failed with diagnostics:
    cannot find ExifTool
    Execution halted
    ```

# mapr

Version: 0.4.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 164 marked UTF-8 strings
    ```

# metacoder

Version: 0.3.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggrepel’ ‘reshape’ ‘svglite’
      All declared Imports should be used.
    ```

# myTAI

Version: 0.9.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        doc    2.4Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘biomartr’
      All declared Imports should be used.
    ```

# originr

Version: 0.3.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# taxa

Version: 0.3.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘knitr’ ‘lazyeval’ ‘rlang’ ‘tidyr’
      All declared Imports should be used.
    ```

# taxlist

Version: 0.1.6

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘grDevices’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 97 marked UTF-8 strings
    ```

