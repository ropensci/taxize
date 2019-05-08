# BIOMASS

<details>

* Version: 2.1.1
* Source code: https://github.com/cran/BIOMASS
* URL: https://github.com/AMAP-dev/BIOMASS
* BugReports: https://github.com/AMAP-dev/BIOMASS/issues
* Date/Publication: 2019-05-03 15:20:25 UTC
* Number of recursive dependencies: 122

Run `revdep_details(,"BIOMASS")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
             fun = fun, na.rm = na.rm, layer = layer, nl = nl, df = df, factors = factors, 
             ...)
      6: .bilinearValue(object, xy, layer = layer, n = nl)
      7: getValues(raster, row1, nrows)
      8: getValues(raster, row1, nrows)
      9: .local(x, row, nrows, ...)
      10: .readRasterLayerValues(x, row, nrows)
      11: rgdal::getRasterData(con, offset = offs, region.dim = reg, band = object@data@band)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 470 SKIPPED: 0 WARNINGS: 0 FAILED: 1
      1. Error: getBioclimParam (@test_01_small_function.R#40) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

