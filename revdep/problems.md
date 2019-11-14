# spocc

<details>

* Version: 1.0.2
* Source code: https://github.com/cran/spocc
* URL: https://github.com/ropensci/spocc (devel), https://ropensci.github.io/spocc/ (user manual)
* BugReports: https://github.com/ropensci/spocc/issues
* Date/Publication: 2019-11-02 09:30:02 UTC
* Number of recursive dependencies: 119

Run `revdep_details(,"spocc")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Loading required package: spocc
      ── 1. Error: has_coords works with all data sources as planned (@test-has_coords
      missing value where TRUE/FALSE needed
      Backtrace:
       1. spocc::occ(...)
       2. base::lapply(...)
       3. spocc:::FUN(X[[i]], ...)
       4. spocc:::foo_ecoengine(sources, x, y, p, z, hc, d, w, ecoengineopts)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 112 | SKIPPED: 9 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: has_coords works with all data sources as planned (@test-has_coords.R#22) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

