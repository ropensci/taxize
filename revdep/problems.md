# Setup

## Platform

|setting  |value                                       |
|:--------|:-------------------------------------------|
|version  |R version 3.4.0 Patched (2017-06-05 r72770) |
|system   |x86_64, darwin15.6.0                        |
|ui       |RStudio (1.0.143)                           |
|language |(EN)                                        |
|collate  |en_US.UTF-8                                 |
|tz       |America/Vancouver                           |
|date     |2017-06-29                                  |

## Packages

|package |*  |version    |date       |source                     |
|:-------|:--|:----------|:----------|:--------------------------|
|taxize  |   |0.8.7.9881 |2017-06-29 |local (ropensci/taxize@NA) |

# Check results

1 packages with problems

|package |version | errors| warnings| notes|
|:-------|:-------|------:|--------:|-----:|
|myTAI   |0.5.0   |      1|        0|     0|

## myTAI (0.5.0)
Maintainer: Hajk-Georg Drost <hgd23@cam.ac.uk>  
Bug reports: https://github.com/HajkD/myTAI/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘testthat.R’ [58s/68s]
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  
  
  `summarise_each()` is deprecated.
  Use `summarise_all()`, `summarise_at()` or `summarise_if()` instead.
  To map `funs` over all variables, use `summarise_all()`
  `summarise_each()` is deprecated.
  Use `summarise_all()`, `summarise_at()` or `summarise_if()` instead.
  To map `funs` over all variables, use `summarise_all()`
  testthat results ================================================================
  OK: 178 SKIPPED: 1 FAILED: 2
  1. Failure: PlotContribution() works properly with PhyloExpressionSet input... (@test-PlotContribution.R#11) 
  2. Failure: PlotContribution() works properly with DivergenceExpressionSet input... (@test-PlotContribution.R#16) 
  
  Error: testthat unit tests failed
  Execution halted
```

