<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{Strategies for programmatic name cleaning}
%\VignetteEncoding{UTF-8}
-->



Strategies for programmatic name cleaning
=========================================

`taxize` offers interactive prompts when using `get_*()` functions (e.g., `get_tsn()`).
These prompts make it easy in interactive use to select choices when there are more
than one match found.

However, to make your code reproducible you don't want interactive prompts.

This vignette covers various options for programmatic name cleaning.


```r
library("taxize")
```

## xxx


```r
"asdfasfsf"
#> [1] "asdfasfsf"
```
