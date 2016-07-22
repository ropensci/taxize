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

## get_* functions

When using `get_*()` functions programatically, you have a few options.

### rows parameter

Normally, if you get more than one result, you get a prompt asking you
to select which taxon you want.


```r
get_tsn(searchterm = "Quercus b")
#>       tsn                       target             commonnames    nameusage
#> 1   19298             Quercus beebiana                         not accepted
#> 2  507263       Quercus berberidifolia               scrub oak     accepted
#> 3   19300              Quercus bicolor         swamp white oak     accepted
#> 4   19303             Quercus borealis                         not accepted
#> 5  195131 Quercus borealis var. maxima                         not accepted
#> 6  195166            Quercus boyntonii Boynton's sand post oak     accepted
#> 7  506533              Quercus brantii             Brant's oak     accepted
#> 8  195150            Quercus breviloba                         not accepted
#> 9  195099              Quercus breweri                         not accepted
#> 10 195168             Quercus buckleyi               Texas oak     accepted
#>
#> More than one TSN found for taxon 'Quercus b'!
#>
#>             Enter rownumber of taxon (other inputs will return 'NA'):
#>
#> 1:
```

Instead, we can use the rows parameter to specify which records we want
by number only (not by a name itself). Here, we want the first 3 records:


```r
get_tsn(searchterm = 'Quercus b', rows = 1:3)
#>     tsn           target     commonnames    nameusage
#> 1 19298 Quercus beebiana                 not accepted
#> 2 19300  Quercus bicolor swamp white oak     accepted
#> 3 19303 Quercus borealis                 not accepted
#>
#> More than one TSN found for taxon 'Quercus b'!
#>
#>             Enter rownumber of taxon (other inputs will return 'NA'):
#>
#> 1:
```

However, you still get a prompt as there is more than one result.

Thus, for full programmatic usage, you can specify a single row, if you happen
to know which one you want:


```r
get_tsn(searchterm = 'Quercus b', rows = 3)
#> [1] "19303"
#> attr(,"match")
#> [1] "found"
#> attr(,"multiple_matches")
#> [1] TRUE
#> attr(,"pattern_match")
#> [1] FALSE
#> attr(,"uri")
#> [1] "http://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=19303"
#> attr(,"class")
#> [1] "tsn"
```

In reality it is unlikely you'll know which row you want, unless perhaps you 
just one one result from each query, regardless of what it is.

### underscore methods

A better fit for programmatic use are underscore methods. Each `get_*()` function
has a sister method with and trailing underscore, e.g., `get_tsn()` and `get_tsn_()`.


```r
get_tsn_(searchterm = "Quercus b")
#> $`Quercus b`
#>       tsn         scientificname             commonnames nameusage
#> 2   19300        Quercus bicolor         swamp white oak  accepted
#> 7  195166      Quercus boyntonii Boynton's sand post oak  accepted
#> 8  195168       Quercus buckleyi               Texas oak  accepted
#> 9  506533        Quercus brantii             Brant's oak  accepted
#> 10 507263 Quercus berberidifolia               scrub oak  accepted
```

The result is a single data.frame for each taxon queried, which can be 
processed downstream with whatever logic is required in your workflow.

You can also combine `rows` parameter with underscore functions, as a single
number of a range of numbers:


```r
get_tsn_(searchterm = "Quercus b", rows = 1)
#> $`Quercus b`
#>     tsn  scientificname     commonnames nameusage
#> 2 19300 Quercus bicolor swamp white oak  accepted
```


```r
get_tsn_(searchterm = "Quercus b", rows = 1:2)
#> $`Quercus b`
#>      tsn    scientificname             commonnames nameusage
#> 2  19300   Quercus bicolor         swamp white oak  accepted
#> 7 195166 Quercus boyntonii Boynton's sand post oak  accepted
```

## gnr_resolve

Some functions in `taxize` are meant specifically for name cleaning. One of those
is `gnr_resolve()`.

`gnr_resolve()` doesn't provide prompts as do `get_*()` functions, but instead
return data.frame's. So we don't face the same problem, and can use `gnr_resolve()` 
in a programmatic workflow straight away.


```r
spp <- names_list(rank = "species", size = 10)
gnr_resolve(names = spp, preferred_data_sources = 11)
#>           user_supplied_name            submitted_name
#> 1          Sedum morganianum         Sedum morganianum
#> 2  Anthurium oblongo-nitidus Anthurium oblongo-nitidus
#> 3         Lupinus syriggedes        Lupinus syriggedes
#> 4         Alona phylicifolia        Alona phylicifolia
#> 5              Cissus nodosa             Cissus nodosa
#> 6              Olyra caudata             Olyra caudata
#> 7         Vochysia oppugnata        Vochysia oppugnata
#> 8        Jadunia racemiflora       Jadunia racemiflora
#> 9         Senecio bulleyanus        Senecio bulleyanus
#> 10       Abutilon fruticosum       Abutilon fruticosum
#>                          matched_name      data_source_title score
#> 1            Sedum morganianum Walth. GBIF Backbone Taxonomy 0.988
#> 2                                                              NaN
#> 3          Lupinus syriggedes C.P.Sm. GBIF Backbone Taxonomy 0.988
#> 4            Alona phylicifolia Phil. GBIF Backbone Taxonomy 0.988
#> 5                 Cissus nodosa Blume GBIF Backbone Taxonomy 0.988
#> 6                 Olyra caudata Trin. GBIF Backbone Taxonomy 0.988
#> 7            Vochysia oppugnata Warm. GBIF Backbone Taxonomy 0.988
#> 8         Jadunia racemiflora Bremek. GBIF Backbone Taxonomy 0.988
#> 9            Senecio bulleyanus Diels GBIF Backbone Taxonomy 0.988
#> 10 Abutilon fruticosum Guill. & Perr. GBIF Backbone Taxonomy 0.988
```
