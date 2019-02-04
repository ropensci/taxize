<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{Case study - resolving Species Plantarum names}
%\VignetteEncoding{UTF-8}
-->

Case study - resolving Species Plantarum names
==============================================

*Species Plantarum* by Carl Linnaeus originally published in 1753 and was the first work to consistently describe species using binominal names. It provides a reference point to chart how the usage of 5940 taxa described within have changed over time. So let's use the taxize package see if we can resolve these names and see their current status. 

Firstly, let's see how many of the names still exist in modern species catologues today. We can use the `resolve` function to query the Global Name Resolver.


```r
library("taxize")
library("knitr")
# load dataset of Species Plantarum names from the taxize package
names <- species_plantarum_binomials
# Species Plantarum names are split into Genus and Species qualifier so need to be paste them
# together to allow the full binominal names to be sent to the global name resolver
names$species <- paste(names$genus, names$epithet)
# use resolve function to send binominal names to resolver (this may take some time)
resolved_names <- resolve(names$species)
# select only the dataframe column from the list send back from gnr
resolved_names <- resolved_names$gnr
# count the distinct names resolved per dataset source
resolved_names_source <- resolved_names[, c("submitted_name", "data_source_title")] 
resolved_names_source <- unique(resolved_names_source)
resolved_names_source$count <- 1
summary_resolved <- aggregate(resolved_names_source$count, list(data_source = resolved_names_source$data_source_title), FUN = "sum")
summary_resolved <- summary_resolved[with(summary_resolved, order(-x)),]
kable(head(summary_resolved, 20))
```



|   |data_source                                         |    x|
|:--|:---------------------------------------------------|----:|
|21 |GBIF Backbone Taxonomy                              | 5872|
|52 |uBio NameBank                                       | 5861|
|2  |Arctos                                              | 5513|
|7  |Catalogue of Life                                   | 5443|
|40 |The International Plant Names Index                 | 5435|
|43 |Tropicos - Missouri Botanical Garden                | 5140|
|13 |EOL                                                 | 4771|
|22 |GRIN Taxonomy for Plants                            | 3633|
|5  |BioLib.cz                                           | 3329|
|34 |Open Tree of Life Reference Taxonomy                | 3321|
|39 |The Interim Register of Marine and Nonmarine Genera | 3110|
|25 |ITIS                                                | 2832|
|32 |NCBI                                                | 2798|
|14 |EUNIS                                               | 2464|
|50 |nlbif                                               | 2372|
|44 |USDA NRCS PLANTS Database                           | 2338|
|49 |iNaturalist                                         | 1777|
|45 |Union 4                                             | 1697|
|19 |Freebase                                            | 1581|
|6  |CU*STAR                                             | 1525|

The table above shows none of the data sources provided can resolve all of the names in the *Species Plantarum*. Let's see if between all the data sources we can resolve all the names.


```r
gnr_names <- resolved_names[, c("submitted_name", "score")] 
gnr_names <-  unique(gnr_names)

merge_gnr_plantarum <- merge(names, gnr_names, by.x = "species", by.y = "submitted_name", all.x = T)

merge_gnr_plantarum <- aggregate(merge_gnr_plantarum$score, list(data_source = merge_gnr_plantarum$species), FUN = "mean")

length(merge_gnr_plantarum$data_source)
```

```
## [1] 5940
```

```r
min(merge_gnr_plantarum$x, na.rm = T)
```

```
## [1] 0.75
```

```r
max(merge_gnr_plantarum$x, na.rm = T)
```

```
## [1] 0.9915
```

Even though no individual name data source can resolve all the names, taken together all names in *Species Plantarum* can be resolved using modern sources. Noting that some names score poorly in terms of fuzzy matching. The poor matching could indicate imperfect matches and perhaps some matches are due to species identified after 1752 sharing similar names. There are lots of questions regarding the history of many of these original binominals and their journey into present day taxonomy.  

For example, let's see how many of these names are still the 'accepted' names according to ITIS. Not all the data providers give an accepted name. So lets use ITIS as the `get_tsn_` function returns a `nameUsage` variable which indicates if a name is still accepted or not.


```r
# find resolved species Plantarum names returned from ITIS
itis_resolved <- resolved_names$submitted_name[resolved_names$data_source_title == "ITIS"]
# select random sub-selection to save time
set.seed(42) 
itis_sample <- itis_resolved[sample(rep(1:length(unique(itis_resolved))), 200)]
# query ITIS (this may take some time)
its_names <- get_tsn_(itis_sample)
its_names_bind <- do.call("rbind",its_names)
its_names_bind$n <- 1
# count the number of names in each nameUsage category 
its_names_agg <- aggregate(its_names_bind$n, list(data_source = its_names_bind$nameUsage), FUN = "sum")

kable(its_names_agg)
```



|data_source |   x|
|:-----------|---:|
|accepted    | 175|
|valid       |   1|

Of the thousands of *Species Plantarum* names found in the ITIS data source, a high number (of 200 randomly selected names) are still the 'accepted' name. This indicates Carl Linnaeus, as widely acknowledged, identified well-thought-out and pragmatic taxonomic concepts which have proved useful until the present day. 

Further research may discover why some names appear not to be used today in some data sources and where they are used, why some are no longer the accepted name.



