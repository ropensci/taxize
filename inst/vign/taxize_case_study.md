<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{Strategies for programmatic name cleaning}
%\VignetteEncoding{UTF-8}
-->

Case study - resolving *Species Plantarum* names
================================================

*Species Plantarum* by Carl Linnaeus originally published in 1753, was the first work to consistently apply binominal names and was the starting point for the naming of plants. It provides useful reference point to see how taxonomic names have changed since their inception. So let's see if we can resolve these names and see if any of the names have changed. 

Firstly, let's see how many of the names still exist in modern species catologues today. We can use ```resolve``` function to query the Global Name Resolver.


```r
library("taxize")
library("tidyverse")
library("knitr")
# load dataset of Species Plantarum names from the taxize package
names <- species_plantarum_binomials
# Species Plantarum names are split into Genus and Species qualifier so need to be pasted together
# to allow the full binominal names to be sent to the global name resolver
names <- names %>% 
mutate(species = paste(names$genus,names$epithet))

resolved_names <- resolve(names$species)
resolved_names <- resolved_names$gnr
# count the distinct names resolved per dataset source
resolved_names_sum <- resolved_names %>% select(submitted_name, data_source_title) %>% 
  distinct() %>% group_by(data_source_title) %>%  tally() %>%  arrange(desc(n))

kable(head(resolved_names_sum,20))
```



|data_source_title                                   |    n|
|:---------------------------------------------------|----:|
|GBIF Backbone Taxonomy                              | 5872|
|uBio NameBank                                       | 5861|
|Arctos                                              | 5513|
|Catalogue of Life                                   | 5443|
|The International Plant Names Index                 | 5435|
|Tropicos - Missouri Botanical Garden                | 5140|
|EOL                                                 | 4771|
|GRIN Taxonomy for Plants                            | 3633|
|BioLib.cz                                           | 3329|
|Open Tree of Life Reference Taxonomy                | 3321|
|The Interim Register of Marine and Nonmarine Genera | 3110|
|ITIS                                                | 2832|
|NCBI                                                | 2798|
|EUNIS                                               | 2464|
|nlbif                                               | 2372|
|USDA NRCS PLANTS Database                           | 2338|
|iNaturalist                                         | 1777|
|Union 4                                             | 1697|
|Freebase                                            | 1581|
|CU*STAR                                             | 1525|

The tabe above shows none of the data sources provides can resolve all of the names in the *Species Plantarum*. Lets see if between all the data sources we can resolve all the names even.


```r
gnr_names <- select(resolved_names, submitted_name, score) %>% distinct()
merge_gnr_plantarum <- left_join(names, gnr_names, by = c("species" = "submitted_name")) %>%
  group_by(score) %>%  summarise("species count" = n()) %>% arrange(score)

kable(merge_gnr_plantarum)
```



| score| species count|
|-----:|-------------:|
| 0.750|           101|
| 0.988|          5839|
| 0.995|             3|

Even though no individual name data source can resolve all the names, taken together all names in *Species Plantarum* can be resolved using modern sources. Noting that some names socre poorly in terms of fuzzy matching. The poor matching maybe indicate imperfect matches and perahps some matches are due to species identified after 1752 sharing similar names. There are lots of questions regarding the history of many of these original binominals and their journey into present day taxonomy.  

For example, let's see how many of these names are still the accepted names according to ITIS.


```r
its_names <- get_tsn_(resolved_names$submitted_name[resolved_names$data_source_title == "ITIS"][
   sample(rep(1:length(unique(resolved_names$submitted_name[resolved_names$data_source_title == "ITIS"]))), 200)])
its_names <- bind_rows(its_names)
its_names_sum <- its_names %>%  group_by(nameUsage) %>% summarise(count = 100 / 205 * n()) 

kable(its_names_sum)
```



|nameUsage |    count|
|:---------|--------:|
|accepted  | 92.68293|


Of the `r, length(unique(resolved_names$submitted_name[resolved_names$data_source_title == "ITIS"]))` *Species Plantarum* names found in the ITIS, a high percentage (of 200 randomly selected names) are still the accepted names. This indicates Carl Linnaeus, as widely acknowledged, identified well thought out and pragmatic taxonomic concepts which have proved useful until the present day. Being the first to name and identify many plant species gives Linnaeus an advantage as subsequent taxonomist must either split or clump Linnaeus' definition into new species. 

So there is more research to discover why some names appear not to be used today in some data sources and if they are used, some are no longer the accepted name.

