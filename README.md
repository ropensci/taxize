taxize
======



[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![cran checks](https://cranchecks.info/badges/worst/taxize)](https://cranchecks.info/pkgs/taxize)
[![R-CMD-check](https://github.com/ropensci/taxize/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci/taxize/actions/)
[![codecov](https://codecov.io/gh/ropensci/taxize/branch/master/graph/badge.svg)](https://codecov.io/gh/ropensci/taxize)
[![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/taxize)](https://github.com/metacran/cranlogs.app)
[![cran version](https://www.r-pkg.org/badges/version/taxize)](https://cran.r-project.org/package=taxize)


`taxize` allows users to search over many taxonomic data sources for species names (scientific and common) and download up and downstream taxonomic hierarchical information - among other things.

The taxize book: https://taxize.dev

Package documentation: https://docs.ropensci.org/taxize/

## Data sources currently implemented

<table>
<colgroup>
<col style="text-align:left;"/>
<col style="text-align:left;"/>
<col style="text-align:left;"/>
<col style="text-align:left;"/>
</colgroup>

<thead>
<tr>
  <th style="text-align:left;">Souce</th>
	<th style="text-align:left;">Function prefix</th>
	<th style="text-align:left;">API Docs</th>
	<th style="text-align:left;">API key</th>
</tr>
</thead>

<tbody>
<tr>
	<td style="text-align:left;">Encylopedia of Life</td>
	<td style="text-align:left;"><code>eol</code></td>
	<td style="text-align:left;"><a href="https://eol.org/docs/what-is-eol/data-services">link</a></td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">Taxonomic Name Resolution Service</td>
	<td style="text-align:left;"><code>tnrs</code></td>
	<td style="text-align:left;">none</td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">Integrated Taxonomic Information Service</td>
	<td style="text-align:left;"><code>itis</code></td>
	<td style="text-align:left;"><a href="https://www.itis.gov/ws_description.html">link</a></td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">Global Names Resolver</td>
	<td style="text-align:left;"><code>gnr</code></td>
	<td style="text-align:left;"><a href="http://resolver.globalnames.org/api">link</a></td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">Global Names Index</td>
	<td style="text-align:left;"><code>gni</code></td>
	<td style="text-align:left;"><a href="https://github.com/dimus/gni/wiki/api">link</a></td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">IUCN Red List</td>
	<td style="text-align:left;"><code>iucn</code></td>
	<td style="text-align:left;"><a href="https://apiv3.iucnredlist.org/api/v3/docs">link</a></td>
	<td style="text-align:left;"><a href="https://apiv3.iucnredlist.org/api/v3/token">link</a></td>
</tr>
<tr>
	<td style="text-align:left;">Tropicos</td>
	<td style="text-align:left;"><code>tp</code></td>
	<td style="text-align:left;"><a href="http://services.tropicos.org/help">link</a></td>
	<td style="text-align:left;"><a href="http://services.tropicos.org/help?requestkey">link</a></td>
</tr>
<tr>
	<td style="text-align:left;">Theplantlist dot org</td>
	<td style="text-align:left;"><code>tpl</code></td>
	<td style="text-align:left;">**</td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">National Center for Biotechnology Information</td>
	<td style="text-align:left;"><code>ncbi</code></td>
	<td style="text-align:left;">none</td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">CANADENSYS Vascan name search API</td>
	<td style="text-align:left;"><code>vascan</code></td>
	<td style="text-align:left;"><a href="https://data.canadensys.net/vascan/api">link</a></td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">International Plant Names Index (IPNI)</td>
	<td style="text-align:left;"><code>ipni</code></td>
	<td style="text-align:left;">none</td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">Barcode of Life Data Systems (BOLD)</td>
	<td style="text-align:left;"><code>bold</code></td>
	<td style="text-align:left;"><a href="http://www.boldsystems.org/index.php/Resources">link</a></td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">National Biodiversity Network (UK)</td>
	<td style="text-align:left;"><code>nbn</code></td>
	<td style="text-align:left;"><a href="https://data.nbn.org.uk/Documentation/Web_Services/Web_Services-REST/resources/restapi/rest.html">link</a></td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">Index Fungorum</td>
	<td style="text-align:left;"><code>fg</code></td>
	<td style="text-align:left;">none</td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">EU BON</td>
	<td style="text-align:left;"><code>eubon</code></td>
	<td style="text-align:left;"><a href="https://cybertaxonomy.eu/eubon-utis/doc.html">link</a></td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">Index of Names (ION)</td>
	<td style="text-align:left;"><code>ion</code></td>
	<td style="text-align:left;"><a href="http://www.organismnames.com/">link</a></td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">Open Tree of Life (TOL)</td>
	<td style="text-align:left;"><code>tol</code></td>
	<td style="text-align:left;"><a href="https://github.com/OpenTreeOfLife/germinator/wiki/Open-Tree-of-Life-Web-APIs">link</a></td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">World Register of Marine Species (WoRMS)</td>
	<td style="text-align:left;"><code>worms</code></td>
	<td style="text-align:left;"><a href="https://www.marinespecies.org/aphia.php?p=webservice">link</a></td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">NatureServe</td>
	<td style="text-align:left;"><code>natserv</code></td>
	<td style="text-align:left;"><a href="https://services.natureserve.org/BrowseServices/getSpeciesData/getSpeciesListREST.jsp">link</a></td>
	<td style="text-align:left;"><a href="https://services.natureserve.org/developer/index.jsp">link</a></td>
</tr>
<tr>
	<td style="text-align:left;">Wikipedia</td>
	<td style="text-align:left;"><code>wiki</code></td>
	<td style="text-align:left;"><a href="https://www.mediawiki.org/wiki/API:Main_page">link</a></td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">Kew's Plants of the World</td>
	<td style="text-align:left;"><code>pow</code></td>
	<td style="text-align:left;">none</td>
	<td style="text-align:left;">none</td>
</tr>
</tbody>
</table>

**: There are none! We suggest using `TPL` and `TPLck` functions in the [taxonstand package](https://cran.r-project.org/package=Taxonstand). We provide two functions to get bulk data: `tpl_families` and `tpl_get`.

\***: There are none! The function scrapes the web directly.

### May be in taxize in the future...

See the [datasources](https://github.com/ropensci/taxize/labels/datasources) tag in the issue tracker

<br>

## Installation

### Stable version from CRAN


```r
install.packages("taxize")
```

### Development version from GitHub

Windows users install Rtools first.


```r
install.packages("remotes")
remotes::install_github("ropensci/taxize")
```


```r
library('taxize')
```



## Get unique taxonomic identifier from NCBI

Alot of `taxize` revolves around taxonomic identifiers. Because, as you know, names can be a mess (misspelled, synonyms, etc.), it's better to get an identifier that a particular data source knows about, then we can move forth acquiring more fun taxonomic data.


```r
uids <- get_uid(c("Chironomus riparius", "Chaetopteryx"))
#> ══  2 queries  ═══════════════
#> ✔  Found:  Chironomus+riparius
#> ✔  Found:  Chaetopteryx
#> ══  Results  ═════════════════
#> 
#> ● Total: 2 
#> ● Found: 2 
#> ● Not Found: 0
```

## Retrieve classifications

Classifications - think of a species, then all the taxonomic ranks up from that species, like genus, family, order, class, kingdom.


```r
out <- classification(uids)
lapply(out, head)
#> $`315576`
#>                 name         rank     id
#> 1 cellular organisms      no rank 131567
#> 2          Eukaryota superkingdom   2759
#> 3       Opisthokonta      no rank  33154
#> 4            Metazoa      kingdom  33208
#> 5          Eumetazoa      no rank   6072
#> 6          Bilateria      no rank  33213
#> 
#> $`492549`
#>                 name         rank     id
#> 1 cellular organisms      no rank 131567
#> 2          Eukaryota superkingdom   2759
#> 3       Opisthokonta      no rank  33154
#> 4            Metazoa      kingdom  33208
#> 5          Eumetazoa      no rank   6072
#> 6          Bilateria      no rank  33213
```

## Immediate children

Get immediate children of _Salmo_. In this case, _Salmo_ is a genus, so this gives species within the genus.


```r
children("Salmo", db = 'ncbi')
#> $Salmo
#>    childtaxa_id                   childtaxa_name childtaxa_rank
#> 1       2705433                     Salmo ghigii        species
#> 2       2304090                  Salmo abanticus        species
#> 3       2126688              Salmo ciscaucasicus        species
#> 4       1509524  Salmo marmoratus x Salmo trutta        species
#> 5       1484545 Salmo cf. cenerinus BOLD:AAB3872        species
#> 6       1483130               Salmo zrmanjaensis        species
#> 7       1483129               Salmo visovacensis        species
#> 8       1483128                Salmo rhodanensis        species
#> 9       1483127                 Salmo pellegrini        species
#> 10      1483126                     Salmo opimus        species
#> 11      1483125                Salmo macedonicus        species
#> 12      1483124                Salmo lourosensis        species
#> 13      1483123                   Salmo labecula        species
#> 14      1483122                  Salmo farioides        species
#> 15      1483121                      Salmo chilo        species
#> 16      1483120                     Salmo cettii        species
#> 17      1483119                  Salmo cenerinus        species
#> 18      1483118                   Salmo aphelios        species
#> 19      1483117                    Salmo akairos        species
#> 20      1201173               Salmo peristericus        species
#> 21      1035833                   Salmo ischchan        species
#> 22       700588                     Salmo labrax        species
#> 23       602068                    Salmo caspius        species
#> 24       237411              Salmo obtusirostris        species
#> 25       235141              Salmo platycephalus        species
#> 26       234793                    Salmo letnica        species
#> 27        62065                  Salmo ohridanus        species
#> 28        33518                 Salmo marmoratus        species
#> 29        33516                    Salmo fibreni        species
#> 30        33515                     Salmo carpio        species
#> 31         8032                     Salmo trutta        species
#> 32         8030                      Salmo salar        species
#> 
#> attr(,"class")
#> [1] "children"
#> attr(,"db")
#> [1] "ncbi"
```

## Downstream children to a rank

Get all species in the genus _Apis_


```r
downstream(as.tsn(154395), db = 'itis', downto = 'species', mesages = FALSE)
#> $`154395`
#>      tsn parentname parenttsn rankname          taxonname rankid
#> 1 154396       Apis    154395  species     Apis mellifera    220
#> 2 763550       Apis    154395  species Apis andreniformis    220
#> 3 763551       Apis    154395  species        Apis cerana    220
#> 4 763552       Apis    154395  species       Apis dorsata    220
#> 5 763553       Apis    154395  species        Apis florea    220
#> 6 763554       Apis    154395  species Apis koschevnikovi    220
#> 7 763555       Apis    154395  species   Apis nigrocincta    220
#> 
#> attr(,"class")
#> [1] "downstream"
#> attr(,"db")
#> [1] "itis"
```

## Upstream taxa

Get all genera up from the species _Pinus contorta_ (this includes the genus of the species, and its co-genera within the same family).


```r
upstream("Pinus contorta", db = 'itis', upto = 'Genus', mesages = FALSE)
#> ══  1 queries  ═══════════════
#> ✔  Found:  Pinus contorta
#> ══  Results  ═════════════════
#> 
#> ● Total: 1 
#> ● Found: 1 
#> ● Not Found: 0
#> $`Pinus contorta`
#>      tsn parentname parenttsn rankname   taxonname rankid
#> 1  18031   Pinaceae     18030    genus       Abies    180
#> 2  18033   Pinaceae     18030    genus       Picea    180
#> 3  18035   Pinaceae     18030    genus       Pinus    180
#> 4 183396   Pinaceae     18030    genus       Tsuga    180
#> 5 183405   Pinaceae     18030    genus      Cedrus    180
#> 6 183409   Pinaceae     18030    genus       Larix    180
#> 7 183418   Pinaceae     18030    genus Pseudotsuga    180
#> 8 822529   Pinaceae     18030    genus  Keteleeria    180
#> 9 822530   Pinaceae     18030    genus Pseudolarix    180
#> 
#> attr(,"class")
#> [1] "upstream"
#> attr(,"db")
#> [1] "itis"
```

## Get synonyms


```r
synonyms("Acer drummondii", db="itis")
#> ══  1 queries  ═══════════════
#> ✔  Found:  Acer drummondii
#> ══  Results  ═════════════════
#> 
#> ● Total: 1 
#> ● Found: 1 
#> ● Not Found: 0
#> $`Acer drummondii`
#>   sub_tsn                    acc_name acc_tsn                    acc_author
#> 1  183671 Acer rubrum var. drummondii  526853 (Hook. & Arn. ex Nutt.) Sarg.
#> 2  183671 Acer rubrum var. drummondii  526853 (Hook. & Arn. ex Nutt.) Sarg.
#> 3  183671 Acer rubrum var. drummondii  526853 (Hook. & Arn. ex Nutt.) Sarg.
#>                          syn_author                    syn_name syn_tsn
#> 1 (Hook. & Arn. ex Nutt.) E. Murray Acer rubrum ssp. drummondii   28730
#> 2             Hook. & Arn. ex Nutt.             Acer drummondii  183671
#> 3     (Hook. & Arn. ex Nutt.) Small          Rufacer drummondii  183672
#> 
#> attr(,"class")
#> [1] "synonyms"
#> attr(,"db")
#> [1] "itis"
```

## Get taxonomic IDs from many sources


```r
get_ids("Salvelinus fontinalis", db = c('itis', 'ncbi'), mesages = FALSE)
#> ══  db: itis ═════════════════
#> ══  1 queries  ═══════════════
#> ✔  Found:  Salvelinus fontinalis
#> ══  Results  ═════════════════
#> 
#> ● Total: 1 
#> ● Found: 1 
#> ● Not Found: 0
#> ══  db: ncbi ═════════════════
#> ══  1 queries  ═══════════════
#> ✔  Found:  Salvelinus+fontinalis
#> ══  Results  ═════════════════
#> 
#> ● Total: 1 
#> ● Found: 1 
#> ● Not Found: 0
#> $itis
#> Salvelinus fontinalis 
#>              "162003" 
#> attr(,"class")
#> [1] "tsn"
#> attr(,"match")
#> [1] "found"
#> attr(,"multiple_matches")
#> [1] FALSE
#> attr(,"pattern_match")
#> [1] FALSE
#> attr(,"uri")
#> [1] "https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=162003"
#> 
#> $ncbi
#> Salvelinus fontinalis 
#>                "8038" 
#> attr(,"class")
#> [1] "uid"
#> attr(,"match")
#> [1] "found"
#> attr(,"multiple_matches")
#> [1] FALSE
#> attr(,"pattern_match")
#> [1] FALSE
#> attr(,"uri")
#> [1] "https://www.ncbi.nlm.nih.gov/taxonomy/8038"
#> 
#> attr(,"class")
#> [1] "ids"
```

You can limit to certain rows when getting ids in any `get_*()` functions


```r
get_ids("Poa annua", db = "gbif", rows=1)
#> ══  db: gbif ═════════════════
#> ══  1 queries  ═══════════════
#> ✔  Found:  Poa annua
#> ══  Results  ═════════════════
#> 
#> ● Total: 1 
#> ● Found: 1 
#> ● Not Found: 0
#> $gbif
#> Poa annua 
#> "2704179" 
#> attr(,"class")
#> [1] "gbifid"
#> attr(,"match")
#> [1] "found"
#> attr(,"multiple_matches")
#> [1] TRUE
#> attr(,"pattern_match")
#> [1] FALSE
#> attr(,"uri")
#> [1] "https://www.gbif.org/species/2704179"
#> 
#> attr(,"class")
#> [1] "ids"
```

Furthermore, you can just back all ids if that's your jam with the `get_*_()` functions (all `get_*()` functions with additional `_` underscore at end of function name)


```r
get_ids_(c("Chironomus riparius", "Pinus contorta"), db = 'nbn', rows=1:3)
#> ══  db: nbn ══════════════════
#> $nbn
#> $nbn$`Chironomus riparius`
#>               guid      scientificName    rank taxonomicStatus
#> 1 NBNSYS0000027573 Chironomus riparius species        accepted
#> 2 NBNSYS0000007169   Elaphrus riparius species        accepted
#> 3 NBNSYS0000023573    Quedius riparius species        accepted
#> 
#> $nbn$`Pinus contorta`
#>               guid                scientificName    rank taxonomicStatus
#> 1 NBNSYS0000004786                Pinus contorta species        accepted
#> 2 NHMSYS0000494848  Pinus contorta var. contorta variety        accepted
#> 3 NHMSYS0000494858 Pinus contorta var. murrayana variety        accepted
#> 
#> 
#> attr(,"class")
#> [1] "ids"
```

## Common names from scientific names


```r
sci2comm('Helianthus annuus', db = 'itis')
#> ══  1 queries  ═══════════════
#> ✔  Found:  Helianthus annuus
#> ══  Results  ═════════════════
#> 
#> ● Total: 1 
#> ● Found: 1 
#> ● Not Found: 0
#> $`Helianthus annuus`
#> [1] "common sunflower" "sunflower"        "wild sunflower"   "annual sunflower"
```

## Scientific names from common names


```r
comm2sci("black bear", db = "itis")
#> $`black bear`
#> [1] "Ursus americanus luteolus"   "Ursus americanus"           
#> [3] "Ursus americanus"            "Ursus americanus americanus"
#> [5] "Chiropotes satanas"          "Ursus thibetanus"           
#> [7] "Ursus thibetanus"
```

## Lowest common rank among taxa


```r
spp <- c("Sus scrofa", "Homo sapiens", "Nycticebus coucang")
lowest_common(spp, db = "ncbi")
#> ══  3 queries  ═══════════════
#> ✔  Found:  Sus+scrofa
#> ✔  Found:  Homo+sapiens
#> ✔  Found:  Nycticebus+coucang
#> ══  Results  ═════════════════
#> 
#> ● Total: 3 
#> ● Found: 3 
#> ● Not Found: 0
#> ══  3 queries  ═══════════════
#> ✔  Found:  Sus+scrofa
#> ✔  Found:  Homo+sapiens
#> ✔  Found:  Nycticebus+coucang
#> ══  Results  ═════════════════
#> 
#> ● Total: 3 
#> ● Found: 3 
#> ● Not Found: 0
#>             name        rank      id
#> 21 Boreoeutheria below-class 1437010
```

## Coerce codes to taxonomic id classes

`numeric` to `uid`


```r
as.uid(315567)
#> [1] "315567"
#> attr(,"class")
#> [1] "uid"
#> attr(,"match")
#> [1] "found"
#> attr(,"multiple_matches")
#> [1] FALSE
#> attr(,"pattern_match")
#> [1] FALSE
#> attr(,"uri")
#> [1] "https://www.ncbi.nlm.nih.gov/taxonomy/315567"
```

`list` to `uid`


```r
as.uid(list("315567", "3339", "9696"))
#> [1] "315567" "3339"   "9696"  
#> attr(,"class")
#> [1] "uid"
#> attr(,"match")
#> [1] "found" "found" "found"
#> attr(,"multiple_matches")
#> [1] FALSE FALSE FALSE
#> attr(,"pattern_match")
#> [1] FALSE FALSE FALSE
#> attr(,"uri")
#> [1] "https://www.ncbi.nlm.nih.gov/taxonomy/315567"
#> [2] "https://www.ncbi.nlm.nih.gov/taxonomy/3339"  
#> [3] "https://www.ncbi.nlm.nih.gov/taxonomy/9696"
```

## Coerce taxonomic id classes to a data.frame


```r
out <- as.uid(c(315567, 3339, 9696))
(res <- data.frame(out))
#>      ids class match multiple_matches pattern_match
#> 1 315567   uid found            FALSE         FALSE
#> 2   3339   uid found            FALSE         FALSE
#> 3   9696   uid found            FALSE         FALSE
#>                                            uri
#> 1 https://www.ncbi.nlm.nih.gov/taxonomy/315567
#> 2   https://www.ncbi.nlm.nih.gov/taxonomy/3339
#> 3   https://www.ncbi.nlm.nih.gov/taxonomy/9696
```

## Screencast

<a href="https://vimeo.com/92883063"><img src="man/figures/screencast.png" width="400"></a>


## Contributing

See our [CONTRIBUTING](https://github.com/ropensci/taxize/blob/master/.github/CONTRIBUTING.md) document.


## Road map

Check out our [milestones](https://github.com/ropensci/taxize/milestones) to see what we plan to get done for each version.

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/taxize/issues).
* License: MIT
* Get citation information for `taxize` in R doing `citation(package = 'taxize')`
* Please note that this project is released with a [Contributor Code of Conduct][coc].
By participating in this project you agree to abide by its terms.

[![rofooter](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)

[coc]: https://github.com/ropensci/taxize/blob/master/CODE_OF_CONDUCT.md
