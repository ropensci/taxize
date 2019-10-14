taxize
======




[![cran checks](https://cranchecks.info/badges/worst/taxize)](https://cranchecks.info/pkgs/taxize)
[![Build Status](https://travis-ci.org/ropensci/taxize.svg?branch=master)](https://travis-ci.org/ropensci/taxize)
[![Build status](https://ci.appveyor.com/api/projects/status/6mgc02mkd8j4sq3g/branch/master)](https://ci.appveyor.com/project/sckott/taxize-175/branch/master)
[![codecov.io](https://codecov.io/github/ropensci/taxize/coverage.svg?branch=master)](https://codecov.io/github/ropensci/taxize?branch=master)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/taxize)](https://github.com/metacran/cranlogs.app)
[![cran version](http://www.r-pkg.org/badges/version/taxize)](https://cran.r-project.org/package=taxize)


`taxize` allows users to search over many taxonomic data sources for species names (scientific and common) and download up and downstream taxonomic hierarchical information - among other things.

The taxize book => <https://taxize.dev>

The functions in the package that hit a specific API have a prefix and suffix separated by an underscore. They follow the format of `service_whatitdoes`.  For example, `gnr_resolve` uses the Global Names Resolver API to resolve species names.  General functions in the package that don't hit a specific API don't have two words separated by an underscore, e.g., `classification`.

You need API keys for Encyclopedia of Life (EOL), Tropicos, IUCN, and NatureServe.

## SOAP

Note that a few data sources require SOAP web services, which are difficult to support in R across all operating systems. These include: Pan-European Species directories Infrastructure and Mycobank. Data sources that use SOAP web services have been moved to `taxizesoap` at <https://github.com/ropensci/taxizesoap>.

## Currently implemented in `taxize`

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
	<td style="text-align:left;"><a href="https://eol.org/docs/what-is-eol/data-services">link</a></td>
</tr>
<tr>
	<td style="text-align:left;">Taxonomic Name Resolution Service</td>
	<td style="text-align:left;"><code>tnrs</code></td>
	<td style="text-align:left;">"api.phylotastic.org/tnrs"</td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">Integrated Taxonomic Information Service</td>
	<td style="text-align:left;"><code>itis</code></td>
	<td style="text-align:left;"><a href="http://www.itis.gov/ws_description.html">link</a></td>
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
	<td style="text-align:left;"><a href="http://apiv3.iucnredlist.org/api/v3/docs">link</a></td>
	<td style="text-align:left;"><a href="http://apiv3.iucnredlist.org/api/v3/token">link</a></td>
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
	<td style="text-align:left;">Catalogue of Life</td>
	<td style="text-align:left;"><code>col</code></td>
	<td style="text-align:left;"><a href="http://www.catalogueoflife.org/content/web-services">link</a></td>
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
	<td style="text-align:left;"><a href="http://data.canadensys.net/vascan/api">link</a></td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">International Plant Names Index (IPNI)</td>
	<td style="text-align:left;"><code>ipni</code></td>
	<td style="text-align:left;"><a href="http://www.ipni.org/link_to_ipni.html">link</a></td>
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
	<td style="text-align:left;"><a href="http://www.indexfungorum.org/ixfwebservice/fungus.asmx">link</a></td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">EU BON</td>
	<td style="text-align:left;"><code>eubon</code></td>
	<td style="text-align:left;"><a href="http://cybertaxonomy.eu/eubon-utis/doc.html">link</a></td>
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
	<td style="text-align:left;"><a href="http://www.marinespecies.org/aphia.php?p=webservice">link</a></td>
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

See the [newdatasource](https://github.com/ropensci/taxize/labels/newdatasource) tag in the issue tracker

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
#> 1       2304090                  Salmo abanticus        species
#> 2       2126688              Salmo ciscaucasicus        species
#> 3       1509524  Salmo marmoratus x Salmo trutta        species
#> 4       1484545 Salmo cf. cenerinus BOLD:AAB3872        species
#> 5       1483130               Salmo zrmanjaensis        species
#> 6       1483129               Salmo visovacensis        species
#> 7       1483128                Salmo rhodanensis        species
#> 8       1483127                 Salmo pellegrini        species
#> 9       1483126                     Salmo opimus        species
#> 10      1483125                Salmo macedonicus        species
#> 11      1483124                Salmo lourosensis        species
#> 12      1483123                   Salmo labecula        species
#> 13      1483122                  Salmo farioides        species
#> 14      1483121                      Salmo chilo        species
#> 15      1483120                     Salmo cettii        species
#> 16      1483119                  Salmo cenerinus        species
#> 17      1483118                   Salmo aphelios        species
#> 18      1483117                    Salmo akairos        species
#> 19      1201173               Salmo peristericus        species
#> 20      1035833                   Salmo ischchan        species
#> 21       700588                     Salmo labrax        species
#> 22       602068                    Salmo caspius     subspecies
#> 23       237411              Salmo obtusirostris        species
#> 24       235141              Salmo platycephalus        species
#> 25       234793                    Salmo letnica        species
#> 26        62065                  Salmo ohridanus        species
#> 27        33518                 Salmo marmoratus        species
#> 28        33516                    Salmo fibreni        species
#> 29        33515                     Salmo carpio        species
#> 30         8032                     Salmo trutta        species
#> 31         8030                      Salmo salar        species
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
#>      tsn parentname parenttsn          taxonname rankid rankname
#> 1 154396       Apis    154395     Apis mellifera    220  species
#> 2 763550       Apis    154395 Apis andreniformis    220  species
#> 3 763551       Apis    154395        Apis cerana    220  species
#> 4 763552       Apis    154395       Apis dorsata    220  species
#> 5 763553       Apis    154395        Apis florea    220  species
#> 6 763554       Apis    154395 Apis koschevnikovi    220  species
#> 7 763555       Apis    154395   Apis nigrocincta    220  species
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
#>      tsn parentname parenttsn   taxonname rankid rankname
#> 1  18031   Pinaceae     18030       Abies    180    genus
#> 2  18033   Pinaceae     18030       Picea    180    genus
#> 3  18035   Pinaceae     18030       Pinus    180    genus
#> 4 183396   Pinaceae     18030       Tsuga    180    genus
#> 5 183405   Pinaceae     18030      Cedrus    180    genus
#> 6 183409   Pinaceae     18030       Larix    180    genus
#> 7 183418   Pinaceae     18030 Pseudotsuga    180    genus
#> 8 822529   Pinaceae     18030  Keteleeria    180    genus
#> 9 822530   Pinaceae     18030 Pseudolarix    180    genus
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
#>   sub_tsn                    acc_name acc_tsn
#> 1  183671 Acer rubrum var. drummondii  526853
#> 2  183671 Acer rubrum var. drummondii  526853
#> 3  183671 Acer rubrum var. drummondii  526853
#>                      acc_author                        syn_author
#> 1 (Hook. & Arn. ex Nutt.) Sarg. (Hook. & Arn. ex Nutt.) E. Murray
#> 2 (Hook. & Arn. ex Nutt.) Sarg.             Hook. & Arn. ex Nutt.
#> 3 (Hook. & Arn. ex Nutt.) Sarg.     (Hook. & Arn. ex Nutt.) Small
#>                      syn_name syn_tsn
#> 1 Acer rubrum ssp. drummondii   28730
#> 2             Acer drummondii  183671
#> 3          Rufacer drummondii  183672
#> 
#> attr(,"class")
#> [1] "synonyms"
#> attr(,"db")
#> [1] "itis"
```

## Get taxonomic IDs from many sources


```r
get_ids(names="Salvelinus fontinalis", db = c('itis', 'ncbi'), mesages = FALSE)
#> ══  1 queries  ═══════════════
#> ✔  Found:  Salvelinus fontinalis
#> ══  Results  ═════════════════
#> 
#> ● Total: 1 
#> ● Found: 1 
#> ● Not Found: 0
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
get_ids(names="Poa annua", db = "gbif", rows=1)
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
#> $nbn
#> $nbn$`Chironomus riparius`
#>               guid      scientificName    rank taxonomicStatus
#> 1 NBNSYS0000027573 Chironomus riparius species        accepted
#> 2 NHMSYS0001718585  Hypnoidus riparius species        accepted
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
#> [1] "common sunflower" "sunflower"        "wild sunflower"  
#> [4] "annual sunflower"
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

<a href="https://vimeo.com/92883063"><img src="tools/screencast.png" width="400"></a>


## Contributing

See our [CONTRIBUTING](https://github.com/ropensci/taxize/blob/master/.github/CONTRIBUTING.md) document.

## Contributors

Alphebetical

### Code Contributors

+ [Zebulun Arendsee](https://github.com/arendsee)
+ [Ignasi Bartomeus](https://github.com/ibartomeus)
+ [John Baumgartner](https://github.com/johnbaums)
+ [Carl Boettiger](https://github.com/cboettig)
+ [Joseph Brown](https://github.com/josephwb)
+ [Scott Chamberlain](https://github.com/sckott)
+ [Anirvan Chatterjee](https://github.com/anirvan)
+ [Zachary Foster](https://github.com/zachary-foster)
+ [Matthias Grenié](https://github.com/Rekyt)
+ [Patrick Hausmann](https://github.com/patperu)
+ [Oliver Keyes](https://github.com/Ironholds)
+ [David LeBauer](https://github.com/dlebauer)
+ [Philippe Marchand](https://github.com/pmarchand1)
+ [Ben Marwick](https://github.com/benmarwick)
+ [Francois Michonneau](https://github.com/fmichonneau)
+ [James O'Donnell](https://github.com/jimmyodonnell)
+ [Jari Oksanen](https://github.com/jarioksa)
+ [Karthik Ram](https://github.com/karthik)
+ [raredd](https://github.com/raredd)
+ [Alexey Shiklomanov](https://github.com/ashiklom)
+ [Eduard Szöcs](https://github.com/EDiLD)
+ [Vinh Tran](https://github.com/trvinh)
+ [Bastian Greshake Tzovaras](https://github.com/gedankenstuecke)
+ [Luis Villanueva](https://github.com/ljvillanueva)
+ [Jakub Wilk](https://github.com/jwilk)

### All Contributors! 

Collected via GitHub Issues - this list honors all contributions, whether code or not.

Alphebetical

[afkoeppel](https://github.com/afkoeppel) - [ahhurlbert](https://github.com/ahhurlbert) - [albnd](https://github.com/albnd) - [Alectoria](https://github.com/Alectoria) - [andzandz11](https://github.com/andzandz11) - [antagomir](https://github.com/antagomir) - [arendsee](https://github.com/arendsee) - [ArielGreiner](https://github.com/ArielGreiner) - [arw36](https://github.com/arw36) - [ashenkin](https://github.com/ashenkin) - [ashiklom](https://github.com/ashiklom) - [benjaminschwetz](https://github.com/benjaminschwetz) - [benmarwick](https://github.com/benmarwick) - [bomeara](https://github.com/bomeara) - [bw4sz](https://github.com/bw4sz) - [cboettig](https://github.com/cboettig) - [cdeterman](https://github.com/cdeterman) - [ChrKoenig](https://github.com/ChrKoenig) - [chuckrp](https://github.com/chuckrp) - [clarson2191](https://github.com/clarson2191) - [claudenozeres](https://github.com/claudenozeres) - [cmzambranat](https://github.com/cmzambranat) - [cparsania](https://github.com/cparsania) - [daattali](https://github.com/daattali) - [DanielGMead](https://github.com/DanielGMead) - [DarrenObbard](https://github.com/DarrenObbard) - [davharris](https://github.com/davharris) - [davidvilanova](https://github.com/davidvilanova) - [diogoprov](https://github.com/diogoprov) - [dlebauer](https://github.com/dlebauer) - [dlenz1](https://github.com/dlenz1) - [dschlaep](https://github.com/dschlaep) - [EDiLD](https://github.com/EDiLD) - [edwbaker](https://github.com/edwbaker) - [emhart](https://github.com/emhart) - [eregenyi](https://github.com/eregenyi) - [fdschneider](https://github.com/fdschneider) - [fgabriel1891](https://github.com/fgabriel1891) - [fischhoff](https://github.com/fischhoff) - [fmichonneau](https://github.com/fmichonneau) - [fozy81](https://github.com/fozy81) - [gedankenstuecke](https://github.com/gedankenstuecke) - [GISKid](https://github.com/GISKid) - [git-og](https://github.com/git-og) - [glaroc](https://github.com/glaroc) - [gpli](https://github.com/gpli) - [gustavobio](https://github.com/gustavobio) - [hlapp](https://github.com/hlapp) - [ibartomeus](https://github.com/ibartomeus) - [Ironholds](https://github.com/Ironholds) - [jangorecki](https://github.com/jangorecki) - [jarioksa](https://github.com/jarioksa) - [jebyrnes](https://github.com/jebyrnes) - [jimmyodonnell](https://github.com/jimmyodonnell) - [johnbaums](https://github.com/johnbaums) - [jonmcalder](https://github.com/jonmcalder) - [josephwb](https://github.com/josephwb) - [jsgosnell](https://github.com/jsgosnell) - [jwilk](https://github.com/jwilk) - [kamapu](https://github.com/kamapu) - [karthik](https://github.com/karthik) - [katrinleinweber](https://github.com/katrinleinweber) - [KevCaz](https://github.com/KevCaz) - [kgturner](https://github.com/kgturner) - [kmeverson](https://github.com/kmeverson) - [Koalha](https://github.com/Koalha) - [ljvillanueva](https://github.com/ljvillanueva) - [maelle](https://github.com/maelle) - [Markus2015](https://github.com/Markus2015) - [mcsiple](https://github.com/mcsiple) - [MikkoVihtakari](https://github.com/MikkoVihtakari) - [millerjef](https://github.com/millerjef) - [miriamgrace](https://github.com/miriamgrace) - [MK212](https://github.com/MK212) - [mpnelsen](https://github.com/mpnelsen) - [MUSEZOOLVERT](https://github.com/MUSEZOOLVERT) - [nate-d-olson](https://github.com/nate-d-olson) - [nmatzke](https://github.com/nmatzke) - [npch](https://github.com/npch) - [paternogbc](https://github.com/paternogbc) - [patperu](https://github.com/patperu) - [pederengelstad](https://github.com/pederengelstad) - [philippi](https://github.com/philippi) - [pmarchand1](https://github.com/pmarchand1) - [PrincessPi314](https://github.com/PrincessPi314) - [pssguy](https://github.com/pssguy) - [raredd](https://github.com/raredd) - [rec3141](https://github.com/rec3141) - [Rekyt](https://github.com/Rekyt) - [RodgerG](https://github.com/RodgerG) - [rossmounce](https://github.com/rossmounce) - [sariya](https://github.com/sariya) - [scelmendorf](https://github.com/scelmendorf) - [sckott](https://github.com/sckott) - [SimonGoring](https://github.com/SimonGoring) - [snsheth](https://github.com/snsheth) - [snubian](https://github.com/snubian) - [Squiercg](https://github.com/Squiercg) - [taddallas](https://github.com/taddallas) - [tdjames1](https://github.com/tdjames1) - [tmkurobe](https://github.com/tmkurobe) - [toczydlowski](https://github.com/toczydlowski) - [tpaulson1](https://github.com/tpaulson1) - [tpoisot](https://github.com/tpoisot) - [vijaybarve](https://github.com/vijaybarve) - [wcornwell](https://github.com/wcornwell) - [willpearse](https://github.com/willpearse) - [wpetry](https://github.com/wpetry) - [yhg926](https://github.com/yhg926) - [zachary-foster](https://github.com/zachary-foster)

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
