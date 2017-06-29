taxize
======

[![Build Status](https://travis-ci.org/ropensci/taxize.svg?branch=master)](https://travis-ci.org/ropensci/taxize)
[![Build status](https://ci.appveyor.com/api/projects/status/6mgc02mkd8j4sq3g/branch/master)](https://ci.appveyor.com/project/sckott/taxize-175/branch/master)
[![codecov.io](https://codecov.io/github/ropensci/taxize/coverage.svg?branch=master)](https://codecov.io/github/ropensci/taxize?branch=master)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/taxize)](https://github.com/metacran/cranlogs.app)
[![cran version](http://www.r-pkg.org/badges/version/taxize)](https://cran.r-project.org/package=taxize)


`taxize` allows users to search over many taxonomic data sources for species names (scientific and common) and download up and downstream taxonomic hierarchical information - among other things.

The `taxize` tutorial is can be found at <https://ropensci.org/tutorials/taxize.html>

The functions in the package that hit a specific API have a prefix and suffix separated by an underscore. They follow the format of `service_whatitdoes`.  For example, `gnr_resolve` uses the Global Names Resolver API to resolve species names.  General functions in the package that don't hit a specific API don't have two words separated by an underscore, e.g., `classification`.

You need API keys for Encyclopedia of Life (EOL), and Tropicos.

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
	<td style="text-align:left;"><a href="http://www.eol.org/api/">link</a></td>
	<td style="text-align:left;"><a href="http://eol.org/users/register">link</a></td>
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
	<td style="text-align:left;"><a href="https://www.assembla.com/spaces/sis/wiki/Red_List_API?version=3">link</a></td>
	<td style="text-align:left;">none</td>
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
	<td style="text-align:left;"><a href="http://www.catalogueoflife.org/colwebsite/content/web-services">link</a></td>
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
	<td style="text-align:left;"><a href="https://opentreeoflife.org">link</a></td>
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
</tbody>
</table>

**: There are none! We suggest using `TPL` and `TPLck` functions in the [taxonstand package](https://cran.r-project.org/package=Taxonstand). We provide two functions to get bullk data: `tpl_families` and `tpl_get`.

\***: There are none! The function scrapes the web directly.

### May be in taxize in the future...

See the [newdatasource](https://github.com/ropensci/taxize/labels/newdatasource) tag in the issue tracker

## Tutorial

For more examples see the [tutorial][tut]

## Installation

### Stable version from CRAN


```r
install.packages("taxize")
```

### Development version from GitHub

Windows users install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) first.


```r
install.packages("devtools")
devtools::install_github("ropensci/taxize")
```


```r
library('taxize')
```

## Get unique taxonomic identifier from NCBI

Alot of `taxize` revolves around taxonomic identifiers. Because, as you know, names can be a mess (misspelled, synonyms, etc.), it's better to get an identifier that a particular data sources knows about, then we can move forth acquiring more fun taxonomic data.


```r
uids <- get_uid(c("Chironomus riparius", "Chaetopteryx"))
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
#> 1       1509524  Salmo marmoratus x Salmo trutta        species
#> 2       1484545 Salmo cf. cenerinus BOLD:AAB3872        species
#> 3       1483130               Salmo zrmanjaensis        species
#> 4       1483129               Salmo visovacensis        species
#> 5       1483128                Salmo rhodanensis        species
#> 6       1483127                 Salmo pellegrini        species
#> 7       1483126                     Salmo opimus        species
#> 8       1483125                Salmo macedonicus        species
#> 9       1483124                Salmo lourosensis        species
#> 10      1483123                   Salmo labecula        species
#> 11      1483122                  Salmo farioides        species
#> 12      1483121                      Salmo chilo        species
#> 13      1483120                     Salmo cettii        species
#> 14      1483119                  Salmo cenerinus        species
#> 15      1483118                   Salmo aphelios        species
#> 16      1483117                    Salmo akairos        species
#> 17      1201173               Salmo peristericus        species
#> 18      1035833                   Salmo ischchan        species
#> 19       700588                     Salmo labrax        species
#> 20       237411              Salmo obtusirostris        species
#> 21       235141              Salmo platycephalus        species
#> 22       234793                    Salmo letnica        species
#> 23        62065                  Salmo ohridanus        species
#> 24        33518                 Salmo marmoratus        species
#> 25        33516                    Salmo fibreni        species
#> 26        33515                     Salmo carpio        species
#> 27         8032                     Salmo trutta        species
#> 28         8030                      Salmo salar        species
#>
#> attr(,"class")
#> [1] "children"
#> attr(,"db")
#> [1] "ncbi"
```

## Downstream children to a rank

Get all species in the genus _Apis_


```r
downstream("Apis", db = 'itis', downto = 'Species', verbose = FALSE)
#> $Apis
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
upstream("Pinus contorta", db = 'itis', upto = 'Genus', verbose=FALSE)
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
#> $`Acer drummondii`
#>   sub_tsn                    acc_name acc_tsn
#> 1  526853 Acer rubrum var. drummondii  526853
#> 2  526853 Acer rubrum var. drummondii  526853
#> 3  526853 Acer rubrum var. drummondii  526853
#>                          author                            author
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
get_ids(names="Salvelinus fontinalis", db = c('itis', 'ncbi'), verbose=FALSE)
#> $itis
#> Salvelinus fontinalis
#>              "162003"
#> attr(,"match")
#> [1] "found"
#> attr(,"multiple_matches")
#> [1] FALSE
#> attr(,"pattern_match")
#> [1] FALSE
#> attr(,"uri")
#> [1] "http://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=162003"
#> attr(,"class")
#> [1] "tsn"
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
#> [1] "http://www.gbif.org/species/2704179"
#>
#> attr(,"class")
#> [1] "ids"
```

Furthermore, you can just back all ids if that's your jam with the `get_*_()` functions (all `get_*()` functions with additional `_` underscore at end of function name)


```r
get_ids_(c("Chironomus riparius", "Pinus contorta"), db = 'nbn', rows=1:3)
#> $nbn
#> $nbn$`Chironomus riparius`
#>   ptaxonversionkey    searchmatchtitle    rank  namestatus
#> 1 NBNSYS0000027573 Chironomus riparius species Recommended
#> 2 NHMSYS0001718042   Elaphrus riparius species Recommended
#> 3 NBNSYS0000023345   Paederus riparius species Recommended
#>
#> $nbn$`Pinus contorta`
#>   ptaxonversionkey               searchmatchtitle       rank  namestatus
#> 1 NHMSYS0000494848   Pinus contorta var. contorta    variety Recommended
#> 2 NBNSYS0000004786                 Pinus contorta    species Recommended
#> 3 NHMSYS0000494848 Pinus contorta subsp. contorta subspecies Recommended
#>
#>
#> attr(,"class")
#> [1] "ids"
```

## Common names from scientific names


```r
sci2comm('Helianthus annuus', db = 'itis')
#> $`Helianthus annuus`
#> [1] "common sunflower" "sunflower"        "wild sunflower"
#> [4] "annual sunflower"
```

## Scientific names from common names


```r
comm2sci("black bear", db = "itis")
#> $`black bear`
#> [1] "Ursus thibetanus"            "Ursus thibetanus"
#> [3] "Ursus americanus luteolus"   "Ursus americanus"
#> [5] "Ursus americanus"            "Ursus americanus americanus"
#> [7] "Chiropotes satanas"
```

## Lowest common rank among taxa


```r
spp <- c("Sus scrofa", "Homo sapiens", "Nycticebus coucang")
lowest_common(spp, db = "ncbi")
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
#> [1] "http://www.ncbi.nlm.nih.gov/taxonomy/315567"
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
#> [1] "http://www.ncbi.nlm.nih.gov/taxonomy/315567"
#> [2] "http://www.ncbi.nlm.nih.gov/taxonomy/3339"
#> [3] "http://www.ncbi.nlm.nih.gov/taxonomy/9696"
```

## Coerce taxonomic id classes to a data.frame


```r
out <- as.uid(c(315567, 3339, 9696))
(res <- data.frame(out))
#>      ids class match multiple_matches pattern_match
#> 1 315567   uid found            FALSE         FALSE
#> 2   3339   uid found            FALSE         FALSE
#> 3   9696   uid found            FALSE         FALSE
#>                                           uri
#> 1 http://www.ncbi.nlm.nih.gov/taxonomy/315567
#> 2   http://www.ncbi.nlm.nih.gov/taxonomy/3339
#> 3   http://www.ncbi.nlm.nih.gov/taxonomy/9696
```

## Contributors

Alphebetical

+ [Ignasi Bartomeus](https://github.com/ibartomeus)
+ [John Baumgartner](https://github.com/johnbaums)
+ [Carl Boettiger](https://github.com/cboettig)
+ [Joseph Brown](https://github.com/josephwb)
+ [Scott Chamberlain](https://github.com/sckott)
+ [Anirvan Chatterjee](https://github.com/anirvan)
+ [Zachary Foster](https://github.com/zachary-foster)
+ [Oliver Keyes](https://github.com/Ironholds)
+ [David LeBauer](https://github.com/dlebauer)
+ [Philippe Marchand](https://github.com/pmarchand1)
+ [Ben Marwick](https://github.com/benmarwick)
+ [Francois Michonneau](https://github.com/fmichonneau)
+ [James O'Donnell](https://github.com/jimmyodonnell)
+ [Jari Oksanen](https://github.com/jarioksa)
+ [Karthik Ram](https://github.com/karthik)
+ [raredd](https://github.com/raredd)
+ [Eduard Szöcs](https://github.com/EDiLD)
+ [Luis Villanueva](https://github.com/ljvillanueva)
+ [Jakub Wilk](https://github.com/jwilk)

## Road map

Check out our [milestones](https://github.com/ropensci/taxize/milestones) to see what we plan to get done for each version.

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/taxize/issues).
* License: MIT
* Get citation information for `taxize` in R doing `citation(package = 'taxize')`
* Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md).
By participating in this project you agree to abide by its terms.

[![ropensci](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)

[tut]: https://ropensci.org/tutorials/taxize.html
