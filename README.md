taxize
=======




[![Build Status](https://api.travis-ci.org/ropensci/taxize.png?branch=master)](https://travis-ci.org/ropensci/taxize)
[![Build status](https://ci.appveyor.com/api/projects/status/6mgc02mkd8j4sq3g/branch/master)](https://ci.appveyor.com/project/sckott/taxize-175/branch/master)
[![codecov.io](https://codecov.io/github/ropensci/taxize/coverage.svg?branch=master)](https://codecov.io/github/ropensci/taxize?branch=master)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/taxize)](https://github.com/metacran/cranlogs.app)
[![cran version](http://www.r-pkg.org/badges/version/taxize)](http://cran.rstudio.com/web/packages/taxize)


`taxize` allows users to search over many taxonomic data sources for species names (scientific and common) and download up and downstream taxonomic hierarchical information - among other things.

The `taxize` tutorial is can be found at [http://ropensci.org/tutorials/taxize.html][tut].

The functions in the package that hit a specific API have a prefix and suffix separated by an underscore. They follow the format of `service_whatitdoes`.  For example, `gnr_resolve` uses the Global Names Resolver API to resolve species names.  General functions in the package that don't hit a specific API don't have two words separated by an underscore, e.g., `classification`.

You need API keys for Encyclopedia of Life (EOL), the Universal Biological Indexer and Organizer (uBio), Tropicos, and Plantminer.

## SOAP

Note that a few data sources require SOAP web services, which are difficult to support in R across all operating systems. These include: World Register of Marine Species, Pan-European Species directories Infrastructure , and Mycobank, so far. Data sources that use SOAP web services have been moved to a new package called `taxizesoap`. Find it at [https://github.com/ropensci/taxizesoap](https://github.com/ropensci/taxizesoap).

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
	<td style="text-align:left;">Phylomatic</td>
	<td style="text-align:left;"><code>phylomatic</code></td>
	<td style="text-align:left;"><a href="http://phylodiversity.net/phylomatic/html/documentation.html">link</a></td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">uBio</td>
	<td style="text-align:left;"><code>ubio</code></td>
	<td style="text-align:left;"><a href="http://www.ubio.org/index.php?pagename=xml_services">link</a></td>
	<td style="text-align:left;"><a href="http://www.ubio.org/index.php?pagename=form">link</a></td>
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
	<td style="text-align:left;">Plantminer</td>
	<td style="text-align:left;"><code>plantminer</code></td>
	<td style="text-align:left;"><a href="http://www.plantminer.com/help">link</a></td>
	<td style="text-align:left;"><a href="http://www.plantminer.com/help">link</a></td>
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
	<td style="text-align:left;">Global Invasive Species Database</td>
	<td style="text-align:left;"><code>gisd</code></td>
	<td style="text-align:left;">***</td>
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
</tbody>
</table>

**: There are none! We suggest using `TPL` and `TPLck` functions in the [taxonstand package](https://cran.rstudio.com/package=Taxonstand). We provide two functions to get bullk data: `tpl_families` and `tpl_get`.

\***: There are none! The function scrapes the web directly.

#### May be in taxize in the future...

+ [NatureServe](http://www.natureserve.org/)
+ [Lichen Taxon dictionary](http://www.thebls.org.uk/)
+ [Wikispecies](https://species.wikimedia.org/wiki/Main_Page)
+ And more, See the [newdatasource](https://github.com/ropensci/taxize/labels/newdatasource) tag in the issue tracker

## Quickstart

For more examples see the [tutorial][tut]

### Installation

#### Stable version from CRAN


```r
install.packages("taxize")
```

#### Development version from GitHub

Windows users install [Rtools](http://cran.r-project.org/bin/windows/Rtools/) first.


```r
install.packages("devtools")
devtools::install_github("taxize", "ropensci")
```


```r
library('taxize')
```

### Get unique taxonomic identifier from NCBI

Alot of `taxize` revolves around taxonomic identifiers. Because, as you know, names can be a mess (misspelled, synonyms, etc.), it's better to get an identifier that a particular data sources knows about, then we can move forth acquiring more fun taxonomic data.


```r
uids <- get_uid(c("Chironomus riparius", "Chaetopteryx"))
```

### Retrieve classifications

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

### Immediate children

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

### Downstream children to a rank

Get all species in the genus _Apis_


```r
downstream("Apis", db = 'itis', downto = 'Species', verbose = FALSE)
#> $Apis
#>      tsn parentname parenttsn          taxonname rankid rankname
#> 1 154396       Apis    154395     Apis mellifera    220  Species
#> 2 763550       Apis    154395 Apis andreniformis    220  Species
#> 3 763551       Apis    154395        Apis cerana    220  Species
#> 4 763552       Apis    154395       Apis dorsata    220  Species
#> 5 763553       Apis    154395        Apis florea    220  Species
#> 6 763554       Apis    154395 Apis koschevnikovi    220  Species
#> 7 763555       Apis    154395   Apis nigrocincta    220  Species
#> 
#> attr(,"class")
#> [1] "downstream"
#> attr(,"db")
#> [1] "itis"
```

### Upstream taxa

Get all genera up from the species _Pinus contorta_ (this includes the genus of the species, and its co-genera within the same family).


```r
upstream("Pinus contorta", db = 'itis', upto = 'Genus', verbose=FALSE)
#> $`Pinus contorta`
#>      tsn parentname parenttsn   taxonname rankid rankname
#> 1  18031   Pinaceae     18030       Abies    180    Genus
#> 2  18033   Pinaceae     18030       Picea    180    Genus
#> 3  18035   Pinaceae     18030       Pinus    180    Genus
#> 4 183396   Pinaceae     18030       Tsuga    180    Genus
#> 5 183405   Pinaceae     18030      Cedrus    180    Genus
#> 6 183409   Pinaceae     18030       Larix    180    Genus
#> 7 183418   Pinaceae     18030 Pseudotsuga    180    Genus
#> 8 822529   Pinaceae     18030  Keteleeria    180    Genus
#> 9 822530   Pinaceae     18030 Pseudolarix    180    Genus
#> 
#> attr(,"class")
#> [1] "upstream"
#> attr(,"db")
#> [1] "itis"
```

### Get synonyms


```r
synonyms("Salmo friderici", db='ubio')
#>    ubioid          target family    rank
#> 1 2529704 Salmo friderici Pisces species
#> 2  169693 Salmo friderici Pisces species
#> $`Salmo friderici`
#>   namebankid                    namestring
#> 1     130562 Leporinus friderici friderici
#> 2     169693               Salmo friderici
#> 3    2495407 Leporinus friderici friderici
#>                                fullnamestring
#> 1 Leporinus friderici friderici (Bloch, 1794)
#> 2                 Salmo friderici Bloch, 1794
#> 3               Leporinus friderici friderici
```

### Get taxonomic IDs from many sources


```r
get_ids(names="Salvelinus fontinalis", db = c('ubio','ncbi'), verbose=FALSE)
#>    ubioid                target     family      rank
#> 1 2501330 Salvelinus fontinalis     Pisces   species
#> 2 6581534 Salvelinus fontinalis Salmonidae   species
#> 3  137827 Salvelinus fontinalis     Pisces   species
#> 4 6244425 Salvelinus fontinalis Salmonidae trinomial
#> 5 7130714 Salvelinus fontinalis Salmonidae trinomial
#> 6 6653671 Salvelinus fontinalis Salmonidae trinomial
#> $ubio
#> Salvelinus fontinalis 
#>             "2501330" 
#> attr(,"class")
#> [1] "ubioid"
#> attr(,"match")
#> [1] "found"
#> attr(,"uri")
#> [1] "http://www.ubio.org/browser/details.php?namebankID=2501330"
#> 
#> $ncbi
#> Salvelinus fontinalis 
#>                "8038" 
#> attr(,"class")
#> [1] "uid"
#> attr(,"match")
#> [1] "found"
#> attr(,"uri")
#> [1] "http://www.ncbi.nlm.nih.gov/taxonomy/8038"
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
#>   ptaxonVersionKey    searchMatchTitle    rank  nameStatus
#> 1 NBNSYS0000027573 Chironomus riparius Species Recommended
#> 2 NBNSYS0000023345   Paederus riparius Species Recommended
#> 3 NHMSYS0001718042   Elaphrus riparius Species Recommended
#> 
#> $nbn$`Pinus contorta`
#>   ptaxonVersionKey               searchMatchTitle       rank  nameStatus
#> 1 NHMSYS0000494848   Pinus contorta var. contorta    Variety Recommended
#> 2 NBNSYS0000004786                 Pinus contorta    Species Recommended
#> 3 NHMSYS0000494848 Pinus contorta subsp. contorta Subspecies Recommended
#> 
#> 
#> attr(,"class")
#> [1] "ids"
```

### Common names from scientific names


```r
sci2comm('Helianthus annuus', db = 'itis')
#> $`Helianthus annuus`
#> [1] "common sunflower" "sunflower"        "wild sunflower"  
#> [4] "annual sunflower"
```

### Scientific names from common names


```r
comm2sci("black bear", db = "itis")
#> $`black bear`
#> [1] "Ursus americanus luteolus"   "Ursus americanus americanus"
#> [3] "Ursus americanus"            "Ursus americanus"           
#> [5] "Chiropotes satanas"          "Ursus thibetanus"           
#> [7] "Ursus thibetanus"
```

### Coerce codes to taxonomic id classes

`numeric` to `uid`


```r
as.uid(315567)
#> [1] "315567"
#> attr(,"class")
#> [1] "uid"
#> attr(,"match")
#> [1] "found"
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
#> attr(,"uri")
#> [1] "http://www.ncbi.nlm.nih.gov/taxonomy/315567"
#> [2] "http://www.ncbi.nlm.nih.gov/taxonomy/3339"  
#> [3] "http://www.ncbi.nlm.nih.gov/taxonomy/9696"
```

### Coerce taxonomic id classes to a data.frame


```r
out <- as.uid(c(315567, 3339, 9696))
(res <- data.frame(out))
#>      ids class match                                         uri
#> 1 315567   uid found http://www.ncbi.nlm.nih.gov/taxonomy/315567
#> 2   3339   uid found   http://www.ncbi.nlm.nih.gov/taxonomy/3339
#> 3   9696   uid found   http://www.ncbi.nlm.nih.gov/taxonomy/9696
```

## Contributors

+ [Scott Chamberlain](https://github.com/sckott)
+ [Eduard Szöcs](https://github.com/EDiLD)
+ [Zachary Foster](https://github.com/zachary-foster)
+ [Carl Boettiger](https://github.com/cboettig)
+ [Karthik Ram](https://github.com/karthik)
+ [Ignasi Bartomeus](https://github.com/ibartomeus)
+ [John Baumgartner](https://github.com/johnbaums)

## Road map

Check out our [milestones](https://github.com/ropensci/taxize/milestones) to see what we plan to get done for each version.

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/taxize/issues).
* License: MIT
* Get citation information for `taxize` in R doing `citation(package = 'taxize')`

[![ropensci](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)

[tut]: http://ropensci.org/tutorials/taxize.html
