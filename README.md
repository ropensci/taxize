taxize
=======

[![Build Status](https://api.travis-ci.org/ropensci/taxize.png?branch=master)](https://travis-ci.org/ropensci/taxize)
[![Build status](https://ci.appveyor.com/api/projects/status/6mgc02mkd8j4sq3g/branch/master)](https://ci.appveyor.com/project/sckott/taxize-175/branch/master)

`taxize` allows users to search over many taxonomic data sources for species names (scientific and common) and download up and downstream taxonomic hierarchical information - among other things.

The `taxize` tutorial is can be found at [http://ropensci.org/tutorials/taxize_tutorial.html](http://ropensci.org/tutorials/taxize_tutorial.html)

### Contributors

+ [Scott Chamberlain](https://github.com/SChamberlain)
+ [Eduard Szöcs](https://github.com/EDiLD)
+ [Zachary Foster](https://github.com/zachary-foster)
+ [Carl Boettiger](https://github.com/cboettig)
+ [Karthik Ram](https://github.com/karthik)
+ [Ignasi Bartomeus](https://github.com/ibartomeus)
+ [John Baumgartner](https://github.com/johnbaums)

The functions in the package that hit a specific API have a prefix and suffix separated by an underscore. They follow the format of `service_whatitdoes`.  For example, `gnr_resolve` uses the Global Names Resolver API to resolve species names.  General functions in the package that don't hit a specific API don't have two words separated by an underscore, e.g., `classification`.

You need API keys for Encyclopedia of Life (EOL), the Universal Biological Indexer and Organizer (uBio), Tropicos, and Plantminer.

The following are URL's for API documentation, where to get API keys, and what prefix they have in function names.

#### SOAP

Note that a few data sources require SOAP web services, which are difficult to support. Thus, data sources that require SOAP web services are included in a full version of the package, but are only available installing from GitHub (see installation notes below). All the remaining data sources are available on the `master` branch and on CRAN. See the column _soap branch only_ in the table below.

### Currently implemented in `taxize`

<table>
<colgroup>
<col style="text-align:left;"/>
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
	<th style="text-align:left;"><em>soap branch only</em></th>
</tr>
</thead>

<tbody>
<tr>
	<td style="text-align:left;">Encylopedia of Life</td>
	<td style="text-align:left;"><code>eol</code></td>
	<td style="text-align:left;"><a href="http://www.eol.org/api/">link</a></td>
	<td style="text-align:left;"><a href="http://eol.org/users/register">link</a></td>
	<td style="text-align:left;">false</td>
</tr>
<tr>
	<td style="text-align:left;">Taxonomic Name Resolution Service</td>
	<td style="text-align:left;"><code>tnrs</code></td>
	<td style="text-align:left;"><a href="http://api.phylotastic.org/tnrs">link</a></td>
	<td style="text-align:left;">none</td>
	<td style="text-align:left;">false</td>
</tr>
<tr>
	<td style="text-align:left;">Integrated Taxonomic Information Service</td>
	<td style="text-align:left;"><code>itis</code></td>
	<td style="text-align:left;"><a href="http://www.itis.gov/ws_description.html">link</a></td>
	<td style="text-align:left;">none</td>
	<td style="text-align:left;">false</td>
</tr>
<tr>
	<td style="text-align:left;">Phylomatic</td>
	<td style="text-align:left;"><code>phylomatic</code></td>
	<td style="text-align:left;"><a href="http://www.phylodiversity.net/phylomatic/phylomatic_api.html">link</a></td>
	<td style="text-align:left;">none</td>
	<td style="text-align:left;">false</td>
</tr>
<tr>
	<td style="text-align:left;">uBio</td>
	<td style="text-align:left;"><code>ubio</code></td>
	<td style="text-align:left;"><a href="http://www.ubio.org/index.php?pagename=xml_services">link</a></td>
	<td style="text-align:left;"><a href="http://www.ubio.org/index.php?pagename=form">link</a></td>
	<td style="text-align:left;">false</td>
</tr>
<tr>
	<td style="text-align:left;">Global Names Resolver</td>
	<td style="text-align:left;"><code>gnr</code></td>
	<td style="text-align:left;"><a href="http://resolver.globalnames.org/api">link</a></td>
	<td style="text-align:left;">none</td>
	<td style="text-align:left;">false</td>
</tr>
<tr>
	<td style="text-align:left;">Global Names Index</td>
	<td style="text-align:left;"><code>gni</code></td>
	<td style="text-align:left;"><a href="https://github.com/dimus/gni/wiki/api">link</a></td>
	<td style="text-align:left;">none</td>
	<td style="text-align:left;">false</td>
</tr>
<tr>
	<td style="text-align:left;">IUCN Red List</td>
	<td style="text-align:left;"><code>iucn</code></td>
	<td style="text-align:left;"><a href="https://www.assembla.com/spaces/sis/wiki/Red_List_API?version=3">link</a></td>
	<td style="text-align:left;">none</td>
	<td style="text-align:left;">false</td>
</tr>
<tr>
	<td style="text-align:left;">Tropicos</td>
	<td style="text-align:left;"><code>tp</code></td>
	<td style="text-align:left;"><a href="http://services.tropicos.org/help">link</a></td>
	<td style="text-align:left;"><a href="http://services.tropicos.org/help?requestkey">link</a></td>
	<td style="text-align:left;">false</td>
</tr>
<tr>
	<td style="text-align:left;">Plantminer</td>
	<td style="text-align:left;"><code>plantminer</code></td>
	<td style="text-align:left;"><a href="http://www.plantminer.com/help">link</a></td>
	<td style="text-align:left;"><a href="http://www.plantminer.com/help">link</a></td>
	<td style="text-align:left;">false</td>
</tr>
<tr>
	<td style="text-align:left;">Theplantlist dot org</td>
	<td style="text-align:left;"><code>tpl</code></td>
	<td style="text-align:left;">**</td>
	<td style="text-align:left;">none</td>
	<td style="text-align:left;">false</td>
</tr>
<tr>
	<td style="text-align:left;">Catalogue of Life</td>
	<td style="text-align:left;"><code>col</code></td>
	<td style="text-align:left;"><a href="http://www.catalogueoflife.org/colwebsite/content/web-services">link</a></td>
	<td style="text-align:left;">none</td>
	<td style="text-align:left;">false</td>
</tr>
<tr>
	<td style="text-align:left;">Global Invasive Species Database</td>
	<td style="text-align:left;"><code>gisd</code></td>
	<td style="text-align:left;">***</td>
	<td style="text-align:left;">none</td>
	<td style="text-align:left;">false</td>
</tr>
<tr>
	<td style="text-align:left;">National Center for Biotechnology Information</td>
	<td style="text-align:left;"><code>ncbi</code></td>
	<td style="text-align:left;">none</td>
	<td style="text-align:left;">none</td>
	<td style="text-align:left;">false</td>
</tr>
<tr>
	<td style="text-align:left;">CANADENSYS Vascan name search API</td>
	<td style="text-align:left;"><code>vascan</code></td>
	<td style="text-align:left;"><a href="http://data.canadensys.net/vascan/api">link</a></td>
	<td style="text-align:left;">none</td>
	<td style="text-align:left;">false</td>
</tr>
<tr>
	<td style="text-align:left;">International Plant Names Index (IPNI)</td>
	<td style="text-align:left;"><code>ipni</code></td>
	<td style="text-align:left;"><a href="http://www.ipni.org/link_to_ipni.html">link</a></td>
	<td style="text-align:left;">none</td>
	<td style="text-align:left;">false</td>
</tr>
<tr>
	<td style="text-align:left;">World Register of Marine Species (WoRMS)</td>
	<td style="text-align:left;"><code>worms</code></td>
	<td style="text-align:left;"><a href="http://www.marinespecies.org/aphia.php?p=webservice">link</a></td>
	<td style="text-align:left;">none</td>
	<td style="text-align:left;"><strong>TRUE</strong></td>
</tr>
<tr>
	<td style="text-align:left;">Barcode of Life Data Systems (BOLD)</td>
	<td style="text-align:left;"><code>bold</code></td>
	<td style="text-align:left;"><a href="http://www.boldsystems.org/index.php/Resources">link</a></td>
	<td style="text-align:left;">none</td>
	<td style="text-align:left;">false</td>
</tr>
<tr>
	<td style="text-align:left;">Pan-European Species directories Infrastructure (PESI)</td>
	<td style="text-align:left;"><code>pesi</code></td>
	<td style="text-align:left;"><a href="http://www.eu-nomen.eu/portal/webservices.php">link</a></td>
	<td style="text-align:left;">none</td>
	<td style="text-align:left;"><strong>TRUE</strong></td>
</tr>
<tr>
	<td style="text-align:left;">Mycobank</td>
	<td style="text-align:left;"><code>myco</code></td>
	<td style="text-align:left;"><a href="http://www.mycobank.org/Services/Generic/Help.aspx?s=searchservice">link</a></td>
	<td style="text-align:left;">none</td>
	<td style="text-align:left;"><strong>TRUE</strong></td>
</tr>
<tr>
	<td style="text-align:left;">National Biodiversity Network (UK)</td>
	<td style="text-align:left;"><code>nbn</code></td>
	<td style="text-align:left;"><a href="https://data.nbn.org.uk/Documentation/Web_Services/Web_Services-REST/resources/restapi/rest.html">link</a></td>
	<td style="text-align:left;">none</td>
	<td style="text-align:left;">false</td>
</tr>
</tbody>
</table>

**: There are none! We suggest using `TPL` and `TPLck` functions in the [taxonstand package](http://cran.r-project.org/web/packages/Taxonstand/index.html). We provide two functions to get bullk data: `tpl_families` and `tpl_get`.

\***: There are none! The function scrapes the web directly.

#### May be in taxize in the future...

+ [NatureServe](http://www.natureserve.org/)
+ [Lichen Taxon dictionary](http://www.thebls.org.uk/)
+ [Wikispecies]()
+ And more, See the [newdatasource](https://github.com/ropensci/taxize/labels/newdatasource) tag in the issue tracker

## Quickstart

For more examples [click here](http://ropensci.org/tutorials/taxize_tutorial.html)

### Install taxize

#### Stable version from CRAN:

```r
install.packages("taxize")
library('taxize')
```

#### Development version from GitHub:

You'll need the `devtools` package, installs from the `master` branch

```r
install.packages("devtools")
devtools::install_github("taxize", "ropensci")
library('taxize')
```

#### Version with SOAP data sources

You'll need the `devtools` package, and the `XMLSchema` and `SSOAP` packages. The canonical source of `XMLSchema` and `SSOAP` is [here](http://www.omegahat.org/Prerelease/), but they were cloned to this other Github location (`github.com/sckott`) to ease installation.

```r
install.packages("devtools")
devtools::install_github(c("sckott/XMLSchema", "sckott/SSOAP"))
```

Install from the `soap` branch

```r
devtools::install_github("taxize", "ropensci", ref='soap')
library('taxize')
```

### Get unique taxonomic identifier from NCBI

```r
uids <- get_uid(c("Chironomus riparius", "Chaetopteryx"))
```

### Retrieve classifications

```r
out <- classification(uids)
lapply(out, head)
```

```r
[[1]]
                name         rank
1 cellular organisms      no rank
2          Eukaryota superkingdom
3       Opisthokonta      no rank
4            Metazoa      kingdom
5          Eumetazoa      no rank
6          Bilateria      no rank

[[2]]
                name         rank
1 cellular organisms      no rank
2          Eukaryota superkingdom
3       Opisthokonta      no rank
4            Metazoa      kingdom
5          Eumetazoa      no rank
6          Bilateria      no rank
```

### Get synonyms

```r
synonyms("Poa annua", db="itis")
```

```r
$`Poa annua`
                          name    tsn
1      Poa annua var. aquatica 538978
2       Poa annua var. reptans 538979
3                  Aira pumila 785854
4             Catabrosa pumila 787993
5               Ochlopoa annua 791574
6               Poa aestivalis 793946
7                   Poa algida 793954
8         Poa annua var. annua 802116
9     Poa annua var. eriolepis 802117
10 Poa annua var. rigidiuscula 802119
11        Poa annua f. reptans 803667
```

### Get taxonomic IDs from many sources

```r
get_ids(names="Chironomus riparius", db = c('ncbi','itis','col'), verbose=FALSE)
```

```r
$ncbi
Chironomus riparius
					"315576"
attr(,"match")
[1] "found"
attr(,"uri")
[1] "http://www.ncbi.nlm.nih.gov/taxonomy/315576"
attr(,"class")
[1] "uid"

$itis
Chironomus riparius
					"129313"
attr(,"match")
[1] "found"
attr(,"uri")
[1] "http://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=129313"
attr(,"class")
[1] "tsn"

$col
Chironomus riparius
					"8663146"
attr(,"class")
[1] "colid"
attr(,"uri")
[1] "http://www.catalogueoflife.org/col/details/species/id/8663146"

attr(,"class")
[1] "ids"
```


### Get common names from scientific names

```r
sci2comm(scinames='Helianthus annuus', db='itis')
```

```r
$`Helianthus annuus`
[1] "common sunflower" "sunflower"        "wild sunflower"   "annual sunflower"
```


<!-- ### Make a phylogeny from Phylomatic

#### Input the taxonomic names

```r
taxa <- c("Poa annua", "Phlox diffusa", "Helianthus annuus")
```

#### Fetch the tree - the formatting of names and higher taxonmy is done within the function

```r
tree <- phylomatic_tree(taxa=taxa, storedtree = "R20120829")
```

#### Plot

```r
plot(tree)
```

![phylogeny](http://f.cl.ly/items/0o253B453R3I0D20082E/Screen%20Shot%202013-12-27%20at%209.03.49%20AM.png) -->

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/taxize/issues).
* License: MIT
* This package is part of the [rOpenSci](http://ropensci.org/packages) project.
* Get citation information for `taxize` in R doing `citation(package = 'taxize')`

[![ropensci](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
