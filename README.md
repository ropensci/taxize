taxize
=======

[![Build Status](https://api.travis-ci.org/ropensci/taxize.png)](https://travis-ci.org/ropensci/taxize)

`taxize` allows users to search over many taxonomic data sources for species names (scientific and common) and download up and downstream taxonomic hierarchical information - among other things. 

The `taxize` tutorial is [here](http://ropensci.org/tutorials/taxizetutorial.html)

### Contributors

+ [Scott Chamberlain](https://github.com/SChamberlain)
+ [Eduard Szöcs](https://github.com/EDiLD)
+ [Carl Boettiger](https://github.com/cboettig)
+ [Karthik Ram](https://github.com/karthik)
+ [Ignasi Bartomeus](https://github.com/ibartomeus)
+ [John Baumgartner](https://github.com/johnbaums)

The functions in the package that hit a specific API have a prefix and suffix separated by an underscore. They follow the format of `service_whatitdoes`.  For example, `gnr_resolve` uses the Global Names Resolver API to resolve species names.  General functions in the package that don't hit a specific API don't have two words separated by an underscore, e.g., `classification`.

You need API keys for Encyclopedia of Life (EOL), the Universal Biological Indexer and Organizer (uBio), Tropicos, and Plantminer.

The following are URL's for API documentation, where to get API keys, and what prefix they have in function names. 

### Currently implemented in `taxize`

|Souce|Function prefix| API Docs|API key|
|---|---|---|---|
|Encylopedia of Life|`eol`|[here](http://www.eol.org/api/)|[here](http://eol.org/users/register)|
|Taxonomic Name Resolution Service|`tnrs`|[here](http://api.phylotastic.org/tnrs)|none|
|Integrated Taxonomic Information Service|`itis`|[here](http://www.itis.gov/ws_description.html)|none|
|Phylomatic|`phylomatic`|[here](http://www.phylodiversity.net/phylomatic/phylomatic_api.html)|none|
|uBio|`ubio`|[here](http://www.ubio.org/index.php?pagename=xml_services)|[here](http://www.ubio.org/index.php?pagename=form)|
|Global Names Resolver|`gnr`|[here](http://resolver.globalnames.org/api)|none|
|Global Names Index|`gni`|[here](https://github.com/dimus/gni/wiki/api)|none|
|IUCN Red List|`iucn`|[here](https://www.assembla.com/spaces/sis/wiki/Red_List_API?version=3)|none|
|Tropicos|`tp`|[here](http://services.tropicos.org/help)|[here](http://services.tropicos.org/help?requestkey)|
|Plantminer|`plantminer`|[here](http://www.plantminer.com/help)|[here](http://www.plantminer.com/help)|
|Theplantlist dot org|`tpl`|\*\*|none|
|Catalogue of Life|`col`|[here](http://www.catalogueoflife.org/colwebsite/content/web-services)|none|
|Global Invasive Species Database|`gisd`|\***|none|

**: There are none! We suggest using `TPL` and `TPLck` functions in the [taxonstand package](http://cran.r-project.org/web/packages/Taxonstand/index.html). We provide two functions to get bullk data: `tpl_families` and `tpl_get`.

\***: There are none! The function scrapes the web directly.

<!-- ### Currently implemented in `taxize`
+ Encyclopedia of Life (EOL)
	+ [API docs](http://www.eol.org/api/)
	+ [Get an API key: start an account on EOL to get your API key](http://eol.org/users/register)
	+ [API forum](https://eol.uservoice.com/forums/15429-encyclopedia-of-life-api)
	+ function prefix: `eol`
+ Taxonomic Name Resolution Service (TNRS) 
	+ [API docs](http://api.phylotastic.org/tnrs)
	+ function prefix: `tnrs`
+ Integrated Taxonomic Information Service (ITIS)
	+ [API docs](http://www.itis.gov/ws_description.html)
	+ function prefix: `itis`
+ Phylomatic 
	+ [API docs](http://www.phylodiversity.net/phylomatic/phylomatic_api.html)
	+ function prefix: `phylomatic`
+ uBio
	+ [API docs](http://www.ubio.org/index.php?pagename=xml_services)
	+ [Get an API key](http://www.ubio.org/index.php?pagename=form)
	+ function prefix: `ubio`
+ Global Names Resolver (from EOL/GBIF)
	+ [Use](http://resolver.globalnames.org/)
	+ [API docs](http://resolver.globalnames.org/api)
	+ function prefix: `gnr`
+ Global Names Index (from EOL/GBIF)
	+ [Use](http://gni.globalnames.org/)
	+ [API docs](https://github.com/dimus/gni/wiki/api)
	+ function prefix: `gni`
+ IUCN Red List 
  	+ [API docs](https://www.assembla.com/spaces/sis/wiki/Red_List_API?version=3)
  	+ function prefix: `iucn`
+ Tropicos (from Missouri Botanical Garden)
	+ [API docs](http://services.tropicos.org/help)
	+ [Get an API key](http://services.tropicos.org/help?requestkey)
	+ function prefix: `tp`
+ Plantminer
	+ [Their website](http://www.plantminer.com/)
 	+ [API docs](http://www.plantminer.com/help)
 	+ function prefix: `plantminer`
+ Theplantlist dot org
	+ [Their website](http://www.theplantlist.org/)
 	+ API docs: There are none! We wrap functions in the [taxonstand package](http://cran.r-project.org/web/packages/Taxonstand/index.html)
 	+ function prefix: `tpl`
+ Catalogue of Life
 	+ [API docs](http://www.catalogueoflife.org/colwebsite/content/web-services)
 	+ function prefix: `col`
+ Global Invasive Species Database
  + [Their website](http://www.issg.org/database/welcome/)
 	+ API docs: There are none! The function scraps the web directly.
 	+ function prefix: `gisd` -->
  
#### May be in taxize in the future...

+ Tree of Life web project
	+ [Their website](http://tolweb.org/tree/phylogeny.html)
 	+ [API docs](http://tolweb.org/tree/home.pages/downloadtree.html)
 	+ to be function prefix: `tol`
+ Freshwaterecology - The Taxa and Autecology Database for Freshwater Organisms
	+ [Their website](http://www.freshwaterecology.info)
	+ API docs: There are none! The function scraps the web directly.
	+ to be function prefix: `fresh`
	+ Note: Currently only the macro-invertebrate database is supported!
+ [USDA Plants](http://plants.usda.gov/java/)
+ [NatureServe](http://www.natureserve.org/)
+ [Lichen Taxon dictionary](http://www.thebls.org.uk/)
+ [MycoBank](http://www.mycobank.org/)

### Install `taxize` 

+ Stable version from CRAN:

```coffee
install.packages("taxize")
library(taxize)
```

+ Or, development version from GitHub:

```coffee
install.packages("devtools")
library(devtools)
install_github("taxize", "ropensci")
library(taxize)
```

### A few examples (for more [click here](http://ropensci.org/tutorials/taxizetutorial.html))

#### Get unique taxonomic identifier from NCBI

```coffee
uids <- get_uid(c("Chironomus riparius", "Chaetopteryx"))

Retrieving data for species ' Chironomus riparius '

Retrieving data for species ' Chaetopteryx '
```

#### And retrieve classification

```coffee
out <- classification(uids)
lapply(out, head)
```

```coffee
[[1]]
              ScientificName         Rank     UID
1         cellular organisms      no rank  131567
2                  Eukaryota superkingdom    2759
3               Opisthokonta      no rank   33154
4                    Metazoa      kingdom   33208
5                  Eumetazoa      no rank    6072
6                  Bilateria      no rank   33213

[[2]]
       ScientificName         Rank     UID
1  cellular organisms      no rank  131567
2           Eukaryota superkingdom    2759
3        Opisthokonta      no rank   33154
4             Metazoa      kingdom   33208
5           Eumetazoa      no rank    6072
6           Bilateria      no rank   33213
```

### Make a phylogeny from Phylomatic

#### Input the taxonomic names

```coffee
taxa <- c("Poa annua", "Abies procera", "Helianthus annuus")
```

#### Fetch the tree - the formatting of names and higher taxonmy is done within the function

```coffee
tree <- phylomatic_tree(taxa=taxa, get = 'POST', informat='newick', method = "phylomatic", storedtree = "R20120829", taxaformat = "slashpath", outformat = "newick", clean = "true")
```

#### Plot

```coffee
plot(tree)
```

![](http://ropensci.github.com/taxize/phylomatic_phylo.png)



[![](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)