taxize
=======

[![Build Status](https://api.travis-ci.org/ropensci/taxize.png)](https://travis-ci.org/ropensci/taxize)

`taxize` allows users to search over many taxonomic data sources for species names (scientific and common) and download up and downstream taxonomic hierarchical information - among other things. 

The `taxize` tutorial is can be found at [http://ropensci.org/tutorials/taxize_tutorial.html](http://ropensci.org/tutorials/taxize_tutorial.html)

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
|Encylopedia of Life|`eol`|[link](http://www.eol.org/api/)|[link](http://eol.org/users/register)|
|Taxonomic Name Resolution Service|`tnrs`|[link](http://api.phylotastic.org/tnrs)|none|
|Integrated Taxonomic Information Service|`itis`|[link](http://www.itis.gov/ws_description.html)|none|
|Phylomatic|`phylomatic`|[link](http://www.phylodiversity.net/phylomatic/phylomatic_api.html)|none|
|uBio|`ubio`|[link](http://www.ubio.org/index.php?pagename=xml_services)|[link](http://www.ubio.org/index.php?pagename=form)|
|Global Names Resolver|`gnr`|[link](http://resolver.globalnames.org/api)|none|
|Global Names Index|`gni`|[link](https://github.com/dimus/gni/wiki/api)|none|
|IUCN Red List|`iucn`|[link](https://www.assembla.com/spaces/sis/wiki/Red_List_API?version=3)|none|
|Tropicos|`tp`|[link](http://services.tropicos.org/help)|[link](http://services.tropicos.org/help?requestkey)|
|Plantminer|`plantminer`|[link](http://www.plantminer.com/help)|[link](http://www.plantminer.com/help)|
|Theplantlist dot org|`tpl`|\*\*|none|
|Catalogue of Life|`col`|[link](http://www.catalogueoflife.org/colwebsite/content/web-services)|none|
|Global Invasive Species Database|`gisd`|\*\*\*|none|
|National Center for Biotechnology Information|`ncbi`|none|none|
|CANADENSYS Vascan name search API|`vascan`|[link](http://data.canadensys.net/vascan/api)|none|

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

#### Notes on the ITIS API

There are a lot of methods for the ITIS API. We have attempted to simplify the interface. Here are some notes:

The following are higher level functions that attempt to make interacting with the various methods easier. Some are new. The function on the left uses the functions on the right of the arrow. You can access functions on the right of the arrow as normal, but aren't shown in the main help file index to avoid cognitive load.

+ `classification` <- `getfullhierarchyfromtsn`
+ `itis_downstream` <- `gethierarchydownfromtsn`
+ `itis_searchcommon` <- `searchbycommonnamebeginswith`, `searchbycommonnameendswith`
+ `get_tsn` <- `searchbycommonnamebeginswith`, `searchbycommonnameendswith`, `searchbycommonname`, `searchbyscientificname`
+ `comm2sci` <- `searchbycommonnamebeginswith`, `searchbycommonnameendswith`, `searchbycommonname`, `getscientificnamefromtsn`
+ `sci2comm` <- `searchbycommonnamebeginswith`, `searchbycommonnameendswith`, `searchbycommonname`, `getcommonnamesfromtsn`, `searchbyscientificname`
+ `synonyms` <- `getsynonymnamesfromtsn`
+ `itis_acceptname` <- `getacceptednamesfromtsn`
+ `itis_taxrank` <- `gettaxonomicranknamefromtsn`, `getranknames`
+ `itis_ping` <- `getdescription`
+ `itis_refs` <- `getpublicationsfromtsn`
+ `itis_getrecord` <- `getfullrecordfromtsn`
+ `itis_kingdomnames` <- `getkingdomnamefromtsn`, `getkingdomnames`
+ `itis_lsid` <- `getfullrecordfromlsid`, `getrecordfromlsid`, `gettsnfromlsid`
+ `itis_native` <- `getjurisdictionaloriginfromtsn`, `getjurisdictionoriginvalues`, `getjurisdictionvalues`
+ `itis_hierarchy` <- `getfullhierarchyfromtsn`, `gethierarchydownfromtsn`, `gethierarchyupfromtsn`
+ `itis_terms` <- `getitisterms`, `getitistermsfromcommonname`, `getitistermsfromscientificname`

The following functions are not used in the higher level functions above, and aren't shown on the `taxize` index page, but are still available, e.g. `FunctionName`

`getanymatchcount`, `searchforanymatch`, `searchforanymatchpaged`, `getexpertsfromtsn`, `getcommentdetailfromtsn`, `getcoremetadatafromtsn`, `getcoveragefromtsn`, `getcredibilityratingfromtsn`, `getcredibilityratings`, `getcurrencyfromtsn`, `getdatedatafromtsn`, `getlastchangedate`, `getothersourcesfromtsn`, `getparenttsnfromtsn`, `getglobalspeciescompletenessfromtsn`, `getgeographicdivisionsfromtsn`, `getgeographicvalues`, `getreviewyearfromtsn`, `gettaxonauthorshipfromtsn`, `gettaxonomicusagefromtsn`, `gettsnbyvernacularlanguage`, `getunacceptabilityreasonfromtsn`, `getvernacularlanguages`

## Quickstart

### Install taxize

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

### A few examples (for more [click here](http://ropensci.org/tutorials/taxize_tutorial.html))

#### Get unique taxonomic identifier from NCBI

```coffee
uids <- get_uid(c("Chironomus riparius", "Chaetopteryx"))
```

#### And retrieve classification

```coffee
out <- classification(uids)
lapply(out, head)
```

```coffee
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

### Make a phylogeny from Phylomatic

#### Input the taxonomic names

```coffee
taxa <- c("Poa annua", "Phlox diffusa", "Helianthus annuus")
```

#### Fetch the tree - the formatting of names and higher taxonmy is done within the function

```coffee
tree <- phylomatic_tree(taxa=taxa, storedtree = "R20120829")
```

#### Plot

```coffee
plot(tree)
```

![](http://f.cl.ly/items/0o253B453R3I0D20082E/Screen%20Shot%202013-12-27%20at%209.03.49%20AM.png)

Please report any issues or bugs](https://github.com/ropensci/taxize/issues).

License: CC0

This package is part of the [rOpenSci](http://ropensci.org/packages) project.

To cite package `taxize` in publications use:

```coffee
To cite taxize in publications use:

  Scott Chamberlain and Eduard Szocs (2013). taxize - taxonomic search
  and retrieval in R. F1000Research, 2:191. URL:
  http://f1000research.com/articles/2-191/v2.

A BibTeX entry for LaTeX users is

  @Article{,
    title = {taxize - taxonomic search and retrieval in R},
    journal = {F1000Research},
    author = {{Scott Chamberlain} and {Eduard Szocs}},
    year = {2013},
    url = {http://f1000research.com/articles/2-191/v2},
  }
```

Get citation information for `taxize` in R doing `citation(package = 'taxize')`

[![](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)