# This is `taxize`

We are developing `taxize` as a package to allow users to search over many websites for species names (scientific and common) and download up and downstream taxonomic hierarchical information - and many other things. 

The `taxize` rOpenSci tutorial is [here](http://ropensci.github.com/taxize_/)

`taxize` is part of the rOpenSci project, visit [our webiste](http://ropensci.org) to learn more.

### Development by
+ [Scott Chamberlain](http://schamberlain.github.com/scott)
+ [Eduard Szöcs](https://github.com/EDiLD)

The functions in the package that hit a specific API have a prefix and suffix separated by an underscore. They follow the format of `service_whatitdoes`.  For example, `gnr_resolve` uses the Global Names Resolver API to resolve species names.  General functions in the package that don't hit a specific API don't have two words separated by an underscore, e.g., `classification`.

You need API keys for Encyclopedia of Life (EOL), the Universal Biological Indexer and Organizer (uBio), Tropicos, and Plantminer.

The following are URL's for API documentation, where to get API keys, and what prefix they have in function names. 

### Currently implemented in `taxize`
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
 	+ [API docs](http://www.catalogueoflife.org/colwebsite/content/services)
 	+ function prefix: `col`
+ Global Invasive Species Database
  + [Their website](http://www.issg.org/database/welcome/)
 	+ API docs: There are none! The function scraps the web directly.
 	+ function prefix: `gisd`
+ Freshwaterecology - The Taxa and Autecology Database for Freshwater Organisms
  + [Their website](http://www.freshwaterecology.info)
  + API docs: There are none! The function scraps the web directly.
  + function prefix: `fresh`
  + Note: Currently only the macro-invertebrate database is supported!
  

### Temporarily not implemented to resolve bugs or to complete development
+ Tree of Life web project
	+ [Their website](http://tolweb.org/tree/phylogeny.html)
 	+ [API docs](http://tolweb.org/tree/home.pages/downloadtree.html)
 	+ function prefix: `tol`

### Install `taxize` 

+ Stable version from CRAN:

```R 
install.packages("taxize")
require(taxize)
```

+ Or, development version from GitHub:

```R 
install.packages("devtools")
require(devtools)
install_github("taxize_", "ropensci")
require(taxize)
```

### Example hitting the TNRS (taxonomic names resolution service Phylotastic API):

```R 
> require(devtools)
> install_github("taxize_","ropensci")
> require(taxize)
> mynames <- c("Panthera tigris", "Eutamias minimus", "Magnifera indica", "Humbert humbert")
> tnrs(query = mynames)
Token number 76686adb91c14c706a8e8ae34b5b7dfe
Pausing 4 seconds for the query to finish...
      submittedName     acceptedName    sourceId score      matchedName   annotations                                                 uri
1  Humbert humbert        Humbertia iPlant_TNRS  0.47        Humbertia          Lam.               http://www.tropicos.org/Name/40028244
2 Magnifera indica Mangifera indica iPlant_TNRS  0.98 Mangifera indica            L.                http://www.tropicos.org/Name/1300071
3 Magnifera indica Mangifera indica        NCBI  1.00 Magnifera indica          none          http://www.ncbi.nlm.nih.gov/taxonomy/29780
4 Eutamias minimus         Euthamia iPlant_TNRS  0.46         Euthamia (Nutt.) Cass.               http://www.tropicos.org/Name/40007649
5  Panthera tigris       Megalachne iPlant_TNRS  0.48       Pantathera        Steud.               http://www.tropicos.org/Name/40015658
6  Panthera tigris  Panthera tigris        NCBI  1.00  Panthera tigris          none           http://www.ncbi.nlm.nih.gov/taxonomy/9694
7  Panthera tigris  Panthera tigris        MSW3  1.00  Panthera tigris          none http://www.bucknell.edu/msw3/browse.asp?id=14000259
```