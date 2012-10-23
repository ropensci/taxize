# This is `taxize`

We are developing `taxize` as a package to allow users to search over many websites for species names (scientific and common) and download up and downstream taxonomic hierarchical information - and many other things. 

The functions in the package that hit a specific API have a prefix and suffix separated by an underscore. They follow the format of `service_whatitdoes`.  For example, `gnr_resolve` uses the Global Names Resolver API to resolve species names.  General functions in the package that don't hit a specific API don't have two words separated by an underscore, e.g., `classification`.

You need API keys for Encyclopedia of Life (EOL), the Universal Biological Indexer and Organizer (uBio), Tropicos, and Plantminer.

The following are URL's for API documentation, where to get API keys, and what prefix they have in function names. 

## Currently implemented in `taxize`
+ Encyclopedia of Life (EOL)
	+ [API docs](http://www.eol.org/api/)
	+ [Get an API key: start an account on EOL to get your API key](http://eol.org/users/register)
	+ [API forum](https://eol.uservoice.com/forums/15429-encyclopedia-of-life-api)
	+ function prefix: `eol`
+ Taxonomic Name Resolution Service (TNRS) 
	+ [API docs](http://tnrs.iplantcollaborative.org/api.html)
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
+ Plantminer
	+ [Their website](http://www.plantminer.com/)
 	+ [API docs](http://www.plantminer.com/help)
 	+ function prefix: `plantminer`

## Temporarily not implemented to resolve bugs or to complete development
+ Tropicos (from Missouri Botanical Garden)
	+ [API docs](http://services.tropicos.org/help)
	+ [Get an API key](http://services.tropicos.org/help?requestkey)
	+ function prefix: `tp`
+ Tree of Life web project
	+ [Their website](http://tolweb.org/tree/phylogeny.html)
 	+ [API docs](http://tolweb.org/tree/home.pages/downloadtree.html)
 	+ function prefix: `tol`

The `taxize` rOpenSci tutorial is [here](https://github.com/ropensci/taxize_/wiki/taxize-tutorial)

`taxize` is part of the rOpenSci project, visit [our webiste](http://ropensci.org) to learn more.

# Development by
+ [Scott Chamberlain](http://schamberlain.github.com/scott)
+ [Eduard Szöcs](https://github.com/EDiLD)

# Install `taxize` from GitHub:

```R 
install.packages("devtools")
require(devtools)
install_github("taxize_", "ropensci")
require(taxize)
```

Example hitting the TNRS (taxonomic names resolution service Phylotastic API):

```R 
> require(devtools)
> install_github("taxize_","ropensci")
> require(taxize)
> mynames <- c("Crepis atrabarba", "Zygadenus venenosus")
> mynames
[1] "Crepis atrabarba"    "Zygadenus venenosus"
> tnrastic(query = mynames, output = 'names')
Your request is being processed. You can retrieve the results at http://api.phylotastic.org/tnrs/retrieve/c8b544f0794e13a61b0b63ea7952f664.
Pausing a bit for the query to finish...
      AcceptedName    sourceId MatchScore       submittedName
1 Crepis atribarba iPlant TNRS       0.98    Crepis atrabarba
2                  iPlant TNRS          1 Zygadenus venenosus
```