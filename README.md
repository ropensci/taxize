# This is `taxize`

We at rOpenSci are developing `taxize` as a package which will allow users to search over many websites for species names (scientific and common) and download up and downstream taxonomic hierarchical information - and many other things. 

You need API keys for Encyclopedia of Life (EOL), the Universal Biological Indexer and Organizer (uBio), and Tropicos.

The following are URL's for API documentation and where to get API keys. 

+ Encyclopedia of Life (EOL)
	+ [API docs](http://www.eol.org/api/)
	+ [Get an API key: start an account on EOL to get your API key] (http://eol.org/users/register)
	+ [API forum] (https://eol.uservoice.com/forums/15429-encyclopedia-of-life-api)
+ Taxonomic Name Resolution Service (TNRS) 
	+ [API docs](http://tnrs.iplantcollaborative.org/api.html)
+ Integrated Taxonomic Information Service (ITIS)
	+ [API docs](http://www.itis.gov/ws_description.html)
+ Phylomatic 
	+ [API docs](http://www.phylodiversity.net/phylomatic/phylomatic_api.html)
+ uBio
	+ [API docs](http://www.ubio.org/index.php?pagename=xml_services)
	+ [Get an API key](http://www.ubio.org/index.php?pagename=form)
+ Tropicos (from Missouri Botanical Garden)
	+ [API docs](http://services.tropicos.org/help)
	+ [Get an API key](http://services.tropicos.org/help?requestkey)

`taxize` rOpenSci tutorial here:  http://ropensci.org/tutorials/r-taxize-tutorial/

`taxize` is part of the rOpenSci project, visit http://ropensci.org to learn more.

# Install

```R 
install.packages("devtools")
require(devtools)
install_github("taxize_", "ropensci")
require(taxize)
```