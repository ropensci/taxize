# Install taxize (uncomment to run)

```r
# install_github('taxize_', 'ropensci')
library(taxize)
library(plyr)
library(ape)
```


*********

# Phylomatic
## Fetch a phylogenetic tree from Phylomatic

```r
mytree <- "asteraceae/bidens/bidens_cernua\nasteraceae/madia/madia_exigua\nasteraceae/helianthus/helianthus_annuus"
```


## Using tree strings that need to be formatted within the function
### Return a tree in the phylo object format

```r
get_phylomatic_tree(mytree, "TRUE", "GET", "xml", "TRUE")
```

```
## 
## Phylogenetic tree with 3 tips and 2 internal nodes.
## 
## Tip labels:
## [1] "bidens_cernua"     "madia_exigua"      "helianthus_annuus"
## 	Node labels:
## [1] "euphyllophyte" ""             
## 
## Rooted; includes branch lengths.
```


### Return a tree in the phylo object format, using POST method

```r
get_phylomatic_tree(mytree, "TRUE", "POST", "xml", "TRUE")
```

```
## 
## Phylogenetic tree with 3 tips and 2 internal nodes.
## 
## Tip labels:
## [1] "bidens_cernua"     "madia_exigua"      "helianthus_annuus"
## 	Node labels:
## [1] "euphyllophyte" ""             
## 
## Rooted; includes branch lengths.
```


### Return tree in newick format

```r
get_phylomatic_tree(mytree, "TRUE", "GET", "new", "FALSE")
```

```
## [1] "(bidens_cernua:3.000000,(madia_exigua:10.000000,helianthus_annuus:5.000000):3.000000)euphyllophyte:42;"
```


### Get a tree in newick format, read using ape::read.tree, and plot

```r
tree <- get_phylomatic_tree(mytree, "TRUE", "POST", "new", "FALSE")
treephylo <- read.tree(text = tree)
plot(treephylo)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


*********

# Integrated Taxonomic Informaiton Service (ITIS)
## Search by term and search type
Note: TSN stands for Taxonomic Serial Number

```r
doc <- get_itis_xml("dolphin", "comname", "name")
doc <- get_itis_xml("grizzly bear", "comnameend", "name")
doc <- get_itis_xml("ursidae", "itistermssciname", "name")
```


## Take advantage of default arguments, spaces accepted, and display data.frame output

```r
doc <- get_itis_xml("Plethodon ")
head(parse_itis(doc))
```

```
##                        sci    com    tsn
## 1         Plethodon aeneus 208328 208328
## 2     Plethodon ainsworthi 668315 668315
## 3       Plethodon albagula 208278 208278
## 4         Plethodon amplus 668316 668316
## 5 Plethodon angusticlavius 668317 668317
## 6         Plethodon asupak 685566 685566
```


## Show higher taxonomy for a given TSN

```r
classification(685566)
```

```
## Error: Must specify Identifier!
```


## Get a TSN from a sepcies name

```r
get_tsn("Quercus_douglasii", "sciname", by_ = "name")
```

```
## Error: unused argument(s) (by_ = "name")
```



*********

# Using ITIS and Phylomatic together 
## Use ITIS and get_phymat_format to format a string for each species to be submitted to Phylomatic

```r
dat_ <- laply(list("36616", "19322", "183327"), get_phymat_format, format = "rsubmit")
dat_
```

```
## [1] "asteraceae%2Fhelianthus%2Fhelianthus_annuus"
## [2] "fagaceae%2Fquercus%2Fquercus_douglasii"     
## [3] "pinaceae%2Fpinus%2Fpinus_contorta"          
```

```r
dat_mine <- paste(dat_, collapse = "%0D%0A")  # collapse and replace \n's
dat_mine
```

```
## [1] "asteraceae%2Fhelianthus%2Fhelianthus_annuus%0D%0Afagaceae%2Fquercus%2Fquercus_douglasii%0D%0Apinaceae%2Fpinus%2Fpinus_contorta"
```

```r
tree <- get_phylomatic_tree(dat_mine, "FALSE", "GET", "new", "TRUE")
tree
```

```
## 
## Phylogenetic tree with 3 tips and 2 internal nodes.
## 
## Tip labels:
## [1] "helianthus_annuus" "quercus_douglasii" "pinus_contorta"   
## 	Node labels:
## [1] "euphyllophyte" ""             
## 
## Rooted; includes branch lengths.
```

```r
plot(tree)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 


## A somewhat realistic workflow
### User's species list 

```r
splist <- c("annona_cherimola", "annona_muricata", "quercus_robur", "shorea_robusta", 
    "pandanus_patina", "oryza_sativa", "durio_zibethinus")
```


### Get TSN code for each species

```r
tsns <- laply(splist, get_tsn, searchtype = "sciname", by_ = "name")
```

```
## Error: unused argument(s) (by_ = "name")
```

```r
tsns
```

```
##  [1] 112752 113264 113844 113877 113918 113996 114289 114413 114458 114486
## [11] 114496 114654 114958 114967 152865 153658 154025 154044 154056 154248
## [21] 154344 610668 650495 650496 650497 650498 650499 657459 657460 657461
## [31] 678306 678308 709258 709259 709260 709261 709262 709263 709264 709265
## [41] 717341
## 181 Levels: 118840 121227 121286 121292 121296 121299 121304 ... 152853
```


### Get Phylomatic formatted strings for each species

```r
dat_ <- laply(tsns, get_phymat_format, format = "rsubmit")
dat_
```

```
##  [1] "epimetopidae%2Fgeoryssidae%2Fhelophoridae"         
##  [2] "agyrtidae%2Fhydraenidae%2Fleiodidae"               
##  [3] "acanthocnemidae%2Fattalomimidae%2Fchaetosomatidae" 
##  [4] "artematopodidae%2Fbrachypsectridae%2Fcantharidae"  
##  [5] "dascillidae%2Frhipiceridae%2FNA"                   
##  [6] "byrrhidae%2Fcallirhipidae%2Fchelonariidae"         
##  [7] "agapythidae%2Falexiidae%2Fbiphyllidae"             
##  [8] "aderidae%2Fanthicidae%2Farcheocrypticidae"         
##  [9] "anobiidae%2Fbostrichidae%2Fdermestidae"            
## [10] "belohinidae%2Fceratocanthidae%2Fdiphyllostomatidae"
## [11] "cerambycidae%2Fchrysomelidae%2Fmegalopodidae"      
## [12] "anthribidae%2Fattelabidae%2Fbelidae"               
## [13] "histeridae%2Fsphaeritidae%2Fsynteliidae"           
## [14] "buprestidae%2Fschizopodidae%2FNA"                  
## [15] "braconidae%2Feoichneumonidae%2Fichneumonidae"      
## [16] "agaonidae%2Faphelinidae%2Fchalcididae"             
## [17] "archaeocynipidae%2Faustrocynipidae%2Fcynipidae"    
## [18] "aulacidae%2Fevaniidae%2Fgasteruptiidae"            
## [19] "austroniidae%2Fdiapriidae%2Fheloridae"             
## [20] "bradynobaenidae%2Ffalsiformicidae%2Fformicidae"    
## [21] "ampulicidae%2Fandrenidae%2Fangarosphecidae"        
## [22] "ceraphronidae%2Fmaimetshidae%2Fmegaspilidae"       
## [23] "chorotypidae%2Fepisactidae%2Feumastacidae"         
## [24] "pneumoridae%2FNA%2FNA"                             
## [25] "acrididae%2Fcharilaidae%2Fdericorythidae"          
## [26] "tetrigidae%2FNA%2FNA"                              
## [27] "cylindrachetidae%2Fripipterygidae%2Ftridactylidae" 
## [28] "pyrgomorphidae%2FNA%2FNA"                          
## [29] "tanaoceridae%2FNA%2FNA"                            
## [30] "trigonopterygidae%2Fxyronotidae%2FNA"              
## [31] "clambidae%2Fdecliniidae%2Feucinetidae"             
## [32] "lymexylidae%2FNA%2FNA"                             
## [33] "megalyridae%2FNA%2FNA"                             
## [34] "serphitidae%2FNA%2FNA"                             
## [35] "mymarommatidae%2FNA%2FNA"                          
## [36] "stephanidae%2FNA%2FNA"                             
## [37] "platygastridae%2Fscelionidae%2FNA"                 
## [38] "trigonalidae%2FNA%2FNA"                            
## [39] "bethylonymidae%2FNA%2FNA"                          
## [40] "bethylidae%2Fchrysididae%2Fdryinidae"              
## [41] "derodontidae%2FNA%2FNA"                            
```


### Send strings to Phylomatic and return newick tree

```r
tree <- get_phylomatic_tree(dat_, "TRUE", "GET", "new", "TRUE")
tree
```

```
## NULL
```


### Plot tree, etc. 

```r
plot(tree)
```

```
## Warning: no non-missing arguments to min; returning Inf
```

```
## Warning: no non-missing arguments to max; returning -Inf
```

```
## Warning: no non-missing arguments to min; returning Inf
```

```
## Warning: no non-missing arguments to max; returning -Inf
```

```
## Error: need finite 'xlim' values
```



*********

# Taxonomic Name Resolution Service
## Name matching using the TNRS

```r
tnrsmatch("best", taxnames = c("helianthus annuus", "acacia", "saltea"), output = "names")  # just best names
```

```
## Hitting the TNRS API and matching names...
```

```
##        AcceptedName   MatchFam MatchGenus MatchScore    Accept?
## 1 Helianthus annuus Asteraceae Helianthus          1 No opinion
## 2            Acacia   Fabaceae     Acacia          1 No opinion
## 3                                 Saltera       0.93 No opinion
##      SubmittedNames
## 1 helianthus annuus
## 2            acacia
## 3            saltea
```


*********

# Global Names Resolver (GNR)
## Get just id's and names of sources in a data.frame

```r
tail(gnr_datasources(todf = T))
```

```
##     id                                title
## 82 164                            BioLib.cz
## 83 165 Tropicos - Missouri Botanical Garden
## 84 166                                nlbif
## 85 167  The International Plant Names Index
## 86 168              Index to Organism Names
## 87 169                        uBio NameBank
```

***
## Give me the id for EOL (Encyclopedia of Life)

```r
out <- gnr_datasources(todf = T)
out[out$title == "EOL", "id"]
```

```
## [1] 12
```

***
## Fuzzy search for sources with the word "zoo"

```r
out <- gnr_datasources(todf = T)
outdf <- out[agrep("zoo", out$title, ignore.case = T), ]
outdf[1:2, ]
```

```
##     id             title
## 20 100 Mushroom Observer
## 25 105           ZooKeys
```

***
## Resolve some names
### Search for _Helianthus annuus_ and _Homo sapiens_, return a data.frame

```r
gnr(names = c("Helianthus annuus", "Homo sapiens"), returndf = TRUE)[1:2, ]
```

```
##   data_source_id    submitted_name       name_string score    title
## 1              4 Helianthus annuus Helianthus annuus 0.988     NCBI
## 3             10 Helianthus annuus Helianthus annuus 0.988 Freebase
```

***
### Search for the same species, with only using data source 12 (i.e., EOL)

```r
gnr(names = c("Helianthus annuus", "Homo sapiens"), data_source_ids = "12", 
    returndf = TRUE)
```

```
##   data_source_id    submitted_name       name_string score title
## 1             12 Helianthus annuus Helianthus annuus 0.988   EOL
## 2             12      Homo sapiens      Homo sapiens 0.988   EOL
```


*********
# Tropicos: all Tropicos functions start with "tp_"
## 

```r
head(tp_getacceptednames(id = 25503923))
```

```
## http://services.tropicos.org/Name/25503923/AcceptedNames?apikey=f3e499d4-1519-42c9-afd1-685a16882f5a&format=json
```

```
##                    variable             value category
## 1                    NameId          25503923  Synonym
## 2            ScientificName       Aira pumila  Synonym
## 3 ScientificNameWithAuthors Aira pumila Pursh  Synonym
## 4                    Family           Poaceae  Synonym
## 5                    NameId          25509881 Accepted
## 6            ScientificName         Poa annua Accepted
```

```r
head(tp_getsynonyms(id = 25509881))
```

```
## http://services.tropicos.org/Name/25509881/Synonyms?apikey=f3e499d4-1519-42c9-afd1-685a16882f5a&format=json
```

```
##                    variable             value category
## 1                    NameId          25503923  Synonym
## 2            ScientificName       Aira pumila  Synonym
## 3 ScientificNameWithAuthors Aira pumila Pursh  Synonym
## 4                    Family           Poaceae  Synonym
## 5                    NameId          25509881 Accepted
## 6            ScientificName         Poa annua Accepted
```


*********
# uBio
## Search uBio using the namebank_search API method.

```r
ubio_namebank_search(searchName = "elephant", sci = 1, vern = 0)
```

```
## Warning: text_content() deprecated. Use parsed_content(x, as = 'parsed')
```

```
##   namebankID               nameString           fullNameString packageID
## 1    6938660 Q2VyeWxvbiBlbGVwaGFudA== Q2VyeWxvbiBlbGVwaGFudA==        80
##   packageName basionymUnit rankID rankName
## 1 Cerylonidae      6938660     24  species
```

```r
head(ubio_namebank_search(searchName = "Helianthus annuus", sci = 1, vern = 0)[, 
    -c(2, 3)])
```

```
## Warning: text_content() deprecated. Use parsed_content(x, as = 'parsed')
```

```
##   namebankID packageID packageName basionymUnit rankID   rankName
## 1    2658020       598   Asterales            0     24    species
## 2     462478       598   Asterales            0     24    species
## 3    9073865      5634  Asteraceae            0    444         fo
## 4    9073864      5634  Asteraceae            0    444         fo
## 5    8624412      5634  Asteraceae      8722942    444         fo
## 6    8291505      5634  Asteraceae            0    265 subspecies
```

```r
out <- lapply(list("Helianthus debilis", "Astragalus aduncus"), function(x) ubio_namebank_search(searchName = x, 
    sci = 1, vern = 0))
```

```
## Warning: text_content() deprecated. Use parsed_content(x, as = 'parsed')
```

```
## Warning: text_content() deprecated. Use parsed_content(x, as = 'parsed')
```

```r
head(out[[2]][, -c(2, 3)])  # just Astragalus aduncus output
```

```
##   namebankID packageID packageName basionymUnit rankID rankName
## 1    2843601       663     Rosales            0     24  species
## 2    9261962      5780    Fabaceae            0    261       SP
## 3    9261999      5780    Fabaceae            0    261       SP
## 4    9261975      5780    Fabaceae            0    261       SP
## 5     704148       663     Rosales            0     24  species
## 6    8374424      5780    Fabaceae       722833    444       fo
```

