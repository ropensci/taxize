# examples.R
# require(taxize)

######## Examples: 
# Search by term and search type
get_itis_xml("Quercus_douglasii", "sciname", "name")
get_itis_xml("dolphin", "anymatch", "name")
get_itis_xml("inch", "comnamebeg", "name")
get_itis_xml("ferret-badger", "comname", "name")
get_itis_xml("grizzly%20bear", "comnameend", "name")
get_itis_xml("bear", "terms", "name")
get_itis_xml("buya", "itistermscomname", "name")
get_itis_xml("ursidae", "itistermssciname", "name")
get_itis_xml("french", "tsnsvernacular", "name")
itisxml <- get_itis_xml("36616", "tsnfullhir", "tsn", "name")

# Get TSN code 
get_tsn("Quercus_douglasii", "sciname", by_="name")

# Print full hierarchy (calls get_itis_xml, then prints hierarchy for view)
printhier("Quercus_douglasii")

# Convert xml to other formats
pagelist <- xmlToList(itisxml)
pagelist[1] # first part of the list



# Get family name 
get_familyname("183327") # single taxon
laply(list("36616", "19322", "183327"), get_familyname, .progress="text") # multiple taxa



# Get formatted string to submit to Phylomatic
get_phymat_format("183327", 'isubmit') # single taxon
dat_ <- laply(list("36616", "19322", "183327"), get_phymat_format, format='rsubmit', .progress="text") # multiple taxa
dat_mine <- paste(dat_, collapse="%0D%0A") # collapse and replace \n's



# Fetch a phylogenetic tree from Phylomatic
phyformat <- "annonaceae/annona/annona_cherimola
annonaceae/annona/annona_muricata"

  # Using tree strings that need to be formatted within the function
get_phylomatic_tree(phyformat, 'TRUE', 'GET', 'xml', 'TRUE')
get_phylomatic_tree(phyformat, 'TRUE', 'GET', 'new', 'FALSE')
get_phylomatic_tree(phyformat, 'TRUE', 'POST', 'xml', 'TRUE')

tree <- get_phylomatic_tree(phyformat, 'TRUE', 'POST', 'new', 'FALSE')
treephylo <- read.tree(text = tree)
plot(treephylo)

  # Using a tree string already formatted for sumission to Phylomatic (convert = 'FALSE')
tree <- get_phylomatic_tree(dat_mine, 'FALSE', 'GET', 'new', 'TRUE')
plot(tree)



# Possible workflow
  # User's species list 
splist <- c("annona_cherimola", 'annona_muricata', "quercus_robur", 
  "shorea_robusta", "pandanus_patina", "oryza_sativa", "durio_zibethinus",
  "rubus_ulmifolius", "asclepias_curassavica", "pistacia_lentiscus")

  # Get TSN code for each species
tsns <- laply(splist, get_tsn, searchtype = "sciname", by_ = "name", 
  .progress = "text")

  # Get Phylomatic formatted strings for each species
dat_ <- laply(tsns, get_phymat_format, format='rsubmit', .progress="text")

  # Send strings to Phylomatic and return newick tree
tree <- get_phylomatic_tree(dat_, 'TRUE', 'GET', 'new', 'TRUE')
  
  # Plot tree, etc. 
plot(tree)
