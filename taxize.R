# Development for package taxize
# Goal: Search taxonomic information on multiple web data bases
# taxize.R
require(XML)
require(stringr)
require(RCurl)
require(plyr)

# Function to search individual strings
# Input: searchterm = any common or scientific name, 
# searchtype = one of 'sciname', 'anymatch', 'comnamebeg', 'comname', 
# 'comnameend', 'terms', 'itistermscomname', 'itistermssciname', or
# 'tsnsvernacular' 
# by_ = one of 'name' (any common or scientific name) or 'tsn' (taxonomic serial number)
# searchtsn = one of 'TRUE' or 'FALSE'
# Output: xml with taxnomic information
get_itis_xml <- function(searchterm, searchtype, by_, searchtsn='FALSE', curl=getCurlHandle()) {
  base_url <- "http://www.itis.gov/ITISWebService/services/ITISService/"
  skey_ <- "srchKey="
  tkey_ <- "tsn="
  sciname_url <- "searchByScientificName?"
  anymatch_url <- "searchForAnyMatch?"
  comnamebeg_url <- "searchByCommonNameBeginsWith?"
  comname_url <- "searchByCommonName?"
  comnameend_url <- "searchByCommonNameEndsWith?"
  itisterms_url <- "getITISTerms?"
  itistermscomname_url <- "getITISTermsFromCommonName?"
  itistermssciname_url <- "getITISTermsFromScientificName?"
  tsnsvernacular_url <- "getTsnByVernacularLanguage?"
  tsnfullhir_url <- "getFullHierarchyFromTSN?"
  if(searchtype == "sciname") {bykey <- sciname_url} else
  if(searchtype == "anymatch") {bykey <- anymatch_url} else
  if(searchtype == "comnamebeg") {bykey <- comnamebeg_url} else
  if(searchtype == "comname") {bykey <- comname_url} else
  if(searchtype == "comnameend") {bykey <- comnameend_url} else
  if(searchtype == "terms") {bykey <- itisterms_url} else
  if(searchtype == "itistermscomname") {bykey <- itistermscomname_url} else
  if(searchtype == "itistermssciname") {bykey <- itistermssciname_url} else
  if(searchtype == "tsnsvernacular") {bykey <- tsnsvernacular_url} else
  if(searchtype == "tsnfullhir") {bykey <- tsnfullhir_url} else
    end
  if (by_ ==  'name') { searchurl <- paste(base_url, bykey, skey_, searchterm, sep="") } else
    if (by_ == 'tsn' ) { searchurl <- paste(base_url, bykey, tkey_, searchterm, sep="")  } 
      end
  tt <- getURLContent(searchurl, curl=curl)
  page <- xmlTreeParse(tt)
  return(page)
}

# Examples: search by term and search type
itisxml <- get_itis_xml("Quercus_douglasii", "sciname", "name")
itisxml <- get_itis_xml("dolphin", "anymatch")
itisxml <- get_itis_xml("inch", "comnamebeg")
itisxml <- get_itis_xml("ferret-badger", "comname")
itisxml <- get_itis_xml("grizzly%20bear", "comnameend")
itisxml <- get_itis_xml("bear", "terms")
itisxml <- get_itis_xml("buya", "itistermscomname")
itisxml <- get_itis_xml("ursidae", "itistermssciname")
itisxml <- get_itis_xml("french", "tsnsvernacular")
itisxml <- 
get_itis_xml(searchterm = "36616", searchtype = "tsnfullhir", by_ = "tsn")

  # Convert xml to other formats
pagelist <- xmlToList(itisxml)
pagelist[1]



# Function to get family names to make Phylomatic input object
# input: x = quoted tsn number (taxonomic serial number)
# output: family name as character
get_familyname <- function (x) {
  temp <- get_itis_xml(searchterm = x, searchtype = "tsnfullhir", by_ = "tsn")
  templist <- ldply(xmlToList(temp), function(x) data.frame(c(x[3], x[4])))[,-3]
  hier <- na.omit(templist)
  famname <- as.character(hier[hier$rankName == 'Family',2])
  return(famname)
}
get_familyname("183327") # single taxon
laply(list("36616", "19322", "183327"), get_familyname, .progress="text") # multiple taxa



# Function to get family names to make Phylomatic input object
# AND output input string to Phylomatic for use in the function get_phylomatic_tree
# input: x = quoted tsn number (taxonomic serial number), format = one of 'isubmit' 
# or 'rsubmit'
# output: family name as character
# x <- "183327"
get_phymat_format <- function (x, format) {
  temp <- get_itis_xml(searchterm = x, searchtype = "tsnfullhir", by_ = "tsn")
  templist <- ldply(xmlToList(temp), function(x) data.frame(c(x[3], x[4])))[,-3]
  hier <- na.omit(templist)
#   names <- tolower(as.character(hier[hier$rankName == c('Family', 'Genus', 'Species'), 2]))
  names <- tolower(c(
    as.character(hier[hier$rankName == 'Family', 2]),
    as.character(hier[hier$rankName == 'Genus', 2]),
    as.character(hier[hier$rankName == 'Species', 2])))
  if (format == 'isubmit') {
    dat <- paste(names[1], "/", names[2], "/", str_replace(names[3], " ", "_"), sep='')
  } else
  if (format == 'rsubmit') {
    dat <- paste(names[1], "%2F", names[2], "%2F", str_replace(names[3], " ", "_"), sep='')
  } 
  end
return(dat)
}
get_phymat_format("183327", 'isubmit') # single taxon
dat_ <- laply(list("36616", "19322", "183327"), get_phymat_format, format='rsubmit', .progress="text") # multiple taxa
dat_mine <- paste(dat_, collapse="%0D%0A")




# Function to match multiple search terms
# Needed because get_itis_xml only searches one term at a time
# terms_: a vector of terms to search
# searchtype = one of 'sciname', 'anymatch', 'comnamebeg', 'comname', 
# 'comnameend', 'terms', 'itistermscomname', 'itistermssciname', or
# 'tsnsvernacular'
spnames <- c("Oncorhynchus_mykiss","Ailuroedus_buccoides")
terms_ <- spnames
searchtype <- "sciname"
match_itsnames <- function(terms_, searchtype) {
  
  tempxml <- sapply(terms_, function(x) get_itis_xml(x, searchtype) )
  tempxmllist <- lapply(tempxml, xmlToList)
  tsns <- sapply(tempxmllist, )
  
}
match_itsnames(spnames, "sciname")
get_itis_xml("Oncorhynchus mykiss", "sciname")

# E.g.
  # input data set
taxdat <- read.csv()

  #
match_itsnames(spnames, "sciname")


# Format tree string, submit to Phylomatic, get newick tree
# Submitted in POST format (not GET format)
# Version: already have in Phylomatic input format
# forward slash (/ -> %2F)
# newline (\n -> %0D%0A)
# input: x = phylomatic format input, 
# convert = one of 'TRUE' of 'FALSE'
# get = 'GET' or 'POST' format for submission to the website
# format = newick or xml output, 
# retphylo = return phylo tree object (TRUE or FALSE)
# output: newick tree
get_phylomatic_tree <- function (x, convert = TRUE, get, format, retphylo = TRUE) {
  # require igraph
	if(!require(ape)) stop("must first install 'igraph' package.")
  
  url <- "http://phylodiversity.net/phylomatic/pm/phylomatic.cgi"
  
  if (convert == 'TRUE') {
    treestring <- str_replace_all(str_replace_all(x, "/", "%2F"), "\n", "%0D%0A")
  } else
  if (convert == 'FALSE') {
    treestring <- x
  }
  
  collapse_double_root <- function(y) {
    temp <- str_split(y, ")")[[1]]
    double <- c(length(temp)-1, length(temp))
    tempsplit <- temp[double]
    tempsplit_1 <- str_split(tempsplit[1], ":")[[1]][2]
    tempsplit_2 <-str_split(tempsplit[2], ":")[[1]]
    rootlength <- as.numeric(tempsplit_1) + 
      as.numeric(str_split(tempsplit_2[2], ";")[[1]][1])
    newx <- paste(")", tempsplit_2[1], ":", rootlength, ";", sep="")
    newpre <- str_replace(temp[1], "[(]", "")
    allelse <- temp[-1]
    allelse <- allelse[setdiff(1:length(allelse), double-1)]
    allelse <- paste(")", allelse, sep="")
    tempdone <- paste(newpre, paste(allelse, collapse=""), newx, sep="")
  return(tempdone)
  }

  colldouble <- function(z) {
    if ( class ( try ( read.tree(text = z), silent = T ) ) %in% 'try-error') 
      { treephylo <- collapse_double_root(z) } else
      { treephylo <- z }
  return(treephylo)
  }
  
#   treestring <- dat_mine
#   format <- 'new'
  if (get == 'POST') {  
    gettree <- postForm(url,
        format = format, 
        tree = treestring
        )
    tree_ <- gsub("\n", "", gettree[[1]]) 
    treenew <- colldouble(tree_)
  } else
  
  if (get == 'GET') {0
    if (format == 'xml') {
      urlplus <- paste(url, "?", "format=", format, "&tree=", treestring, sep="")
      tt <- getURLContent(urlplus, curl=getCurlHandle())
      page <- xmlParse(tt)
      tree_ <- xmlToList(page)$newick
      treenew <- colldouble(tree_)
    } else
    
    if (format == 'new') {
      urlplus <- paste(url, "?", "format=", format, "&tree=", treestring, sep="")
      tt <- getURLContent(urlplus, curl=getCurlHandle())
      tree <- tt[[1]]
      tree_ <- gsub("\n", "", tree[[1]])
      treenew <- colldouble(tree_)
    } else
    {stop("Error: format must be one of 'xml' or 'new' (for newick)")}
  } else
  {stop("Error: get must be one of 'POST' or 'GET'")}
  
  if (retphylo == 'TRUE') {
    treenew <- read.tree(text = treenew)
  } else
  if (retphylo == 'FALSE') { treenew <- treenew }
  
return(treenew)
}

# E.g.
phyformat <- "annonaceae/annona/annona_cherimola
annonaceae/annona/annona_muricata"

phyformat <- "annonaceae/annona/annona_cherimola
annonaceae/annona/annona_muricata
fagaceae/quercus/quercus_robur
dipterocarpaceae/shorea/shorea_parvifolia
pandanaceae/pandanus/pandanus_minor
poaceae/oryza/oryza_sativa
malvaceae/durio/durio_zibethinus
rosaceae/rubus/rubus_ulmifolius
apocynaceae/asclepias/asclepias_curassavia
anacardiaceae/pistacia/pistacia_lentiscus"

# Using tree strings that need to be formatted within the function
get_phylomatic_tree(phyformat, 'TRUE', 'GET', 'xml', 'TRUE')
get_phylomatic_tree(phyformat, 'TRUE', 'GET', 'new', 'FALSE')
get_phylomatic_tree(phyformat, 'TRUE', 'POST', 'xml', 'TRUE')

tree <- get_phylomatic_tree(phyformat, 'TRUE', 'POST', 'new', 'FALSE')
treephylo <- read.tree(text = tree)
plot(treephylo)

# using a tree string already formatted for sumission to Phylomatic (convert = 'FALSE')
get_phylomatic_tree(dat_mine, 'FALSE', 'POST', 'new', 'TRUE')







# ADD SEARCHES OF THE FOLLOWING DATABASES
# USDA plants database
# 
