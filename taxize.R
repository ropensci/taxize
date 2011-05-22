# Development for package taxize
# Goal: Search taxonomic information on multiple web data bases
# taxize.R
require(XML); require(stringr); require(RCurl); require(plyr)

# Function to search individual strings
get_itis_xml <- function(searchterm, searchtype, curl=getCurlHandle()) {
  
  base_url <- "http://www.itis.gov/ITISWebService/services/ITISService/"
  skey_ <- "srchKey="
  sciname_url <- "searchByScientificName?"
  anymatch_url <- "searchForAnyMatch?"
  comnamebeg_url <- "searchByCommonNameBeginsWith?"
  comname_url <- "searchByCommonName?"
  comnameend_url <- "searchByCommonNameEndsWith?"
  itisterms_url <- "getITISTerms?"
  itistermscomname_url <- "getITISTermsFromCommonName?"
  itistermssciname_url <- "getITISTermsFromScientificName?"
  tsnsvernacular_url <- "getTsnByVernacularLanguage?"
  if(searchtype == "sciname") {bykey <- sciname_url} else
  if(searchtype == "anymatch") {bykey <- anymatch_url} else
  if(searchtype == "comnamebeg") {bykey <- comnamebeg_url} else
  if(searchtype == "comname") {bykey <- comname_url} else
  if(searchtype == "comnameend") {bykey <- comnameend_url} else
  if(searchtype == "terms") {bykey <- itisterms_url} else
  if(searchtype == "itistermscomname") {bykey <- itistermscomname_url} else
  if(searchtype == "itistermssciname") {bykey <- itistermssciname_url} else
  if(searchtype == "tsnsvernacular") {bykey <- tsnsvernacular_url} else
    end
  searchurl <- paste(base_url, bykey, skey_, searchterm, sep="")
  tt <- getURLContent(searchurl, curl=curl)
  page <- xmlParse(tt)
  return(page)
  
}

# Examples: search by term and search type
itisxml <- get_itis_xml("Helianthus_annuus", "sciname")
itisxml <- get_itis_xml("dolphin", "anymatch")
itisxml <- get_itis_xml("inch", "comnamebeg")
itisxml <- get_itis_xml("ferret-badger", "comname")
itisxml <- get_itis_xml("grizzly%20bear", "comnameend")
itisxml <- get_itis_xml("bear", "terms")
itisxml <- get_itis_xml("buya", "itistermscomname")
itisxml <- get_itis_xml("ursidae", "itistermssciname")
itisxml <- get_itis_xml("french", "tsnsvernacular")

# Function to convert xml to other formats
pagelist <- xmlToList(itisxml)
pagelist[1]

# Function to match names
# terms_: a vector of terms to search
spnames <- c("Oncorhynchus_mykiss","Ailuroedus_buccoides")
terms_ <- spnames
searchtype <- "sciname"
match_itsnames <- function(terms_, searchtype) {
  
  tempxml <- sapply(terms_, function(x) get_itis_xml(x, "sciname") )
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





# Functions to get trees from Phylomatic
  # GET format
    # xml format
url <- "http://phylodiversity.net/phylomatic/pm/phylomatic.cgi?format=xml&tree=annonaceae%2Fannona%2Fannona_cherimola%0D%0Aannonaceae%2Fannona%2Fannona_muricata"
tt <- getURLContent(url, curl=getCurlHandle())
page <- xmlParse(tt)
page
page_ <- xmlToList(page)
page_$newick
    # newick format
url <- "http://phylodiversity.net/phylomatic/pm/phylomatic.cgi?format=new&tree=annonaceae%2Fannona%2Fannona_cherimola%0D%0Aannonaceae%2Fannona%2Fannona_muricata"
tt <- getURLContent(url, curl=getCurlHandle())
tt[[1]]


  # POST format
    # xml format
url <- "http://phylodiversity.net/phylomatic/pm/phylomatic.cgi"
x <- postForm(url,
  format = 'xml', 
  tree = 'annonaceae%2Fannona%2Fannona_cherimola%0D%0Aannonaceae%2Fannona%2Fannona_muricata'
  )
x[[1]]

    # newick format
url <- "http://phylodiversity.net/phylomatic/pm/phylomatic.cgi"
x <- postForm(url,
  format = 'new', 
  tree = 'annonaceae%2Fannona%2Fannona_cherimola%0D%0Aannonaceae%2Fannona%2Fannona_muricata'
  )
x[[1]]


# Format tree string, submit to Phylomatic, get newick tree
# Submitted in POST format (not GET format)
# Version: already have in Phylomatic input format
# forward slash (/ -> %2F)
# newline (\n -> %0D%0A)
# output: newick tree
# input: x = phylomatic format input, format = newick or xml output, 
# retphylo = return phylo tree object (TRUE or FALSE)
x <- phyformat
get_phylomatic_tree <- function (x, get, format, retphylo = TRUE) {
  url <- "http://phylodiversity.net/phylomatic/pm/phylomatic.cgi"
  treestring <- str_replace_all(str_replace_all(x, "/", "%2F"), "\n", "%0D%0A")
  
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
    {stop("Error: format must be one of 'xml' or 'new' for newick")}
  } else
  {stop("Error: get must be one of 'POST' or 'GET'")}
  
  if (retphylo == 'TRUE') {
    treenew <- read.tree(text = treenew)
  } else
  { treenew <- treenew }
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

tree <- 
get_phylomatic_tree(phyformat, 'GET', 'xml', 'TRUE')
get_phylomatic_tree(phyformat, 'GET', 'new', 'FALSE')
get_phylomatic_tree(phyformat, 'POST', 'xml', 'TRUE')
tree <- get_phylomatic_tree(phyformat, 'POST', 'new', 'FALSE')
treephylo <- read.tree(text = tree)











