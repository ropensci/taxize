#' Format tree string, submit to Phylomatic, get newick tree.
#' 
#' @import XML RCurl
#' @param x Phylomatic format input.
#' @param convert One of 'TRUE' of 'FALSE'.
#' @param get 'GET' or 'POST' format for submission to the website.
#' @param format Newick (new) or xml (xml) output. 
#' @param retphylo Return phylo tree object (TRUE or FALSE).
#' @details Submitted in POST format (not GET format).  
#'  Version: already have in Phylomatic input format
#'    forward slash (/ -> %2F)
#'    newline (\n -> %0D%0A)
#' @return Newick formatted tree.
#' @examples \dontrun{
#' dat_ <- laply(list("36616", "19322", "183327"), get_phymat_format, format='rsubmit', .progress="text")
#' dat_mine <- paste(dat_, collapse="%0D%0A") # collapse and replace \n's
#' tree <- get_phylomatic_tree(dat_mine, 'FALSE', 'GET', 'new', 'TRUE')
#' plot(tree)
#' }
#' @export
get_phylomatic_tree <- function (x, convert = TRUE, get, format, retphylo = TRUE, 
        url = "http://phylodiversity.net/phylomatic/pm/phylomatic.cgi") 
{  
  # require igraph
  if(!require(ape)) stop("must first install 'igraph' package.")
  
  
  if (length(x) > 1) { x <- paste(x, collapse = "\n") } else { x <- x }    
  
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
    		.params = list(format = format, 
        tree = treestring)
    )
#     postForm(urlcomplete, style="httppost")
#     POST(urlcomplete)
# #     getURLContent("http://phylodiversity.net/phylomatic/pm/phylomatic.cgi?format=xml&tree=annonaceae%2Fannona%2FAnnona_cherimola%0D%0Aannonaceae%2Fannona%2FAnnona_muricata", curl=getCurlHandle())[[1]]
#     POST(url, query = list(
#     	format = format, 
#     	tree = paste(dat_, collapse="\n")))
    tree_ <- gsub("\n", "", gettree[[1]]) 
    treenew <- colldouble(tree_)
  } else
  
  if (get == 'GET') {
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
  
treenew
}