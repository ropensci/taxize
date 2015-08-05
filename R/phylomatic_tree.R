#' Query Phylomatic for a phylogenetic tree.
#'
#' @import httr stringr
#' @export
#'
#' @param taxa Phylomatic format input of taxa names.
#' @param taxnames If true, we get the family names for you to attach to your
#'    species names to send to Phylomatic API. If FALSE, you have to provide the
#'    strings in the right format.
#' @param get 'GET' or 'POST' format for submission to the website.
#' @param informat One of newick, nexml, or cdaordf. If using a stored tree,
#'    informat should always be newick.
#' @param method One of phylomatic or convert
#' @param storedtree One of R20120829 (Phylomatic tree R20120829 for plants),
#'    smith2011 (Smith 2011, plants), or binindaemonds2007 (Bininda-Emonds 2007,
#'    mammals).
#' @param treeuri URL for a phylogenetic tree in newick format.
#' @param taxaformat Only option is slashpath for now. Leave as is.
#' @param outformat One of newick, nexml, or fyt.
#' @param clean Return a clean tree or not.
#' @param db One of "ncbi", "itis", or "apg". If there are gymnosperms in your taxa
#' list, don't use apg, intead use ncbi or itis.
#' @param verbose Print messages (default: TRUE).
#' @param ... Curl options passed on to \code{\link[httr]{GET}} or \code{\link[httr]{POST}}
#' @details Use the web interface here \url{http://phylodiversity.net/phylomatic/}
#' @return Newick formatted tree or nexml text.
#' @rdname phylomatic_tree-deprecated
#' @examples \dontrun{
#' # Input taxonomic names
#' taxa <- c("Poa annua", "Phlox diffusa", "Helianthus annuus")
#' tree <- phylomatic_tree(taxa=taxa, get = 'POST')
#' plot(tree, no.margin=TRUE)
#'
#' # Genus names
#' taxa <- c("Poa", "Phlox", "Helianthus")
#' tree <- phylomatic_tree(taxa=taxa, storedtree='R20120829', get='POST')
#' plot(tree, no.margin=TRUE)
#'
#' # Lots of names
#' taxa <- c("Poa annua", "Collomia grandiflora", "Lilium lankongense", "Phlox diffusa",
#' "Iteadaphne caudata", "Gagea sarmentosa", "Helianthus annuus")
#' tree <- phylomatic_tree(taxa=taxa, get = 'POST')
#' plot(tree, no.margin=TRUE)
#'
#' # Output NeXML format
#' taxa <- c("Gonocarpus leptothecus", "Gonocarpus leptothecus", "Lilium lankongense")
#' out <- phylomatic_tree(taxa=taxa, get = 'POST', outformat = "nexml")
#' cat(out)
#'
#' # Lots of names, note that when you have enough names (number depends on length of individual
#' # names, so there's no per se rule), you will get an error when using \code{get='GET'},
#' # when that happens use \code{get='POST'}
#' spp <- names_list("species", 200)
#' # (out <- phylomatic_tree(taxa = spp, get = "GET"))
#' (out <- phylomatic_tree(taxa = spp, get = "POST"))
#' plot(out)
#'
#' # Pass in a tree from a URL on the web
#' spp <- c('Abies amabilis','Abies balsamea','Abies bracteata','Abies concolor','Abies fraseri',
#'    'Abies grandis','Abies lasiocarpa','Abies magnifica','Abies procera','Acacia berlandieri')
#' spp <- c('Pinus koraiensis', 'Pinus sibirica', 'Pinus albicaulis', 'Pinus lambertiana',
#'          'Pinus bungeana', 'Pinus strobus', 'Pinus_cembra')
#' url <- "http://datadryad.org/bitstream/handle/10255/dryad.8791/final_tree.tre?sequence=1"
#' phylomatic_tree(taxa=spp, treeuri=url)
#'
#' # If there gymnosperms in your taxa list, use db of itis or ncbi
#' taxa <- c('Abies amabilis','Abies balsamea','Abies grandis','Abies lasiocarpa',
#' 'Abies magnifica','Abies procera','Acacia berlandieri','Poa annua')
#' (tree1 <- phylomatic_tree(taxa=taxa, db="ncbi"))
#' plot(tree1)
#' }

phylomatic_tree <- function(taxa, taxnames = TRUE, get = 'GET',
  informat = "newick", method = "phylomatic", storedtree = "R20120829", treeuri = NULL,
  taxaformat = "slashpath", outformat = "newick", clean = "true", db="apg", verbose=TRUE, ...)
{
  .Deprecated(msg = "This function is deprecated - will be removed in a future version of this pacakge. See ?`taxize-deprecated`")

  url = "http://phylodiversity.net/phylomatic/pmws"

  if (taxnames) {
    dat_ <- phylomatic_format(taxa, format = 'isubmit', db = db)

    checknas <- sapply(dat_, function(x) strsplit(x, "/")[[1]][1])
    checknas2 <- checknas[match("na", checknas)]
    if (is.numeric(checknas2)) {
      stop(sprintf("A family was not found for the following taxa:\n %s \n\n try setting taxnames=FALSE, and passing in a vector of strings, like \n%s",
                   paste(sapply(dat_, function(x) strsplit(x, "/")[[1]][3])[match("na", checknas)], collapse = ", "),
                   'phylomatic_tree(taxa = c("asteraceae/taraxacum/taraxacum_officinale", "ericaceae/gaylussacia/gaylussacia_baccata", "ericaceae/vaccinium/vaccinium_pallidum"), taxnames=FALSE, parallel=FALSE)'
      ))
    }
  } else {
    dat_ <- taxa
  }

  if (length(dat_) > 1) {
    dat_ <- paste(dat_, collapse = "\n")
  }

  # Only one of storedtree or treeuri
  if (!is.null(treeuri)) storedtree <- NULL

  args <- tc(list(taxa = dat_, informat = informat, method = method,
                       storedtree = storedtree, treeuri = treeuri, taxaformat = taxaformat,
                       outformat = outformat, clean = clean))

  if (get == 'POST') {
    # tt <- POST(url, body = list(taxa = dat_), query=args, multipart=FALSE)
    ff <- tempfile(fileext = ".txt")
    argstofile <- paste(names(args), args, collapse = "&", sep = "=")
    writeLines(argstofile, con = ff)
    tt <- POST(url, body = upload_file(ff), ...)
    out <- content(tt, "text")
  } else if (get == 'GET') {
    tt <- GET(url, query = args, ...)
    if (tt$status_code == 414) {
      stop("(414) Request-URI Too Long - Use get='POST' in your function call", call. = FALSE)
    } else {
      stop_for_status(tt)
    }
    out <- content(tt, as = "text")
  } else {
    stop("Error: get must be one of 'POST' or 'GET'")
  }

  if (grepl("No taxa in common", out)) {
    stop(out)
  } else {
    # parse out missing taxa note
    if (grepl("\\[NOTE: ", out)) {
      taxa_na <- str_extract(out, "NOTE:.+")
      taxa_na2 <- str_extract(taxa_na, ":\\s[A-Za-z].+")
      taxa_na2 <- strsplit(taxa_na2, ",")[[1]][-length(strsplit(taxa_na2, ",")[[1]])]
      taxa_na2 <- gsub(":|\\s", "", taxa_na2)
      taxa_na2 <- sapply(taxa_na2, function(x) strsplit(x, "/")[[1]][[3]], USE.NAMES = FALSE)
      taxa_na2 <- taxize_capwords(gsub("_", " ", taxa_na2), onlyfirst = TRUE)

      mssg(verbose, taxa_na)
      out <- gsub("\\[NOTE:.+", ";\n", out)
    } else {
        taxa_na2 <- NULL
    }

    outformat <- match.arg(outformat, choices = c("nexml",'newick'))
    res <- switch(outformat,
           nexml = out,
           newick = getnewick(out))
    class(res) <- c("phylo","phylomatic")
    attr(res, "missing") <- taxa_na2
    return( res )
  }
}

collapse_double_root <- function(y) {
  temp <- str_split(y, ")")[[1]]
  double <- c(length(temp) - 1, length(temp))
  tempsplit <- temp[double]
  tempsplit_1 <- str_split(tempsplit[1], ":")[[1]][2]
  tempsplit_2 <- str_split(tempsplit[2], ":")[[1]]
  rootlength <- as.numeric(tempsplit_1) +
    as.numeric(str_split(tempsplit_2[2], ";")[[1]][1])
  newx <- paste0(")", tempsplit_2[1], ":", rootlength, ";")
  newpre <- str_replace(temp[1], "[(]", "")
  allelse <- temp[-1]
  allelse <- allelse[setdiff(1:length(allelse), double - 1)]
  allelse <- paste0(")", allelse)
  tempdone <- paste0(newpre, paste(allelse, collapse = ""), newx)
  return(tempdone)
}

colldouble <- function(z) {
  ztry <- try( read.tree(text = z), silent = TRUE)
  if (class(ztry) %in% 'try-error') {
    treephylo <- collapse_double_root(z)
  } else {
    treephylo <- z
  }
  return(treephylo)
}

getnewick <- function(x){
  tree <- gsub("\n", "", x[[1]])
  read.tree(text = colldouble(tree))
}
