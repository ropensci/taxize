#' Aggregate species data to given taxonomic rank
#'
#' @importFrom reshape2 melt
#' @importFrom reshape2 dcast
#'
#' @param x Community data matrix. Taxa in columns, samples in rows.
#' @param rank character; Taxonomic rank to aggregate by.
#' @param db character; taxonomic API to use, 'ncbi, 'itis' or both, see
#' \code{\link[taxize]{tax_name}}.
#' @param verbose (loigical) If FALSE (Default) suppresss messages
#' @param ... Other arguments passed to \code{\link[taxize]{get_tsn}} or \code{\link[taxize]{get_uid}}.
#'
#' @details \code{tax_agg} aggregates (sum) taxa to a specific taxonomic level.
#' If a taxon is not found in the database (ITIS or NCBI) or the supplied taxon
#' is on higher taxonomic level this taxon is not aggregated.
#'
#'
#' @return A list of class \code{tax_agg} with the following items:
#' \item{x}{Community data matrix with aggregated data.}
#' \item{by}{A lookup-table showing which taxa were aggregated.}
#' \item{n_pre}{Number of taxa before aggregation.}
#' \item{rank}{Rank at which taxa have been aggregated.}
#'
#' @export
#'
#' @seealso \code{\link[taxize]{tax_name}}
#' @examples \dontrun{
#' # use dune dataset
#' library("vegan")
#' data(dune, package='vegan')
#' species <- c("Bellis perennis", "Empetrum nigrum", "Juncus bufonius",
#' "Juncus articulatus",
#' "Aira praecox", "Eleocharis parvula", "Rumex acetosa", "Vicia lathyroides",
#' "Brachythecium rutabulum", "Ranunculus flammula", "Cirsium arvense",
#' "Hypochaeris radicata", "Leontodon autumnalis", "Potentilla palustris",
#' "Poa pratensis", "Calliergonella cuspidata", "Trifolium pratense",
#' "Trifolium repens", "Anthoxanthum odoratum", "Salix repens", "Achillea
#' millefolium",
#' "Poa trivialis", "Chenopodium album", "Elymus repens", "Sagina procumbens",
#' "Plantago lanceolata", "Agrostis stolonifera", "Lolium perenne", "Alopecurus
#' geniculatus",
#' "Bromus hordeaceus")
#' colnames(dune) <- species
#'
#' # aggregate sample to families
#' (agg <- tax_agg(dune, rank = 'family', db = 'ncbi'))
#'
#' # extract aggregated community data matrix for further usage
#' agg$x
#' # check which taxa have been aggregated
#' agg$by
#'
#' # A use case where there are different taxonomic levels in the same dataset
#' spnames <- c('Puma','Ursus americanus','Ursidae')
#' df <- data.frame(c(1,2,3), c(11,12,13), c(1,4,50))
#' names(df) <- spnames
#' out <- tax_agg(df, rank = 'family', db='itis')
#' out$x
#'
#' # You can input a matrix too
#' mat <- matrix(c(1,2,3, 11,12,13), nrow = 2, ncol = 3,
#'  dimnames=list(NULL, c('Puma concolor','Ursus americanus','Ailuropoda melanoleuca')))
#' tax_agg(mat, rank = 'family', db='itis')
#' }

tax_agg <- function(x, rank, db = 'ncbi', verbose=FALSE, ...)
{
  if(is.matrix(x))
  {
    if(is.null(colnames(x))) stop("The community data matrix must have named columns")
    x <- data.frame(x, check.names=FALSE)
  }
  # bring to long format
  x$rownames <- rownames(x)
  df_m <- melt(x, id = 'rownames')

  # aggregate to family level (by querying NCBI for taxonomic classification)
  uniq_tax <- as.character(unique(df_m$variable))
  agg <- tax_name(uniq_tax, get = rank, db = db, verbose=verbose, ...)
  lookup <- data.frame(variable = uniq_tax, agg = agg[ , 3], stringsAsFactors = FALSE)

  # merge lookup with orig.
  df_merged <- merge(lookup, df_m, by = 'variable')

  # if not found , or on higher level -> use orig. = no aggrgation
  df_merged$agg <- ifelse(is.na(df_merged$agg), df_merged$variable, df_merged$agg)

  # bring back to long format and aggregate
  df_l <- dcast(df_merged, rownames ~ agg,
                value.var='value', fun.aggregate = sum)

  rownames(df_l) <- df_l$rownames
  df_l$rownames <- NULL
  # restore order
  df_l <- df_l[x$rownames, ]
  out <- list(x = df_l, by = lookup, n_pre = ncol(x) - 1, rank = rank)
  class(out) <- 'tax_agg'
  return(out)
}

#' @method print tax_agg
#' @export
#' @rdname tax_agg
print.tax_agg <- function(x, ...)
{
  cat("\n")
  writeLines(strwrap("Aggregated community data\n",
                     prefix = "\t"))
  cat(paste("\nLevel of Aggregation:", toupper(x$rank)))
  cat(paste("\nNo. taxa before aggregation:", x$n_pre))
  cat(paste("\nNo. taxa after aggregation:", ncol(x$x)))
  cat(paste("\nNo. taxa not found:", sum(is.na(x$by$agg))))
}
