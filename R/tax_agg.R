#' Aggregate species data to given taxonomic rank
#'
#' @export
#' @param x Community data matrix. Taxa in columns, samples in rows.
#' @param rank character; Taxonomic rank to aggregate by.
#' @param db character; taxonomic API to use, 'ncbi, 'itis' or both, see
#' [tax_name()]. Note that each taxonomic data source has
#' their own identifiers, so that if you provide the wrong `db` value
#' for the identifier you could get a result, but it will likely be wrong (not
#' what you were expecting). If using ncbi we recommend getting an API key;
#' see [taxize-authentication]
#' @param messages (logical) If FALSE (Default) suppress messages
#' @param ... Other arguments passed to [get_tsn()] or [get_uid()]
#'
#' @details `tax_agg` aggregates (sum) taxa to a specific taxonomic level.
#' If a taxon is not found in the database (ITIS or NCBI) or the supplied taxon
#' is on higher taxonomic level this taxon is not aggregated.
#'
#'
#' @return A list of class `tax_agg` with the following items:
#' * `x` Community data matrix with aggregated data.
#' * `by` A lookup-table showing which taxa were aggregated.
#' * `n_pre` Number of taxa before aggregation.
#' * `rank` Rank at which taxa have been aggregated.
#'
#' @seealso [tax_name]
#' @examples \dontrun{
#' if (requireNamespace("vegan", quietly = TRUE)) {
#'   # use dune dataset
#'   data(dune, package='vegan')
#'   species <- c("Achillea millefolium", "Agrostis stolonifera",
#'     "Aira praecox", "Alopecurus geniculatus", "Anthoxanthum odoratum",
#'     "Bellis perennis", "Bromus hordeaceus", "Chenopodium album",
#'     "Cirsium arvense", "Comarum palustre", "Eleocharis palustris",
#'     "Elymus repens", "Empetrum nigrum", "Hypochaeris radicata",
#'     "Juncus articulatus", "Juncus bufonius", "Lolium perenne",
#'     "Plantago lanceolata", "Poa pratensis", "Poa trivialis",
#'     "Ranunculus flammula", "Rumex acetosa", "Sagina procumbens",
#'     "Salix repens", "Scorzoneroides autumnalis", "Trifolium pratense",
#'     "Trifolium repens", "Vicia lathyroides", "Brachythecium rutabulum",
#'     "Calliergonella cuspidata")
#'   colnames(dune) <- species
#'
#'   # aggregate sample to families
#'   (agg <- tax_agg(dune, rank = 'family', db = 'ncbi'))
#'
#'   # extract aggregated community data matrix for further usage
#'   agg$x
#'   # check which taxa have been aggregated
#'   agg$by
#' }
#'
#' # A use case where there are different taxonomic levels in the same dataset
#' spnames <- c('Puma','Ursus americanus','Ursidae')
#' df <- data.frame(c(1,2,3), c(11,12,13), c(1,4,50))
#' names(df) <- spnames
#' out <- tax_agg(x=df, rank = 'family', db='itis')
#' out$x
#'
#' # You can input a matrix too
#' mat <- matrix(c(1,2,3, 11,12,13), nrow = 2, ncol = 3,
#'  dimnames=list(NULL, c('Puma concolor','Ursus americanus','Ailuropoda melanoleuca')))
#' tax_agg(mat, rank = 'family', db='itis')
#' }

tax_agg <- function(x, rank, db = 'ncbi', messages=FALSE, ...)
{
  if (is.matrix(x)) {
    if (is.null(colnames(x)))
      stop("The community data matrix must have named columns")
    x <- data.frame(x, check.names = FALSE)
  }
  # bring to long format
  # df_m <- data.table::melt(x)
  x$rownames <- rownames(x)
  df_m <- setDF(suppressWarnings(data.table::melt(as.data.table(x))))

  # aggregate to family level (by querying NCBI for taxonomic classification)
  uniq_tax <- as.character(unique(df_m$variable))
  agg <- tax_name(uniq_tax, get = rank, db = db, messages = messages, ...)
  lookup <- data.frame(variable = uniq_tax, agg = agg[ , 3], stringsAsFactors = FALSE)

  # merge lookup with orig.
  df_merged <- merge(lookup, df_m, by = 'variable')

  # if not found , or on higher level -> use orig. = no aggrgation
  df_merged$agg <- ifelse(is.na(df_merged$agg), df_merged$variable, df_merged$agg)

  # bring back to long format and aggregate
  df_l <- setDF(data.table::dcast(as.data.table(df_merged),
    rownames ~ agg, value.var = 'value', fun.aggregate = sum))

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
