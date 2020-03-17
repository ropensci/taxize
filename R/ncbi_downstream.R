#' Retrieve all taxa names downstream in hierarchy for NCBI
#'
#' @export
#' @param id (numeric/integer) An NCBI taxonomic identifier
#' @param downto The taxonomic level you want to go down to. See examples below.
#' 		The taxonomic level IS case sensitive, and you do have to spell it
#' 		correctly. See `data(rank_ref)` for spelling.
#' @param intermediate (logical) If `TRUE`, return a list of length two
#' with target taxon rank names, with additional list of data.frame's of
#' intermediate taxonomic groups. Default: `FALSE`
#' @param ... Further args passed on to [ncbi_children()]
#' @return Data.frame of taxonomic information downstream to family from e.g.,
#' 		Order, Class, etc., or if `intermediate=TRUE`, list of length two,
#'   	with target taxon rank names, and intermediate names.
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @section No Rank:
#' A sticky point with NCBI is that they can have designation for taxonomic
#' rank of "No Rank". So we have no way of programatically knowing what to
#' do with that taxon. Of course one can manually look at a name and perhaps
#' know what it is, or look it up on the web - but we can't do anything
#' programatically. So, no rank things will sometimes be missing.
#' 
#' @section Authentication:
#' See [taxize-authentication()] for help on authentication. 
#' We strongly recommend getting an API key
#' 
#' @examples \dontrun{
#' ## genus Apis
#' ncbi_downstream(id = 7459, downto="species")
#'
#' ## get intermediate taxa as a separate object
#' ncbi_downstream(id = 7459, downto="species", intermediate = TRUE)
#'
#' ## Lepidoptera
#' ncbi_downstream(id = 7088, downto="superfamily")
#'
#' ## families in the ferns (Moniliformopses)
#' (id <- get_uid("Moniliformopses"))
#' ncbi_downstream(id = id, downto = "order")
#' }
ncbi_downstream <- function(id, downto, intermediate = FALSE, ...) {

  should_be('intermediate', intermediate, 'logical')

  downto <- tolower(downto)
  poss_ranks <- unique(do.call(c,
    sapply(taxize_ds$rank_ref$ranks, strsplit, split = ",",
      USE.NAMES = FALSE)))
  downto <- match.arg(downto, choices = poss_ranks)
  torank <- sapply(taxize_ds$rank_ref[which_rank(downto), "ranks"],
    function(x) strsplit(x, ",")[[1]][[1]], USE.NAMES = FALSE)

  stop_ <- "not"
  notout <- data.frame(rank = "", stringsAsFactors = FALSE)
  out <- list()
  if (intermediate) intermed <- list()
  iter <- 0
  while (stop_ == "not") {
    iter <- iter + 1
    tt <- dt2df(lapply(id, function(x) ncbi_children(id = x, ...)[[1]]))
    tt$.id <- NULL
    tt <- remove_self_ids(tt, id)
    tt <- rename(tt, c('childtaxa_rank' = 'rank'))
    tt <- prune_too_low(tt, downto, ignore_no_rank = TRUE)

    if (NROW(tt) == 0) {
      out[[iter]] <- data.frame(stringsAsFactors = FALSE)
      stop_ <- "nodata"
    } else {
      if (intermediate) intermed[[iter]] <- tt
      if (NROW(tt[tt$rank == downto, ]) > 0) {
        out[[iter]] <- tt[tt$rank == downto, ]
      }
      if (NROW(tt[!tt$rank == downto, ]) > 0) {
        notout <- tt[!tt$rank %in% torank, ]
      } else {
        notout <- data.frame(rank = downto, stringsAsFactors = FALSE)
      }

      if (all(notout$rank == downto)) {
        stop_ <- "fam"
      } else {
        id <- notout$childtaxa_id
        stop_ <- "not"
      }
    }
    if (intermediate) intermed[[iter]] <- intermed[[iter]]
  } # end while loop

  tmp <- unique(dt2df(out, idcol = FALSE))
  if (intermediate) {
    list(target = tmp, intermediate = intermed)
  } else {
    tmp
  }
}

remove_self_ids <- function(x, id) {
  x[!as.character(x$childtaxa_id) %in% as.character(id), ]
}
