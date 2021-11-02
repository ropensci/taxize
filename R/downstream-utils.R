# param: x (character) a rank name (e.g., "variety")
which_rank <- function(x, zoo = FALSE) {
  rr <- if (zoo) taxize_ds$rank_ref_zoo else taxize_ds$rank_ref
  which(sapply(rr$ranks, function(z) {
    any(unlist(strsplit(z, split = ",")) == x)
  }, USE.NAMES = FALSE))
}

prune_too_low <- function(x, rank, ignore_no_rank = FALSE, zoo = FALSE,
  ignore_missing_rank = FALSE) {

  # ignore_missing_rank
  if (ignore_missing_rank) x$rank[is.na(x$rank)] <- "no rank"

  # the pruning
  rr <- if (zoo) taxize_ds$rank_ref_zoo else taxize_ds$rank_ref
  rank_target_no <- as.numeric(rr[which_rank(rank, zoo = zoo), "rankid"])
  rank_nos <- as.numeric(rr$rankid[vapply(x$rank, FUN.VALUE = numeric(1), function(z) {
    out <- which_rank(z, zoo = zoo)
    if (length(out) == 0) {
      stop('The rank "', z, '" is not part of rank_ref so cannot be assigned a relative position.',
           '\n\n  Post an issue at https://github.com/ropensci/taxize/issues/ to get the rank added.')
    } else {
      return(out)
    }
  })])
  if (ignore_no_rank) rank_nos[rank_nos %in% c(300, 400)] <- 0
  x[!rank_nos > rank_target_no, ]
}
