# param: x (character) a rank name (e.g., "variety")
which_rank <- function(x, zoo = FALSE) {
  rr <- if (zoo) taxize_ds$rank_ref_zoo else taxize_ds$rank_ref
  which(sapply(rr$ranks, function(z) {
    any(unlist(strsplit(z, split = ",")) == x)
  }, USE.NAMES = FALSE))
}

prune_too_low <- function(x, rank, ignore_no_rank = FALSE, zoo = FALSE) {
  rr <- if (zoo) taxize_ds$rank_ref_zoo else taxize_ds$rank_ref
  rank_target_no <- as.numeric(rr[which_rank(rank, zoo = zoo), "rankid"])
  rank_nos <- as.numeric(
    rr[vapply(x$rank, function(z) which_rank(z, zoo = zoo), 1),
    "rankid"])
  if (ignore_no_rank) rank_nos[rank_nos %in% c(300, 400)] <- 0
  x[!rank_nos > rank_target_no, ]
}
