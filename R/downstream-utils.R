which_rank <- function(x) {
  which(sapply(rank_ref$ranks, function(z) {
    any(unlist(strsplit(z, split = ",")) == x)
  }, USE.NAMES = FALSE)
  )
}

prune_too_low <- function(x, rank, ignore_no_rank = FALSE) {
  rank_target_no <- as.numeric(rank_ref[which_rank(rank), "rankid"])
  rank_nos <- as.numeric(rank_ref[vapply(x$rank, function(z) which_rank(z), 1),
                                  "rankid"])
  if (ignore_no_rank) rank_nos[rank_nos %in% c(300, 400)] <- 0
  x[!rank_nos > rank_target_no, ]
}
