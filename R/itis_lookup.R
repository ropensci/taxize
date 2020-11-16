itis_lookup = 'https://sckott.github.io/itis-lookup/itis_lookup.csv'
itis_lookup_file <- function() {
  file.path(tx_itis_cache$cache_path_get(), "itis_lookup.csv")
}

il_fetch <- function() {
  tx_itis_cache$mkdir()
  if (!file.exists(itis_lookup_file())) {
    download.file(itis_lookup, itis_lookup_file(), quiet = TRUE)
  }
}

itis_env <- new.env()
itis_env$il_vec <- NULL
il_make_vec <- function() {
  il_fetch()
  if (is.null(itis_env$il_vec)) {
    df = data.table::fread(itis_lookup_file())
    itis_env$il_vec <- stats::setNames(df$rank_name, df$tsn)
  }
}

il_tsn2rank <- function(tsns) {
  il_make_vec()
  unname(itis_env$il_vec[match(tsns, names(itis_env$il_vec))])
}
