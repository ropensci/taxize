make_generic <- function(x, uu, clz, check = TRUE) {
  if (check) {
    if ( evalfxn(clz)(x) ) {
      toid(x, uu, clz)
    } else {
      structure(
        NA, class = clz, match = "not found",
        multiple_matches = FALSE, pattern_match = FALSE, uri = NA
      )
    }
  } else {
    toid(x, uu, clz)
  }
}

make_iucn_generic <- function(x, uu, clz, check = TRUE, key = NULL) {
  if (check) {
    res <- check_iucn_getname(x, key = key)
    if (length(res$result) != 0) {
      toid(res$result$taxonid, uu, clz, name = res$result$scientific_name)
    } else {
      structure(
        NA, class = clz, match = "not found",
        multiple_matches = NA, pattern_match = NA, uri = NA,
        name = NA
      )
    }
  } else {
    toid(x, uu, clz)
  }
}

make_wiki_generic <- function(x, uu, clz, check = TRUE) {
  if (check) {
    if ( evalfxn(clz)(sprintf(uu, x)) ) {
      toid(x, uu, clz)
    } else {
      structure(
        NA, class = clz, match = "not found",
        multiple_matches = FALSE, pattern_match = FALSE, uri = NA
      )
    }
  } else {
    toid(x, uu, clz)
  }
}

# messages
tx_msg_not_found <-
  "Not found. Consider checking the spelling or alternate classification"
