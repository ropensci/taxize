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
