fill_len <- function(w, len = 3) {
  if (length(w) == len && length(unlist(w)) > 0) return(w)
  w[vapply(w, function(b) is.na(b) || nchar(b) == 0 || length(b) == 0, 
    logical(1))] <- NA_character_
  return(w)
}
pluck_taxon_part <- function(z, part) {
  unlist(fill_len(switch(part, 
    url = lapply(z$taxa, function(w) w$get_url()),
    id = lapply(z$ids, "[[", part),
    name = lapply(z$names, "[[", part),
    rank = lapply(z$ranks, "[[", "name")
  )))
}
pluck_taxon_attr <- function(z, attr) {
  unlist(lapply(z$taxa, function(w) w$attributes[[attr]]))
}

as_data_frame_general <- function(x, ...) {
  data.frame(id = pluck_taxon_part(x, "id"),
    rank = pluck_taxon_part(x, "rank"),
    name = pluck_taxon_part(x, "name"),
    class = "gbifid",
    match = pluck_taxon_attr(x, "match"),
    multiple_matches = pluck_taxon_attr(x, "multiple_matches"),
    pattern_match = pluck_taxon_attr(x, "pattern_match"),
    uri = pluck_taxon_part(x, "url"),
    stringsAsFactors = FALSE)
}

as_class_data_frame <- function(class) {
  function(x, check = TRUE) {
    tmp <- taxa::taxa(.list = 
      apply(x, 1, function(z) {
        id <- taxa::taxon_id(z["id"], switch_taxa_db(class), z["uri"])
        taxa::taxon(z["name"], z["rank"], id, 
          attributes = as.list(
            z[c("match", "multiple_matches", "pattern_match")])
        )
      })
    )
    structure(tmp, class = c(class, class(tmp)))
  }
}
