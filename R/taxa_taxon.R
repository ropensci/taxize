.new_taxa_taxon <- function(.names = NULL, name = character(),
  rank = taxa::taxon_rank(), id = taxa::taxon_id(),
  auth = taxa::taxon_authority(),
  uri = character(), match = character(),
  multiple_matches = character(), pattern_match = character(),
  class = NULL, ...) {

  # Set names to NA if not set
  if (is.null(names) || all(is.na(.names))) {
    .names_set <- FALSE
    .names <- vctrs::vec_recycle(NA_character_, length(name))
  } else {
    .names_set <- TRUE
    vctrs::vec_assert(.names, ptype = character())
  }
  
  # Check that values are the correct type
  vctrs::vec_assert(name, ptype = character())
  vctrs::vec_assert(id, ptype = taxa::taxon_id())
  vctrs::vec_assert(auth, ptype = taxa::taxon_authority())
  vctrs::vec_assert(uri, ptype = character())
  vctrs::vec_assert(match, ptype = character())
  vctrs::vec_assert(multiple_matches, ptype = logical())
  vctrs::vec_assert(pattern_match, ptype = logical())
  
  vctrs::new_rcrd(
    list(.names = .names, name = name, rank = rank,
      id = id, auth = auth, uri = uri, match = match,
      multiple_matches = multiple_matches, pattern_match = pattern_match),
    .names_set = .names_set, ..., class = c("txid", "taxa_taxon", class))
}


taxa_taxon <- function(name = character(0), rank = NA, id = NA,
  auth = NA, .names = NA, uri = NA, match = NA,
  multiple_matches = NA, pattern_match = NA, class = NULL, ...) {

  # Cast inputs to correct values
  name <- vctrs::vec_cast(name, character())
  rank <- vctrs::vec_cast(rank, taxa::taxon_rank())
  id <- vctrs::vec_cast(id, taxa::taxon_id())
  auth <- vctrs::vec_cast(auth, taxa::taxon_authority())
  uri <- vctrs::vec_cast(uri, character())
  match <- vctrs::vec_cast(match, character())
  multiple_matches <- vctrs::vec_cast(multiple_matches, logical())
  pattern_match <- vctrs::vec_cast(pattern_match, logical())
  .names <- vctrs::vec_cast(.names, character())
  
  # Recycle ranks and databases to common length
  recycled <- vctrs::vec_recycle_common(name, rank, id, auth, uri,
    match, multiple_matches, pattern_match, .names)
  name <- recycled[[1]]
  rank <- recycled[[2]]
  id <- recycled[[3]]
  auth <- recycled[[4]]
  uri <- recycled[[5]]
  match <- recycled[[6]]
  multiple_matches <- recycled[[7]]
  pattern_match <- recycled[[8]]
  .names <- recycled[[9]]
  
  # Create taxon object
  .new_taxa_taxon(.names = .names, name = name, rank = rank, id = id,
    auth = auth,  uri = uri, match = match,
    multiple_matches = multiple_matches,
    pattern_match = pattern_match, class = class, ...) 
}


vec_cast.txid <- function(x, to, ..., x_arg, to_arg) {
  UseMethod("vec_cast.txid")
}

vec_cast.txid.default <- function(x, to, ..., x_arg, to_arg) {
  vctrs::vec_default_cast(x, to, x_arg, to_arg)
}

vec_cast.txid.txid <- function(x, to, ..., x_arg, to_arg) x

vec_cast.txid.character <- function(x, to, ..., x_arg, to_arg) taxa::taxon(x)

vec_cast.character.txid <- function(x, to, ..., x_arg, to_arg) {
  as.character(vctrs::field(x, "name"))
}

txz_named_field <- function(x, name) {
  out <- vctrs::field(x, name)
  if (!is.null(names(x))) names(out) <- names(x)
  return(out)
}
txz_uri <- function(x) txz_named_field(x, "uri")
txz_match <- function(x) txz_named_field(x, "match")
txz_mm <- function(x) txz_named_field(x, "multiple_matches")
txz_pm <- function(x) txz_named_field(x, "pattern_match")

as_txid_df <- function(x, check = TRUE) {
  taxa_taxon(
    name = x$name,
    id = x$ids,
    rank = x$rank,
    uri = x$uri,
    match = x$match,
    multiple_matches = x$multiple_matches,
    pattern_match = x$pattern_match,
    class = "gbif"
  )
}
