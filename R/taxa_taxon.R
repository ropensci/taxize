.new_taxa_taxon <- function(.names = NULL, name = character(),
  rank = taxa2::taxon_rank(), id = taxa2::taxon_id(),
  auth = taxa2::taxon_authority(),
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
  vctrs::vec_assert(id, ptype = taxa2::taxon_id())
  vctrs::vec_assert(auth, ptype = taxa2::taxon_authority())
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
  rank <- vctrs::vec_cast(rank, taxa2::taxon_rank())
  id <- vctrs::vec_cast(id, taxa2::taxon_id())
  auth <- vctrs::vec_cast(auth, taxa2::taxon_authority())
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


#' @method vec_cast txid
#' @importFrom vctrs vec_cast
#' @export
#' @keywords internal
vec_cast.txid <- function(x, to, ..., x_arg, to_arg) {
  UseMethod("vec_cast.txid")
}

#' @method vec_cast.txid default
#' @export
vec_cast.txid.default <- function(x, to, ..., x_arg, to_arg) {
  vctrs::vec_default_cast(x, to, x_arg, to_arg)
}

#' @method vec_cast.txid txid
#' @export
vec_cast.txid.txid <- function(x, to, ..., x_arg, to_arg) x

vec_cast.txid.character <- function(x, to, ..., x_arg, to_arg) taxa2::taxon(x)

vec_cast.character.txid <- function(x, to, ..., x_arg, to_arg) {
  as.character(vctrs::field(x, "name"))
}



#' @method vec_ptype2 txid
#' @importFrom vctrs vec_ptype2
#' @export
#' @keywords internal
vec_ptype2.txid <- function(x, y, ...) UseMethod("vec_ptype2.txid", y)


#' @method vec_ptype2.txid default
#' @export
vec_ptype2.txid.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}


#' @method vec_ptype2.txid vctrs_unspecified
#' @export
vec_ptype2.txid.vctrs_unspecified <- function(x, y, ...) x


#' @method vec_ptype2.txid txid
#' @export
vec_ptype2.txid.txid <- function(x, y, ...) x


txz_named_field <- function(x, name) {
  out <- vctrs::field(x, name)
  if (!is.null(names(x))) names(out) <- names(x)
  return(out)
}

#' Access various metadata from taxonomic id objects
#' @name id-accessors
#' @family taxonomic-ids
#' @param x result of a call to a `get_*` function, an object
#' of class `txid`
NULL

#' @export
#' @rdname id-accessors
txz_uri <- function(x) txz_named_field(x, "uri")

#' @export
#' @rdname id-accessors
txz_match <- function(x) txz_named_field(x, "match")

#' @export
#' @rdname id-accessors
txz_mm <- function(x) txz_named_field(x, "multiple_matches")

#' @export
#' @rdname id-accessors
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

txac_template <- function(fun) {
  function(x) {
    assert(x, c("character", "txid"))
    if (is.character(x)) x else as.character(fun(x))
  }
}
txidac <- txac_template(taxa2::tax_id)
txnameac <- txac_template(taxa2::tax_name)
txdbac <- txac_template(taxa2::tax_db)
txrankac <- txac_template(taxa2::tax_rank)

names_or_ids <- function(x) {
  z <- txnameac(x)
  if (length(na.omit(z)) == 0) txidac(x) else z
}
