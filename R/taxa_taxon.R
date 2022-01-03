.new_taxa_taxon <- function(.names = NULL, name = character(),
  rank = taxa::taxon_rank(), id = taxa::taxon_id(),
  auth = taxa::taxon_authority(),
  uri = character(), match = character(),
  multiple_matches = logical(), pattern_match = logical(),
  misc = character(), class = NULL, ...) {

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
  vctrs::vec_assert(misc, ptype = character())
  
  vctrs::new_rcrd(
    list(.names = .names, name = name, rank = rank,
      id = id, auth = auth, uri = uri, match = match,
      multiple_matches = multiple_matches, pattern_match = pattern_match,
      misc = misc),
    .names_set = .names_set, ..., class = c("txid", "taxa_taxon", class))
}


taxa_taxon <- function(name = character(0), rank = NA, id = NA,
  auth = NA, .names = NA, uri = NA, match = NA,
  multiple_matches = NA, pattern_match = NA, misc = NA,
  class = NULL, ...) {

  # Cast inputs to correct values
  name <- vctrs::vec_cast(name, character())
  rank <- vctrs::vec_cast(rank, taxa::taxon_rank())
  id <- vctrs::vec_cast(id, taxa::taxon_id())
  auth <- vctrs::vec_cast(auth, taxa::taxon_authority())
  uri <- vctrs::vec_cast(uri, character())
  match <- vctrs::vec_cast(match, character())
  multiple_matches <- vctrs::vec_cast(multiple_matches, logical())
  pattern_match <- vctrs::vec_cast(pattern_match, logical())
  misc <- vctrs::vec_cast(misc, character())
  .names <- vctrs::vec_cast(.names, character())
  
  # Recycle ranks and databases to common length
  recycled <- vctrs::vec_recycle_common(name, rank, id, auth, uri,
    match, multiple_matches, pattern_match, misc, .names)
  name <- recycled[[1]]
  rank <- recycled[[2]]
  id <- recycled[[3]]
  auth <- recycled[[4]]
  uri <- recycled[[5]]
  match <- recycled[[6]]
  multiple_matches <- recycled[[7]]
  pattern_match <- recycled[[8]]
  misc <- recycled[[9]]
  .names <- recycled[[10]]
  
  # Create taxon object
  .new_taxa_taxon(.names = .names, name = name, rank = rank, id = id,
    auth = auth,  uri = uri, match = match,
    multiple_matches = multiple_matches,
    pattern_match = pattern_match, misc = misc, class = class, ...)
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

vec_cast.txid.character <- function(x, to, ..., x_arg, to_arg) taxa::taxon(x)

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

#' @export
#' @rdname id-accessors
txz_misc <- function(x) txz_named_field(x, "misc")

wiki_misc <- function(x, attr) {
  z <- txz_misc(x)
  if (
    is.character(z) &&
    all(vapply(z, jsonlite::validate, logical(1)))
  ) {
    tmp <- unname(vapply(z, function(w) jsonlite::fromJSON(w)[[attr]], ""))
    return(tmp)
  }
  return("")
}

#' txid
#' 
#' txid is the class all `get_*()` functions return. It is extended from
#' [vctrs::new_rcrd()] and is adapted from [taxa::taxon()] with modificaitons
#' specifically for taxize.
#' 
#' Additional classes attached include `taxa_taxon` (mostly to differentiate
#' it from objects created from [taxa::taxon()]), vctrs classes
#' `vctrs_rcrd` and `vctrs_vctr` (because rcrd is extended from vctr),
#' and a class for which data source the data is from (e.g., `gbif`).
#' 
#' See [id-accessors] for functions for accessing parts of txid objects
#' 
#' @seealso many taxize functions have S3 methods for `txid`, e.g., 
#' [children]
#' @name txid
NULL

#' @export
#' @param x an object of class `txid`
#' @param ... ignored
#' @return a tibble
#' @rdname txid
as.data.frame.txid <- function(x, ...) {
  tibble::as_tibble(
    data.frame(ids = as.character(taxa::tax_id(x)),
      name = as.character(taxa::tax_name(x)),
      rank = unname(as.character(taxa::tax_rank(x))),
      uri = txz_uri(x),
      match = txz_match(x),
      multiple_matches = txz_mm(x),
      pattern_match = txz_pm(x),
      misc = txz_misc(x),
      stringsAsFactors = FALSE)
  )
}

as_txid_df <- function(x, check = TRUE) {
  taxa_taxon(
    name = x$name,
    id = x$ids,
    rank = x$rank,
    uri = x$uri,
    match = x$match,
    multiple_matches = x$multiple_matches,
    pattern_match = x$pattern_match,
    misc = x$misc,
    class = "gbif"
  )
}

txac_template <- function(fun) {
  function(x) {
    assert(x, c("character", "txid"))
    if (is.character(x)) x else as.character(fun(x))
  }
}
txidac <- txac_template(taxa::tax_id)
txnameac <- txac_template(taxa::tax_name)
txdbac <- txac_template(taxa::tax_db)
txrankac <- txac_template(taxa::tax_rank)

names_or_ids <- function(x) {
  z <- txnameac(x)
  if (length(na.omit(z)) == 0) txidac(x) else z
}

# function used inside of all/most get_* functions for calling taxa_taxon()
make_taxa_taxon <- function(x, class, rank = NULL, ...) {
  ids <- as.character(unlist(pluck(x, "id")))
  if (!is.null(rank)) rank <- if (all(is.na(ids))) NA_character_ else rank
  res <- taxa_taxon(
    name = unlist(pluck(x, "name")) %||% NA_character_,
    id = taxa::taxon_id(ids, db = class),
    rank = if (!is.null(rank)) rank else unname(unlist(pluck(x, "rank"))),
    uri = sprintf(get_url_templates[[class]], ids),
    match = unname(unlist(pluck(x, "att"))),
    multiple_matches = unname(unlist(pluck(x, "multiple"))) %||% NA,
    pattern_match = unname(unlist(pluck(x, "direct"))) %||% NA,
    class = class,
    ...
  )
  return(res)
}
