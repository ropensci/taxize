#' @title Details on `get_*()` functions
#' @description Including outputs from `get_*()` functions, as well as their
#' attributes, and all exception behaviors.
#'
#' @name get_id_details
#'
#' @details This document applies to the following functions:
#' * [`get_boldid()`]
#' * [`get_colid()`]
#' * [`get_eolid()`]
#' * [`get_gbifid()`]
#' * [`get_ids()`]
#' * [`get_iucn()`]
#' * [`get_natservid()`]
#' * [`get_nbnid()`]
#' * [`get_tolid()`]
#' * [`get_tpsid()`]
#' * [`get_tsn()`]
#' * [`get_ubioid()`]
#' * [`get_uid()`]
#' * [`get_wiki()`]
#' * [`get_wormsid()`]
#'
#' @section attributes:
#' Each output from `get_*()` functions have the following attributes:
#'
#' * *match* (character) - the reason for NA, either 'not found', 'found' or
#' if `ask = FALSE` then 'NA due to ask=FALSE')
#' * *multiple_matches* (logical) - Whether multiple matches were returned by
#' the data source.
#' This can be `TRUE`, even if you get 1 name back because we try to pattern
#' match the name to see if there's any direct matches. So sometimes this
#' attribute is `TRUE`, as well as `pattern_match`, which then returns 1
#' resulting name without user prompt.
#' * *pattern_match* (logical) - Whether a pattern match was made.
#' If `TRUE` then`multiple_matches` must be `TRUE`, and we found a perfect match
#' to your name, ignoring case. If `FALSE`, there wasn't a direct match, and
#' likely you need to pick from many choices or further parameters can be used
#' to limit results
#' * *uri* (character) - The URI where more information can be read on the taxon
#' - includes the taxonomic identifier in the URL somewhere. This may be missing
#' if the value returned is `NA`
#' 
#'
#' @section exceptions:
#' The following are the various ways in which `get_*()` functions behave:
#'
#'  * success - the value returned is a character string or numeric
#'  * no matches found - you'll get an NA, refine your search or possible the
#'  taxon searched for does not exist in the database you're using
#'  * more than on match and `ask = FALSE` - if there's more than one matching
#'  result, and you have set `ask = FALSE`, then we can't determine the single
#'  match to return, so we give back `NA`. However, in this case we do set the
#'  `match` attribute to say `NA due to ask=FALSE & > 1 result` so it's very
#'  clear what happened - and you can even programatically check this as well
#'  * NA due to some other reason - some `get_*()` functions have additional
#'  parameters for filtering taxa. It's possible that even though there's
#'  results (that is, `found` will say `TRUE`), you can get back an NA. This is
#'  most likely if the parameter filters taxa after they are returned from the
#'  data provider and the value passed to the parameter leads to no matches.
NULL
