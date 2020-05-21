#' taxize parameters
#' 
#' Information on standardized parameters across the package
#' 
#' @name taxize-params
#' @section Standardized parameters:
#' 
#' - `sci`: scientific name
#' - `com`: common name
#' - `id`: name identifier
#' - `sci_com`: scientific name or common name
#' - `sci_id`: scientific name or name identifier
#' 
#' We were going to standardize parameter names for cases in which
#' a parameter accepts either of three options: scientific name,
#' common name, or name identifier. However, there was no clear
#' parameter name we could use for this case, so we've left
#' parameter names as they are for the two cases ([get_ids()] and
#' [vascan_search()])
NULL
