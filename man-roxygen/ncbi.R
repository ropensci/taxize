#' @param taxa (character) Scientific name to search for.
#' @param seqrange (character) Sequence range, as e.g., \code{"1:1000"}. This is the range of 
#'    sequence lengths to search for. So \code{"1:1000"} means search for sequences from 1 to 1000
#'    characters in length.
#' @param getrelated (logical) If \code{TRUE}, gets the longest sequences of a species
#'   	in the same genus as the one searched for. If \code{FALSE}, returns nothing if no match 
#'   	found.
#' @param verbose (logical) If \code{TRUE} (default), informative messages printed.
