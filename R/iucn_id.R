#' Get an ID for a IUCN listed taxon
#'
#' @export
#' @param sciname character; Scientific name. Should be cleaned and in the
#' format `*<Genus> <Species>*`. One or more.
#' @param key (character) required. you IUCN Redlist API key. See
#' [rredlist::rredlist-package] for help on authenticating with
#' IUCN Redlist
#' @param ... Curl options passed on to [crul::HttpClient]
#' @return A named list (names are input taxa names) of one or more IUCN IDs.
#' Taxa that aren't found are silently dropped.
#' @author Scott Chamberlain, 
#' @examples \dontrun{
#' iucn_id("Branta canadensis")
#' iucn_id("Branta bernicla")
#' iucn_id("Panthera uncia")
#' iucn_id("Lynx lynx")
#'
#' # many names
#' iucn_id(c("Panthera uncia", "Lynx lynx"))
#'
#' # many names, some not found
#' iucn_id(c("Panthera uncia", "Lynx lynx", "foo bar", "Gorilla gorilla gorilla"))
#'
#' # a name not found
#' iucn_id("Foo bar")
#' }
iucn_id <- function(sciname, key = NULL, ...) {
  out <- list()
  parts <- strsplit(sciname, split = ' +')
  lengths <- vapply(parts, length, FUN.VALUE = numeric(1))
  invalid_lengths <- lengths <= 1 | lengths > 3
  if (any(invalid_lengths)) {
    error_list <- paste0('    ', which(invalid_lengths), ': "', sciname[invalid_lengths], '"')
    if (length(error_list) > 100) {
      error_list <- c(error_list[1:100], '   ...')
    }
    stop(
      'The following inputs have the incorrect number of elements in their species name:\n',
      paste0(error_list, collapse = '\n')
    )
  }
  
  for (i in seq_along(parts)) {
    out[[i]] <- get_iucn_id(parts[[i]], key = key, ...)
  }
  unlist(out)
}

get_iucn_id <- function(parts, key = NULL, ...) {
  tryCatch(
    {
      if (length(parts) == 2) {
        tmp <- rredlist::rl_species(genus = parts[1], species = parts[2], key = key, ...)
      } else {
        tmp <- rredlist::rl_species(genus = parts[1], species = parts[2], infra = parts[3], key = key, ...)
      }
      tmp$taxon$sis_id
    },
    error = function(e) {
      NA_integer_
    }
  ) 
}

