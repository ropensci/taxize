#' Get The Plant List csv files.
#'
#' The Plant List \url{http://www.theplantlist.org/}. Note there is a package on
#' CRAN (taxonstand - \url{http://cran.r-project.org/web/packages/Taxonstand/})
#' that uses theplantlist.org to search plant names - we have a wrapper around
#' that function called tpl_search.
#'
#' @export
#' @param x Directory to write csv files to.
#' @param family If you want just one, or >1 family, but not all, list them in
#' a vector.
#' @details Throws a warning if you already have a directory of the one
#' provided, but still works. Writes to your home directory, change dir_ as needed.
#' @return Returns nothing to console, except a message and progress bar.
#' Writes csv files to dir_.
#' @author John Baumgartner (johnbb@@student.unimelb.edu.au)
#' @seealso \code{\link{tpl_search}} \code{\link{tpl_families}}
#' @examples \dontrun{
#' # Get a few families
#' tpl_get("~/foo2", family = c("Platanaceae","Winteraceae"))
#'
#' # You can now get Gymnosperms as well
#' tpl_get("~/foo2", family = c("Pinaceae","Taxaceae"))
#'
#' # You can get mosses too!
#' tpl_get("~/foo4", family = "Echinodiaceae")
#'
#' # Get all families
#' ## Beware, will take a while
#' ## tpl_get("~/foo")
#' }

tpl_get <- function(x, family = NULL) {
  temp <- GET('http://www.theplantlist.org/1.1/browse/-/')
  temp <- htmlParse(content(temp, "text"), encoding = "UTF-8")
  families <- xpathSApply(temp, "//ul[@id='nametree']//a", xmlValue)
  csvlinks <- sprintf('http://www.theplantlist.org%s%s.csv',
                      xpathSApply(temp, "//ul[@id='nametree']//a", xmlGetAttr, 'href'),
                      families)
  if (!is.null(family) && all(!family %in% families)) {
    stop(paste('Requested families not found on TPL.',
               'Use tpl_families() to list plant families indexed by TPL.'),
         call. = FALSE)
  }
  if (!is.null(family) && any(!family %in% families)) {
    warning(sprintf('Requested families not found on TPL: %s.\n%s',
                    paste(family[!family %in% families], collapse = ', '),
                    'Use tpl_families() to list plant families indexed by TPL.'),
            call. = FALSE)
  }
  if (!is.null(family)) {
    csvlinks <- csvlinks[families %in% family]
    families <- families[families %in% family]
  }

  message("Downloading csv files to ", x, "...")
  dir.create(x, showWarnings = FALSE, recursive = TRUE)
  l_ply(csvlinks, getcsv, x = x, .progress = "text")
  message("...el fin")
}

getcsv <- function(z, x) {
  download.file(z, destfile = file.path(x, basename(z)), quiet = TRUE)
}
