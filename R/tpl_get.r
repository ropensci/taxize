#' Get The Plant List csv files.
#'
#' The Plant List \url{http://www.theplantlist.org/}. Note there is a package on
#' CRAN (taxonstand - \url{http://cran.r-project.org/web/packages/Taxonstand/})
#' that uses theplantlist.org to search plant names - we have a wrapper around
#' that function called tpl_search.
#'
#' @import RCurl plyr XML
#' @export
#'
#' @param dir_ Directory to write csv files to.
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
#' tpl_get(dir_ = "~/foo2", family = c("Platanaceae","Winteraceae"))
#'
#' # You can now get Gymnosperms as well
#' tpl_get(dir_ = "~/foo2", family = c("Pinaceae","Taxaceae"))
#'
#' # You can get mosses too!
#' tpl_get2(dir_ = "~/foo4", family = "Echinodiaceae")
#'
#' # Get all families
#' tpl_get("~/foo")
#' }

tpl_get <- function(dir_, family = NULL)
{
  temp <- getURL('http://www.theplantlist.org/browse/-/')
  temp <- htmlParse(temp)
  families <- xpathSApply(temp, "//ul[@id='nametree']//a", xmlValue)
  csvlinks <- sprintf('http://www.theplantlist.org%s%s.csv',
                      xpathSApply(temp, "//ul[@id='nametree']//a", xmlGetAttr, 'href'),
                      families)
  if (!is.null(family) && all(!family %in% families)) {
    stop(paste('Requested families not found on TPL.',
               'Use tpl_families() to list plant families indexed by TPL.'),
         call.=FALSE)
  }
  if (!is.null(family) && any(!family %in% families)) {
    warning(sprintf('Requested families not found on TPL: %s.\n%s',
                    paste(family[!family %in% families], collapse=', '),
                    'Use tpl_families() to list plant families indexed by TPL.'),
            call.=FALSE)
  }
  if (!is.null(family)) {
    csvlinks <- csvlinks[families %in% family]
    families <- families[families %in% family]
  }
  getcsv <- function(x) {
    download.file(x, destfile=file.path(dir_, basename(x)), quiet=TRUE)
  }
  message("Downloading csv files to ", dir_, "...")
  dir.create(dir_)
  l_ply(csvlinks, getcsv, .progress = "text")
  message("...el fin")
}
