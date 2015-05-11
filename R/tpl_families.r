#' Get The Plant List families.
#'
#' @export
#' @details Requires an internet connection in order to connect to www.theplantlist.org.
#' @return Returns a \code{data.frame} including the names of all families indexed
#' by The Plant List, and the major groups into which they fall (i.e. Angiosperms,
#' Gymnosperms, Bryophytes and Pteridophytes).
#' @author John Baumgartner (johnbb@@student.unimelb.edu.au)
#' @seealso \code{\link{tpl_search}} \code{\link{tpl_get}}
#' @examples \dontrun{
#' # Get a data.frame of plant families, with the group name (Angiosperms, etc.)
#' head( tpl_families() )
#' }

tpl_families <- function() {
  temp <- GET('http://www.theplantlist.org/1.1/browse/-/')
  temp <- htmlParse(content(temp, "text"), encoding = "UTF-8")
  families <- xpathSApply(temp, "//ul[@id='nametree']//a", xmlValue)
  groups <- as.character(factor(basename(dirname(xpathSApply(temp,
                                                "//ul[@id='nametree']//a",
                                                xmlGetAttr, 'href'))),
                   levels = c('A', 'B', 'G', 'P'),
                   labels = c('Angiosperms', 'Bryophytes',
                            'Gymnosperms', 'Pteridophytes')))
  data.frame(group = groups, family = families, stringsAsFactors = FALSE)
}
