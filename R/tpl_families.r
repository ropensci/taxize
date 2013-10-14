#' Get The Plant List families. 
#' 
#' @import RCurl XML
#' @param dir_ Directory to write csv files to.
#' @param family If you want just one, or >1 family, but not all, list them in 
#' a vector.
#' @details Throws a warning if you already have a directory of the one 
#' provided, but still works. Writes to your home directory, change dir_ as needed.
#' @return Returns nothing to console, except a message and progress bar. 
#' Writes csv files to dir_.
#' @author John Baumgartner (johnbb@@student.unimelb.edu.au)
#' @seealso \code{\link{tpl_search}} \code{\link{tpl_get}}
#' @examples \donttest{
#' # Get a data.frame of plant families, with the group name (Angiosperms, etc.)
#' head( tpl_families() )
#' }
#' @export

tpl_families <- function()
{
  temp <- getURL('http://www.theplantlist.org/browse/-/')
  temp <- htmlParse(temp)
  families <- xpathSApply(temp, "//ul[@id='nametree']//a", xmlValue)
  groups <- factor(basename(dirname(xpathSApply(temp, 
                                                "//ul[@id='nametree']//a", 
                                                xmlGetAttr, 'href'))),
                   levels=c('A', 'B', 'G', 'P'),
                   labels=c('Angiosperms', 'Bryophytes', 
                            'Gymnosperms', 'Pteridophytes'))
  data.frame(group=groups, family=families)
}