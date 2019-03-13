#' Get The Plant List families.
#'
#' @export
#' @param ... (list) Curl options passed on to [`crul::verb-GET`]
#' @details Requires an internet connection in order to connect to 
#' <www.theplantlist.org>.
#' @return Returns a `data.frame` including the names of all families
#' indexed by The Plant List, and the major groups into which they fall 
#' (i.e. Angiosperms, Gymnosperms, Bryophytes and Pteridophytes).
#' @author John Baumgartner (johnbb@@student.unimelb.edu.au)
#' @seealso [`tpl_get()`]
#' @examples \dontrun{
#' # Get a data.frame of plant families, with the group name 
#' # (Angiosperms, etc.)
#' head(tpl_families())
#' }
tpl_families <- function(...) {
  cli <- crul::HttpClient$new('http://www.theplantlist.org/1.1/browse/-/',
    headers = tx_ual, opts = list(...))
  temp <- cli$get()
  temp$raise_for_status()
  temp <- xml2::read_html(temp$parse("UTF-8"), encoding = "UTF-8")
  families <- xml2::xml_text(
    xml2::xml_find_all(temp, "//ul[@id='nametree']//a"))
  groups <- as.character(factor(basename(dirname(
    xml2::xml_attr(
      xml2::xml_find_all(temp, "//ul[@id='nametree']//a"), "href"))),
        levels = c('A', 'B', 'G', 'P'),
        labels = c('Angiosperms', 'Bryophytes',
          'Gymnosperms', 'Pteridophytes')))
  data.frame(group = groups, family = families, 
    stringsAsFactors = FALSE)
}
