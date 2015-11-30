#' asdfadsfsdf
#' @examples
#' cat(wikipedia(x = "Helianthus"))
#' cat(wikipedia(x = "Helianthus annuus"))
#' cat(wikipedia(x = "Helianthus annuus", 1))
wikipedia <- function(x, y) {
  bb <- WikipediR::page_content("en", domain = "species.wikimedia.org", page_name = x)
  html <- xml2::read_html(bb$parse$text$`*`)
  table <- xml2::xml_find_all(html, '//table[@class="wikitable mw-collapsible mw-collapsed"]')
  xml_text(xml_children(table[1])[[y]])
}

# html <- htmlParse(bb$parse$text$`*`)
# xpathSApply(html, '//table[@class="wikitable mw-collapsible mw-collapsed"]')

# library(WikidataR)
# WikidataR::find_item("Helianthus annuus")
# res <- WikidataR::get_item("Q171497")
# res$
