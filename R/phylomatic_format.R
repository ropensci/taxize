#' Get family names to make Phylomatic input object, and output input string
#'    to Phylomatic for use in the function phylomatic_tree.
#'
#' @export
#' @import XML
#' @param taxa quoted tsn number (taxonomic serial number)
#' @param format output format, isubmit (you can paste in to the Phylomatic
#'     website), or 'rsubmit' to use in fxn phylomatic_tree
#' @param db One of "ncbi", "itis", or "apg"
#' @return e.g., "pinaceae/pinus/pinus_contorta", in Phylomatic submission format.
#' @examples \dontrun{
#' mynames <- c("Poa annua", "Salix goodingii", "Helianthus annuus")
#' phylomatic_format(mynames, format='rsubmit')
#' phylomatic_format(mynames, format='isubmit', db="apg")
#' }
phylomatic_format <- function(taxa = NA, format='isubmit', db="ncbi") {

  foo <- function(nnn){
    # split up strings if a species name
    taxa2 <- strsplit(gsub("_"," ",nnn), "\\s")[[1]]
    taxa_genus <- tc(taxa2[[1]], onlyfirst = TRUE)

    if (db %in% c("ncbi","itis")) {
      family <- tax_name(query = taxa_genus, get = "family", db = db)$family
    } else {
      tplfamily <- theplantlist[ match(taxa_genus, theplantlist$genus), "family" ]
      dd <- apg_families[ match(tplfamily, apg_families$this), ]
      if (nchar(as.character(dd$that)) == 0) {
        family <- dd$this
      } else {
        family <- dd$that
      }
    }
    stringg <- c(family, strsplit(nnn, " ")[[1]])
    stringg <- tolower(as.character(stringg))
    if (format == 'isubmit') {
      paste(stringg[[1]], "/", stringg[2], "/", tolower(str_replace(nnn, " ", "_")), sep = '')
    } else
      if (format == 'rsubmit') {
        paste(stringg[[1]], "%2F", stringg[2], "%2F", tolower(str_replace(nnn, " ", "_")), sep = '')
      }
  }

  sapply(taxa, foo, USE.NAMES = FALSE)
}
