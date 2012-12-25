#' Interface to the Taxa Validation Tool (TVT) of \url{http://www.freshwaterecology.info}
#' 
#' @import XML RCurl
#' @param x character; vector of (scientific) species names.
#' @return a data.frame with the status of submitted taxa.
#' 
#' @description Check Species Names with the Taxa Validation Tool (TVT) from 
#'  \url{http://www.freshwaterecology.info}.
#'  
#' @details This also set up a temporary cookie, which is used in other funcions like 
#'  \link{fresh_traits} or \link{fresh_codes}.
#' 
#' @note Currently only the macro-invertebrate database is supported.
#' 
#' @author Eduard Szoecs \email{szoe8822@@uni-landau.de}
#' @export
#' @examples \dontrun{
#' spec <- c("Acentrella sinaica",
#' "Acentria ephemerella",
#' "Acilius sp.",
#' "Acroloxus lacustris",
#' "Allotrichi pallicornis")
#'  
#' a <- fresh_validate(spec)
#' a
#' }
fresh_validate <- function(x) {
  if(!is.character(x))
    stop('Need character vector as input!')
  # create temporary .txt file
  tmp_file <- paste(tempfile(), ".txt", sep = "")
  write.table(x, tmp_file, sep = ";", row.names = FALSE)
  curl <- getCurlHandle()
  cookiefile <- tempfile(pattern='cookiefile', fileext='.txt')
  curlSetOpt(cookiefile=cookiefile, curl=curl)
  # Query TVT
  ret <- postForm("http://www.freshwaterecology.info/TaxaDB_TVT.php", .params=list(
    taxadbtvt_filename=fileUpload(tmp_file),
    cboorggroup='0',
    cbokeycode1='1',
    cbokeycode2='0',
    chkHeader='1',
    cbocsvdelimiter=';',
    cbocsvenclosure='"',
    btnsubmit='Upload'),
                curl = curl)
  # remove tmp_file
  unlink(tmp_file)
  # Parse results
  tt <- htmlParse(ret, asText=TRUE)
  tables <- getNodeSet(tt, "//table")
  
  tab <- readHTMLTable(tables[[2]])
  # Clean output
  tab <- tab[-1 , c(2,4,5,6)]
  names(tab) <- c("Status", "Genus", "Species", "Submitted")
  out <- list(tab = tab, curl = curl)
  class(out) <- "tvt"
  out
}

#' @method print tvt
#' @export
x <- print.tvt <- function(x, ...) {
  print(x$tab)
}