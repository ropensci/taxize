#' Query Codes from freshwaterbiology.info
#' 
#' @import XML RCurl
#' @param x tvt-object from fresh_validate
#' @return data.frame
#' 
#' @note Currently only the Macro-invertebrate database is supported.
#' 
#' @author Eduard Szoecs \email{szoe8822@@uni-landau.de}
#' @examples \dontrun{
#' spec <- c("Acentrella sinaica",
#' "Acentria ephemerella",
#' "Acilius sp.",
#' "Acroloxus lacustris",
#' "Allotrichi pallicornis")
#'  
#' a <- fresh_validate(spec)
#' fresh_codes(a)
#' }
fresh_codes <- function(x){
  xa <- postForm("http://www.freshwaterecology.info/tvt2csv.php", .params= list(
    ExportFeld_Codelist='1',
    ExportFeld_Shortcode='1',
    btnexport='Export',
    cbodecimaldelimiter='.',
    txtgenus_ac_minChars='3',
    txtgenus_id0='0',
    txtorggroup='0',
    txtspecies_ac_minChars='3',
    txtspecies_id0='0'),
                 # need curl-object from upload (cookie!)
                 curl = x$curl,
                 style = 'httppost',
                 binary = TRUE)
  Sys.setlocale("LC_ALL", "C")
  # Read binary output
  bin <- readBin(xa, "character")
  bin
  # Crop binary string to table and read table
  df <- read.table(sep = ";", header = TRUE, colClasses='character', na.strings = "",
                   text = regmatches(bin, regexpr('Genus;Species;(.*)', bin)))
  df
  # First species is missing!
  # Reset Locale
  Sys.setlocale("LC_ALL", "")
  return(df)
}