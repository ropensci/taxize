#' Query ecological parameters from \url{freshwaterbiology.info}
#' 
#' @import XML RCurl
#' @param x tvt-object; A object of class tvt as returned from \link{fresh_validate}.
#' @return A data.frame with ecological parameters for each taxon
#' 
#' @description Query ecological parameters from \url{freshwaterbiology.info}. This function queries 
#'  all parameters. If you want data only for specific traits use the \link{fresh_desc} table (see examples).
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
#' a_traits <- fresh_traits(a)
#' a_traits
#' 
#' ### select only current preference using lookup table
#' take <- fresh_desc$Modality[fresh_desc$Trait == 'current preference']
#' a_traits[ , take]
#' }
fresh_traits <- function(x){
  if(class(x) != 'tvt')
    stop("Object of class 'tvt' as returned by fresh_validate() needed!")
  xa <- postForm("http://www.freshwaterecology.info/tvt2csv.php", .params= list(
    btnexport='Export',
    cbodecimaldelimiter='.',
    'ecoparam[]'='5',
    'ecoparam[]'='34',
    'ecoparam[]'='37',
    'ecoparam[]'='31',
    'ecoparam[]'='36',
    'ecoparam[]'='32',
    'ecoparam[]'='1',
    'ecoparam[]'='19',
    'ecoparam[]'='18',
    'ecoparam[]'='10',
    'ecoparam[]'='11',
    'ecoparam[]'='15',
    'ecoparam[]'='12',
    'ecoparam[]'='14',
    'ecoparam[]'='13',
    'ecoparam[]'='17',
    'ecoparam[]'='16',
    'ecoparam[]'='29',
    'ecoparam[]'='120',
    'ecoparam[]'='6',
    'ecoparam[]'='3',
    'ecoparam[]'='4',
    'ecoparam[]'='25',
    'ecoparam[]'='21',
    'ecoparam[]'='24',
    'ecoparam[]'='23',
    'ecoparam[]'='28',
    'ecoparam[]'='20',
    'ecoparam[]'='7',
    'ecoparam[]'='9',
    'ecoparam[]'='8',
    'ecoparam[]'='26',
    'ecoparam[]'='30',
    'ecoparam[]'='22',
    'ecoparam[]'='27',
    'ecoparam[]'='38',
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
  df <- read.table(sep = ";", header = TRUE, stringsAsFactors = FALSE,
                   text = regmatches(bin, regexpr('Genus;Species;(.*)', bin)))
  df
  # First species is missing!
  # Reset Locale
  Sys.setlocale("LC_ALL", "")
  return(df)
}