#' Taxnomic search and phylogeny retrieval.
#' 
#' We are developing taxize as a package to allow users to search over many 
#' websites for species names (scientific and common) and download up- and 
#' downstream taxonomic hierarchical information - and many other things.
#' The functions in the package that hit a specific API have a prefix and suffix
#' separated by an underscore. They follow the format of service_whatitdoes. 
#' For example, gnr_resolve uses the Global Names Resolver API to resolve species names. 
#' General functions in the package that don't hit a specific API don't have 
#' two words separated by an underscore, e.g., classification.
#' You need API keys for Encyclopedia of Life (EOL), 
#' the Universal Biological Indexer and Organizer (uBio), Tropicos, and Plantminer.
#' 
#' @name taxize-package
#' @aliases taxize
#' @docType package
#' @title Taxnomic search and phylogeny retrieval.
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @author Eduard Szoecs \email{szoe8822@@uni-landau.de}
#' @author Carl Boettiger \email{cboettig@@gmail.com}
#' @keywords package
NULL
#' Lookup-table for IDs of taxonomic ranks
#' @name rank_ref
#' @docType data
#' @keywords data
NULL