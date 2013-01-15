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
#' \tabular{ll}{
#' Encyclopedia of Life (EOL)  \tab eol_ \cr
#' Integrated Taxonomic Information Service (ITIS)  \tab itis_ \cr
#' Phylomatic \tab phylomatic_ \cr
#' uBio \tab ubio_ \cr
#' Global Names Resolver (from EOL/GBIF) \tab gnr_ \cr
#' Global Names Index (from EOL/GBIF) \tab gni_ \cr
#' IUCN Red List \tab iucn_ \cr
#' Tropicos (from Missouri Botanical Garden) \tab tp_ \cr
#' Plantminer \tab plantminer_ \cr
#' Theplantlist.org \tab tpl_ \cr
#' Catalogue of Life \tab col_ \cr
#' Global Invasive Species Database \tab gisd_ \cr
#' Freshwaterecology.info \tab fresh_ \cr
#' }
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
#' Lookup-table for fresh_traits
#' @name fresh_desc
#' @docType data
#' @description Lookup-table for fresh_traits. A table with three columns: 
#' \describe{
#'  \item{"Modality"}{Equates to the column-names of \link{fresh_traits}}
#'  \item{"Description"}{Description of each Modality}
#'  \item{"Trait"}{Traits}
#' }
#' @examples \dontrun{
#' data(fresh_desc)
#' head(fresh_desc)
#' }
#' @keywords data
NULL