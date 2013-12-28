#' Taxnomic search and phylogeny retrieval.
#' 
#' We are developing taxize as a package to allow users to search over many 
#' websites for species names (scientific and common) and download up- and 
#' downstream taxonomic hierarchical information - and many other things.
#' 
#' The functions in the package that hit a specific API have a prefix and suffix
#' separated by an underscore. They follow the format of service_whatitdoes. 
#' For example, gnr_resolve uses the Global Names Resolver API to resolve 
#' species names. 
#' General functions in the package that don't hit a specific API don't have 
#' two words separated by an underscore, e.g., classification.
#' 
#' You need API keys for Encyclopedia of Life (EOL), 
#' the Universal Biological Indexer and Organizer (uBio), Tropicos, and 
#' Plantminer.
#' 
#' Currently supported APIs are:
#' 
#' \tabular{ll}{
#' API \tab prefix \cr
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
#' }
#' 
#' @name taxize-package
#' @aliases taxize
#' @docType package
#' @title Taxnomic search and phylogeny retrieval.
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @author Eduard Szoecs \email{eduardszoecs@@gmail.com}
#' @author Carl Boettiger \email{cboettig@@gmail.com}
#' @keywords package
NULL

#' Lookup-table for IDs of taxonomic ranks
#' @name rank_ref
#' @docType data
#' @keywords data
NULL

#' Table of APGIII generic names
#' @name apg_genera
#' @docType data
#' @keywords data
NULL

#' Lookup-table for APGIII family names
#' @name apg_families
#' @docType data
#' @keywords data
NULL

#' Lookup-table for APGIII order names
#' @name apg_orders
#' @docType data
#' @keywords data
NULL

#' Vector of 1,182 species names of plants
#' @name plantNames
#' @docType data
#' @keywords data
NULL

#' Vector of 793 genus names of plants
#' @name plantGenusNames
#' @docType data
#' @keywords data
NULL

#' Deprecated functions in taxize
#' 
#' \itemize{
#'  \item \code{\link{col_classification}}: This function is deprecated, and the function col_classification will be removed in v0.2.0. See classification()
#'  \item \code{\link{eol_hierarchy}}: This function is deprecated, and the function eol_hierarchy will be removed in v0.2.0. See classification()
#'  \item \code{\link{tp_classification}}: This function is deprecated, and the function tp_classification will be removed in v0.2.0. See classification()
#' }
#'
#' The following functions have just changed names. The old function names are still available to call, but simply return a message directing the user to the new function name.
#' 
#' \itemize{
#'  \item \code{\link{get_seqs}}: This function has changed name to ncbi_getbyname(). The function get_seqs will be removed in v0.2.0.
#'  \item \code{\link{get_genes}}: This function has changed name to ncbi_getbyid(), The function get_genes will be removed in v0.2.0.
#'  \item \code{\link{get_genes_avail}}: This function has changed name to ncbi_search(), The function get_genes_avail will be removed in v0.2.0.
#'  \item \code{\link{tp_acceptednames}}: This function has changed name to tp_accnames(), The function tp_acceptednames will be removed in v0.2.0.
#'  \item \code{\link{tp_namedistributions}}: This function has changed name to tp_dist(), The function tp_namedistributions will be removed in v0.2.0.
#'  \item \code{\link{tp_namereferences}}: This function has changed name to tp_refs(), The function tp_namereferences will be removed in v0.2.0.
#' }
#' 
#' @name taxize-deprecated
NULL