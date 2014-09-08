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

#' Lookup-table for family, genus, and species names for ThePlantList
#' @name theplantlist
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
#' The following functions are now deprecated - see the function \code{classification}
#'
#' \itemize{
#'  \item \code{\link{col_classification}}: This function is deprecated. See \code{classification}
#'  \item \code{\link{eol_hierarchy}}: This function is deprecated. See \code{classification}
#'  \item \code{\link{tp_classification}}: This function is deprecated. See \code{classification}
#' }
#'
#' The following functions have changed names. The old function names are still available to call,
#' but simply return a message directing the user to the new function name.
#'
#' \itemize{
#'  \item \code{\link{get_seqs}}: This function has changed name to \code{ncbi_getbyname}.
#'  \item \code{\link{get_genes}}: This function has changed name to \code{ncbi_getbyid}.
#'  \item \code{\link{get_genes_avail}}: This function has changed name to \code{ncbi_search}.
#'  \item \code{\link{tp_acceptednames}}: This function has changed name to \code{tp_accnames}.
#'  \item \code{\link{tp_namedistributions}}: This function has changed name to \code{tp_dist}.
#'  \item \code{\link{tp_namereferences}}: This function has changed name to \code{tp_refs}.
#'  \item \code{\link{itis_name}}: This function has changed name to \code{tax_name}.
#' }
#'
#' In addition, \code{tpl_search} is deprecated - use the Taxonstand functions \code{TPL} or
#' \code{TPLck} directly.
#'
#' @name taxize-deprecated
NULL
