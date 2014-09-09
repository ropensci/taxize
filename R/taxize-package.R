#' Taxnomic search and phylogeny retrieval.
#'
#' We are developing taxize as a package to allow users to search over many
#' websites for species names (scientific and common) and download up- and
#' downstream taxonomic hierarchical information - and many other things.
#'
#' The functions in the package that hit a specific API have a prefix and suffix
#' separated by an underscore. They follow the format of \code{service_whatitdoes}.
#' For example, \code{gnr_resolve} uses the Global Names Resolver API to resolve
#' species names.
#' 
#' General functions in the package that don't hit a specific API don't have
#' two words separated by an underscore, e.g., \code{classification}
#'
#' You need API keys for Encyclopedia of Life (EOL), the Universal Biological 
#' Indexer and Organizer (uBio), Tropicos, and Plantminer.
#'
#' Currently supported APIs are:
#'
#' \tabular{llc}{
#' API \tab prefix \tab SOAP? \cr
#' Encyclopedia of Life (EOL)  \tab eol \tab FALSE \cr
#' Integrated Taxonomic Information Service (ITIS)  \tab itis \tab FALSE \cr
#' Phylomatic \tab phylomatic \tab FALSE \cr
#' uBio \tab ubio \tab FALSE \cr
#' Global Names Resolver (from EOL/GBIF) \tab gnr \tab FALSE \cr
#' Global Names Index (from EOL/GBIF) \tab gni \tab FALSE \cr
#' IUCN Red List \tab iucn \tab FALSE \cr
#' Tropicos (from Missouri Botanical Garden) \tab tp \tab FALSE \cr
#' Plantminer \tab plantminer \tab FALSE \cr
#' Theplantlist.org \tab tpl \tab FALSE \cr
#' Catalogue of Life \tab col \tab FALSE \cr
#' Global Invasive Species Database \tab gisd \tab FALSE \cr
#' National Center for Biotechnology Information \tab ncbi \tab FALSE \cr
#' CANADENSYS Vascan name search API \tab vascan \tab FALSE \cr
#' International Plant Names Index (IPNI) \tab ipni \tab FALSE \cr
#' World Register of Marine Species (WoRMS) \tab worms \tab TRUE \cr
#' Barcode of Life Data Systems (BOLD) \tab bold \tab FALSE \cr
#' Pan-European Species directories Infrastructure (PESI) \tab pesi \tab TRUE \cr
#' Mycobank \tab myco \tab TRUE \cr
#' }
#' 
#' If the source above has a \code{TRUE} in the \code{SOAP?} column, it is not available if you 
#' installed this package from CRAN. See the Github repo for how to install the version with the 
#' data sources that use SOAP web services: 
#' \url{https://github.com/ropensci/taxize#version-with-soap-data-sources}
#'
#' @name taxize-package
#' @aliases taxize
#' @docType package
#' @title Taxonomic information from around the web.
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @author Eduard Szoecs \email{eduardszoecs@@gmail.com}
#' @author Carl Boettiger \email{cboettig@@gmail.com}
#' @author Karthik Ram \email{karthik@@ropensci.org}
#' @author Ignasi Bartomeus \email{nacho.bartomeus@@gmail.com}
#' @author John Baumgartner \email{johnbb@@student.unimelb.edu.au}

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
