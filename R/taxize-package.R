#' @title Taxonomic Information from Around the Web
#'
#' @description This package interacts with a suite of web 'APIs'
#' for taxonomic tasks, such as verifying species names, getting
#' taxonomic hierarchies, and verifying name spelling.
#'
#' @section About:
#' Allows users to search over many websites for species names (scientific and
#' common) and download up- and downstream taxonomic hierarchical information -
#' and many other things.
#'
#' The functions in the package that hit a specific API have a prefix and suffix
#' separated by an underscore. They follow the format of \code{service_whatitdoes}.
#' For example, \code{gnr_resolve} uses the Global Names Resolver API to resolve
#' species names.
#'
#' General functions in the package that don't hit a specific API don't have
#' two words separated by an underscore, e.g., \code{classification}
#'
#' You need API keys for some data sources. See \code{\link{taxize-authentication}}
#' for more information.
#'
#' @section Currently supported APIs:
#'
#' \tabular{llc}{
#' API \tab prefix \tab SOAP? \cr
#' Encyclopedia of Life (EOL)  \tab eol \tab FALSE \cr
#' Taxonomic Name Resolution Service \tab tnrs \tab FALSE \cr
#' Integrated Taxonomic Information Service (ITIS)  \tab itis \tab FALSE \cr
#' Global Names Resolver (from EOL/GBIF) \tab gnr \tab FALSE \cr
#' Global Names Index (from EOL/GBIF) \tab gni \tab FALSE \cr
#' IUCN Red List \tab iucn \tab FALSE \cr
#' Tropicos (from Missouri Botanical Garden) \tab tp \tab FALSE \cr
#' Theplantlist.org \tab tpl \tab FALSE \cr
#' Catalogue of Life \tab col \tab FALSE \cr
#' National Center for Biotechnology Information \tab ncbi \tab FALSE \cr
#' CANADENSYS Vascan name search API \tab vascan \tab FALSE \cr
#' International Plant Names Index (IPNI) \tab ipni \tab FALSE \cr
#' World Register of Marine Species (WoRMS) \tab worms \tab TRUE \cr
#' Barcode of Life Data Systems (BOLD) \tab bold \tab FALSE \cr
#' Pan-European Species directories Infrastructure (PESI) \tab pesi \tab TRUE \cr
#' Mycobank \tab myco \tab TRUE \cr
#' National Biodiversity Network (UK)	\tab nbn \tab FALSE \cr
#' Index Fungorum \tab fg \tab FALSE \cr
#' EU BON \tab eubon \tab FALSE \cr
#' Index of Names (ION) \tab ion \tab FALSE \cr
#' Open Tree of Life (TOL) \tab tol \tab FALSE \cr
#' World Register of Marine Species (WoRMS) \tab worms \tab FALSE \cr
#' NatureServe \tab natserv \tab FALSE \cr
#' }
#'
#' If the source above has a \code{TRUE} in the \code{SOAP?} column, it is not available
#' in this package. They are available from a different package called \strong{taxizesoap}.
#' See the GitHub repo for how to install \url{https://github.com/ropensci/taxizesoap}
#'
#' @importFrom graphics plot
#' @importFrom methods as is
#' @importFrom stats as.dist hclust na.omit setNames aggregate complete.cases
#' @importFrom crul HttpClient HttpRequest AsyncVaried upload
#' @importFrom zoo na.locf
#' @importFrom utils URLencode citation download.file read.delim write.table tail
#' @importFrom ape read.tree as.phylo.hclust plot.phylo
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom data.table rbindlist setDF transpose
#' @importFrom foreach foreach %do%
#' @importFrom stringr str_extract str_split str_replace str_replace_all
#' @importFrom plyr failwith rbind.fill llply ldply ddply l_ply summarise colwise .
#' @importFrom reshape2 melt dcast
#' @importFrom xml2 xml_text xml_find_first xml_find_all xml_children read_xml
#' xml_name xml_ns as_list
#' @name taxize-package
#' @aliases taxize
#' @docType package
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @author Eduard Szoecs \email{eduardszoecs@@gmail.com}
#' @author Zachary Foster \email{zacharyfoster1989@@gmail.com}
#' @author Carl Boettiger \email{cboettig@@gmail.com}
#' @author Karthik Ram \email{karthik@@ropensci.org}
#' @author Ignasi Bartomeus \email{nacho.bartomeus@@gmail.com}
#' @author John Baumgartner \email{johnbb@@student.unimelb.edu.au}
#' @author James O'Donnell \email{jodonnellbio@@gmail.com}
#' @keywords package
NULL

#' Lookup-table for IDs of taxonomic ranks
#'
#' data.frame of 43 rows, with 2 columns:
#' \itemize{
#'  \item rankid - a numeric rank id, consecutive
#'  \item ranks - a comma separated vector of names that are considered
#'  equal to one another within the row
#' }
#'
#' We use this data.frame to do data sorting/filtering based on the ordering
#' of ranks.
#'
#' Please let us know if there is a rank that occurs from one of the data
#' sources \pkg{taxize} that we don't have in \code{rank_ref} dataset.
#'
#' Let us know if you disagree with the ordering of ranks.
#'
#' @name rank_ref
#' @docType data
#' @keywords data
NULL

#' Lookup-table for family, genus, and species names for ThePlantList
#'
#' These names are from http://www.theplantlist.org, and are from
#' version 1.1 of their data. This data is used in the function
#' \code{\link{names_list}}. This is a randomly selected subset of the ~350K
#' accepted species names in Theplantlist.
#'
#' @format A data frame with 10,000 rows and 3 variables:
#' \describe{
#'   \item{family}{family name}
#'   \item{genus}{genus name}
#'   \item{species}{specific epithet name}
#' }
#' @source http://www.theplantlist.org
#' @name theplantlist
#' @docType data
#' @keywords data
NULL

#' MOBOT family names
#'
#' Family names and their replacements from the Angiosperm Phylogeny
#' Website system of flowering plant classification.
#'
#' This dataset is from Version 13, incorporated on 2015-04-29.
#'
#' @format A data frame with 1597 rows and 4 variables:
#' \describe{
#'   \item{original}{original data record from APG website}
#'   \item{this}{Order name}
#'   \item{that}{Replacement order name}
#'   \item{order}{Order name}
#' }
#' @source \url{http://www.mobot.org/MOBOT/research/APweb/}
#' @name apg_families
#' @docType data
#' @keywords data
NULL

#' MOBOT order names
#'
#' Order names and their replacements from the Angiosperm Phylogeny
#' Website system of flowering plant classification.
#'
#' This dataset is from Version 13, incorporated on 2015-04-29.
#'
#' @format A data frame with 494 rows and 3 variables:
#' \describe{
#'   \item{original}{original data record from APG website}
#'   \item{this}{Order name}
#'   \item{that}{Replacement order name}
#' }
#' @source \url{http://www.mobot.org/MOBOT/research/APweb/}
#' @name apg_orders
#' @docType data
#' @keywords data
NULL

#' Vector of plant species (genus - specific epithet) names from ThePlantList
#'
#' These names are from http://www.theplantlist.org, and are a
#' randomly chosen subset of names of the form genus/specific epithet
#' for the purpose of having some names to play with for examples in
#' this package.
#'
#' @format A vector of length 1182
#' @source http://www.theplantlist.org
#' @name plantNames
#' @docType data
#' @keywords data
NULL

#' Vector of plant genus names from ThePlantList
#'
#' These names are from http://www.theplantlist.org, and are a
#' randomly chosen subset of genera names for the purpose of having some
#' names to play with for examples in this package.
#'
#' @format A vector of length 793
#' @source http://www.theplantlist.org
#' @name plantGenusNames
#' @docType data
#' @keywords data
NULL

#' Defunct functions in taxize
#'
#' The following functions are now defunct (no longer available):
#'
#' \itemize{
#'  \item \code{\link{col_classification}}: See \code{\link{classification}}
#'  \item \code{\link{eol_hierarchy}}: See \code{\link{classification}}
#'  \item \code{\link{tp_classification}}: See \code{\link{classification}}
#'  \item \code{\link{tpl_search}}: Use the \pkg{Taxonstand} functions
#'  \code{TPL} or \code{TPLck} directly.
#'  \item \code{\link{get_seqs}}: This function changed name to \code{\link{ncbi_getbyname}}.
#'  \item \code{\link{get_genes}}: This function changed name to \code{\link{ncbi_getbyid}}.
#'  \item \code{\link{get_genes_avail}}: This function changed name to \code{\link{ncbi_search}}.
#'  \item \code{\link{ncbi_getbyname}}: See \code{ncbi_byname} in the \pkg{traits} package.
#'  \item \code{\link{ncbi_getbyid}}: See \code{ncbi_byid} in the \pkg{traits} package.
#'  \item \code{\link{ncbi_search}}: See \code{ncbi_searcher} in the \pkg{traits} package.
#'  \item \code{\link{eol_invasive}}: See \code{eol} in the \pkg{originr} package.
#'  \item \code{\link{gisd_isinvasive}}: See \code{gisd} in the \pkg{originr} package.
#'  \item \code{\link{ubio_classification}}:  The uBio web services was down for quite a while, is
#'  now (as of 2016-05-09) back up, but we don't trust that it will stay up and available.
#'  \item \code{\link{ubio_classification_search}}: The uBio web services was down for quite
#'  a while, is now (as of 2016-05-09) back up, but we don't trust that it will stay up
#'  and available.
#'  \item \code{\link{ubio_id}}: The uBio web services was down for quite a while, is
#'  now (as of 2016-05-09) back up, but we don't trust that it will stay up and available.
#'  \item \code{\link{ubio_ping}}:  The uBio web services was down for quite a while, is
#'  now (as of 2016-05-09) back up, but we don't trust that it will stay up and available.
#'  \item \code{\link{ubio_search}}:  The uBio web services was down for quite a while, is
#'  now (as of 2016-05-09) back up, but we don't trust that it will stay up and available.
#'  \item \code{\link{ubio_synonyms}}:  The uBio web services was down for quite a while, is
#'  now (as of 2016-05-09) back up, but we don't trust that it will stay up and available.
#'  \item \code{\link{get_ubioid}}: The uBio web services are apparently down indefinitely.
#'  \item \code{\link{phylomatic_tree}}: This function is defunct. See
#'  \code{phylomatic} in the package \pkg{brranching}
#'  \item \code{\link{phylomatic_format}}: This function is defunct. See
#'  \code{phylomatic_names} in the package \pkg{brranching}
#'  \item \code{\link{iucn_summary_id}}: This function is defunct. Use 
#'  \code{\link{iucn_summary}}
#' }
#'
#' @name taxize-defunct
#' @aliases defunct
NULL

#' Species names from Species Plantarum
#'
#' These names have been compiled from
#' \href{https://en.wikipedia.org/wiki/Species_Plantarum}{\emph{Species Plantarum}} by
#' \href{https://en.wikipedia.org/wiki/Carl_Linnaeus}{Carl Linnaeus} originally
#' published in 1753. It is the first work to consistently apply
#' \href{https://en.wikipedia.org/wiki/Binomial_nomenclature}{binomial names}
#' and was the starting point for the naming of plants. The book lists every
#' species of plant known at the time, classified into
#' \href{https://en.wikipedia.org/wiki/Genus}{genera}. The dataset provides a
#' useful reference point to see how taxonomic names have changed since their
#' inception. The names were transcribed by Robert W. Kiger.
#'
#' @format A data frame with 5940 rows and 3 variables: \describe{
#'   \item{genus}{First part of the binomial species name for each species
#'   within the \href{https://en.wikipedia.org/wiki/Genus}{genus}}
#'   \item{epithet}{specific epithet or second part of the binomial species name
#'   for each \href{https://en.wikipedia.org/wiki/Species}{species}}
#'   \item{page_number}{The following abbreviations sometimes are used in the
#'   page_number field.  \itemize{\item{"add."} {refers to addenda that appear
#'   on the unnumbered last page of the index in volume
#'   two.}\item{"err."}{refers to the unnumbered page of errata that appears
#'   following the index in volume two.}\item{"canc."}{following a page number
#'   indicates that the binomial appeared on the cancelled version of that page
#'   and does not appear on its replacement (as in the 1957–1959 facsimile
#'   edition).}}}}
#' @source \href{http://fmhibd.library.cmu.edu/HIBD-DB/Species/home.php}{Hunt
#'   Institute for Botanical Documentation}
#' @name species_plantarum_binomials
#' @references Linnaeus, C. 1753. Species Plantarum. 2 vols. Salvius, Stockholm.
#'   [Facsimile edition, 1957–1959, Ray Society, London.]
#' @author Carl Linnaeus
#' @docType data
#' @keywords data
NULL
