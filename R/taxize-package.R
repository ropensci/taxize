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
#' separated by an underscore. They follow the format of `service_whatitdoes`.
#' For example, `gnr_resolve` uses the Global Names Resolver API to resolve
#' species names.
#'
#' General functions in the package that don't hit a specific API don't have
#' two words separated by an underscore, e.g., `classification`
#'
#' You need API keys for some data sources. See [taxize-authentication]
#' for more information.
#'
#' @section Currently supported APIs:
#'
#' \tabular{llc}{
#' API \tab prefix \tab SOAP? \cr
#' Encyclopedia of Life (EOL)  \tab eol \tab FALSE \cr
#' Integrated Taxonomic Information Service (ITIS)  \tab itis \tab FALSE \cr
#' Global Names Resolver (from EOL/GBIF) \tab gnr \tab FALSE \cr
#' Global Names Index (from EOL/GBIF) \tab gni \tab FALSE \cr
#' IUCN Red List \tab iucn \tab FALSE \cr
#' Tropicos (from Missouri Botanical Garden) \tab tp \tab FALSE \cr
#' Theplantlist.org \tab tpl \tab FALSE \cr
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
#' If the source above has a `TRUE` in the `SOAP?` column, it is not available
#' in this package. They are available from a different package called **taxizesoap**.
#' See the GitHub repo for how to install https://github.com/ropensci/taxizesoap
#' 
#' @section Catalogue of Life (COL):
#' COL introduced rate limiting recently in 2019 - which has made the API
#' essentially unusable - CoL+ is coming soon and we'll incorporate it here
#' when it's stable. See https://github.com/ropensci/colpluz for the
#' R implementation for CoL+
#'
#' @importFrom graphics plot
#' @importFrom methods as is
#' @importFrom stats as.dist hclust na.omit setNames aggregate complete.cases
#' @importFrom crul HttpClient HttpRequest AsyncVaried upload
#' @importFrom utils URLencode citation download.file read.delim write.table tail
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom data.table rbindlist setDF transpose melt dcast as.data.table
#' @importFrom xml2 xml_text xml_find_first xml_find_all xml_children read_xml
#' xml_name xml_ns as_list
#' @importFrom R6 R6Class
#' @importFrom crayon style
#' @importFrom cli symbol cat_line rule
#' @importFrom conditionz ConditionKeeper
#' @importFrom hoardr hoard
#' @importFrom vctrs field new_rcrd vec_assert vec_cast vec_default_cast
#' vec_recycle vec_recycle_common
#' @importFrom taxa2 db_ref tax_db tax_id tax_name tax_rank taxon
#' taxon_authority taxon_id taxon_rank
#' @name taxize-package
#' @aliases taxize
#' @docType package
#' @author Scott Chamberlain 
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
#' data.frame of 46 rows, with 2 columns:
#' * rankid - a numeric rank id, consecutive
#' * ranks - a comma separated vector of names that are considered
#'  equal to one another within the row
#'
#' We use this data.frame to do data sorting/filtering based on the ordering
#' of ranks.
#'
#' Please let us know if there is a rank that occurs from one of the data
#' sources \pkg{taxize} that we don't have in `rank_ref` dataset.
#'
#' Let us know if you disagree with the ordering of ranks.
#' 
#' Note that `rankid` 280 are essentially "genetic variants"; placed just above
#' 'unspecified' to denote they're not without rank, but they're not
#' really taxonomic ranks either. As far as I know there's no way 
#' to delineate among these "genetic variant" types.
#'
#' @name rank_ref
#' @docType data
#' @keywords data
NULL

#' Lookup-table for IDs of taxonomic ranks (WoRMS)
#'
#' Same as `rank_ref` but specifically for WoRMS, where section/subsection
#' ranks are put between family/order rather than between species/genus.
#'
#' @name rank_ref_zoo
#' @docType data
#' @keywords data
NULL

#' Lookup-table for family, genus, and species names for ThePlantList
#'
#' These names are from http://www.theplantlist.org, and are from
#' version 1.1 of their data. This data is used in the function
#' [names_list()]. This is a randomly selected subset of the ~350K
#' accepted species names in Theplantlist.
#'
#' @format A data frame with 10,000 rows and 3 variables:
#' 
#'   * `family` family name
#'   * `genus` genus name
#'   * `species` specific epithet name
#' 
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
#' This dataset is from Version 14, incorporated on 2020-06-03, 
#' generated using [apgFamilies()]
#' 
#' (update script in inst/ignore/apg_script.R)
#'
#' @format A data frame with 1705 rows and 6 variables:
#' 
#' * `family`: family name
#' * `synonym`: if `accepted=FALSE`, this is the accepted name;
#' if `accepted=TRUE`, this is `NA`, and the name in `family` is accepted
#' * `order`: order name for the family
#' * `accepted`: logical, if name in `family` column is accepted or not
#' * `original`: original data record from APG website, mapping
#' name in `family` column to a new name, if there is one
#' * `accepted_name`: accepted name. accepted names, combining those that
#' are accepted from `family` column, with the new name from `synonym`
#' if applicable
#' 
#' @source http://www.mobot.org/MOBOT/research/APweb/
#' @name apg_families
#' @docType data
#' @keywords data
NULL

#' MOBOT order names
#'
#' Order names and their replacements from the Angiosperm Phylogeny
#' Website system of flowering plant classification.
#'
#' This dataset is from Version 14, incorporated on 2020-06-03, 
#' generated using [apgOrders()]
#' 
#' (update script in inst/ignore/apg_script.R)
#'
#' @format A data frame with 576 rows and 5 variables:
#' 
#' * `order`: order name
#' * `synonym`: if `accepted=FALSE`, this is the accepted name;
#' if `accepted=TRUE`, this is `NA`, and the name in `order` is accepted
#' * `accepted`: logical, if name in `order` column is accepted or not
#' * `original`: original data record from APG website, mapping
#' name in `order` column to a new name, if there is one
#' * `accepted_name`: accepted name. accepted names, combining those that
#' are accepted from `order` column, with the new name from `synonym`
#' if applicable
#' 
#' @source http://www.mobot.org/MOBOT/research/APweb/
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

#' WORMS ranks
#'
#' Created using `worrms::wm_ranks_id(-1)` on 2020-02-11.
#'
#' Present in taxize in the case where WORMS does not
#' return rank names - with this dataset we can fill
#' in rank information as long as rank ids are returned
#'
#' @format A data frame with 216 rows and 2 variables:
#'
#'   * `id`: rank id
#'   * `rank`: rank name
#'
#' @name worrms_ranks
#' @docType data
#' @keywords data
NULL

#' Defunct functions in taxize
#'
#' The following functions are now defunct (no longer available):
#' * All COL functions are defunct: `as.colid, `col_children`,
#' `col_classification`, `col_downstream`, `col_search`, `get_colid`,
#' `get_colid_`, `as.data.frame.colid`, `children.colid`,
#' `classification.colid`, `downstream.colid`, `id2name.colid`,
#' `lowest_common.colid`, `synonyms.colid`, `upstream.colid`
#' * `col_classification()`: See`classification()`
#' * `tp_classification()`: See`classification()`
#' * `eol_hierarchy()`: See`classification()`
#' * [eol_invasive()]: See `eol` in the \pkg{originr} package.
#' * [use_eol()]: EOL no longer requires an API key
#' * [tpl_search()]: Use the \pkg{Taxonstand} functions `TPL` or `TPLck` directly.
#' * [get_seqs()]: This function changed name to`ncbi_getbyname()`()].
#' * [get_genes()]: This function changed name to`ncbi_getbyid()`()].
#' * [get_genes_avail()]: This function changed name to`ncbi_search()`()].
#' * [ncbi_getbyname()]: See `ncbi_byname` in the \pkg{traits} package.
#' * [ncbi_getbyid()]: See `ncbi_byid` in the \pkg{traits} package.
#' * [ncbi_search()]: See `ncbi_searcher` in the \pkg{traits} package.
#' * [gisd_isinvasive()]: See `gisd` in the \pkg{originr} package.
#' * [ubio_classification()]:  The uBio web services was down for quite a while, is
#'  now (as of 2016-05-09) back up, but we don't trust that it will stay up and available.
#' * [ubio_classification_search()]: The uBio web services was down for quite
#'  a while, is now (as of 2016-05-09) back up, but we don't trust that it will stay up
#'  and available.
#' * [ubio_id()]: The uBio web services was down for quite a while, is
#'  now (as of 2016-05-09) back up, but we don't trust that it will stay up and available.
#' * [ubio_ping()]:  The uBio web services was down for quite a while, is
#'  now (as of 2016-05-09) back up, but we don't trust that it will stay up and available.
#' * [ubio_search()]:  The uBio web services was down for quite a while, is
#'  now (as of 2016-05-09) back up, but we don't trust that it will stay up and available.
#' * [ubio_synonyms()]:  The uBio web services was down for quite a while, is
#'  now (as of 2016-05-09) back up, but we don't trust that it will stay up and available.
#' * [get_ubioid()]: The uBio web services are apparently down indefinitely.
#' * [phylomatic_tree()]: This function is defunct. See
#'  `phylomatic` in the package \pkg{brranching}
#' * [phylomatic_format()]: This function is defunct. See
#'  `phylomatic_names` in the package \pkg{brranching}
#' * [iucn_summary_id()]: This function is defunct. Use [iucn_summary()]
#' * [eubon()]: This function is defunct. Use [eubon_search()]
#' * [tnrs()]: This function is defunct. Was too unreliable
#' * [tnrs_sources()]: This function is defunct. Was too unreliable
#'
#' @name taxize-defunct
#' @aliases defunct
NULL

#' Deprecated functions in taxize
#'
#' The following functions are now deprecated:
#' 
#' * [get_tsn()]: use [get_itis()]
#' * [get_uid()]: use [get_ncbi()]
#'
#' @name taxize-deprecated
#' @aliases deprecated
NULL

#' Species names from Species Plantarum
#'
#' These names have been compiled from
#' [*Species Plantarum*](https://en.wikipedia.org/wiki/Species_Plantarum) by
#' [Carl Linnaeus](https://en.wikipedia.org/wiki/Carl_Linnaeus) originally
#' published in 1753. It is the first work to consistently apply
#' [binomial names](https://en.wikipedia.org/wiki/Binomial_nomenclature)
#' and was the starting point for the naming of plants. The book lists every
#' species of plant known at the time, classified into
#' [genera](https://en.wikipedia.org/wiki/Genus). The dataset provides a
#' useful reference point to see how taxonomic names have changed since their
#' inception. The names were transcribed by Robert W. Kiger.
#'
#' @format A data frame with 5940 rows and 3 variables:
#' 
#' - genus First part of the binomial species name for each species
#' within the [genus](https://en.wikipedia.org/wiki/Genus)
#' 
#' - epithet specific epithet or second part of the binomial species name
#'   for each [species](https://en.wikipedia.org/wiki/Species)
#' 
#' - page_number The following abbreviations sometimes are used in the
#'   page_number field.  
#'     - "add." refers to addenda that appear on the unnumbered last page of
#'       the index in volume two.
#'     - "err." refers to the unnumbered page of errata that appears following
#'       the index in volume two.
#'     - "canc." following a page number indicates that the binomial appeared
#'       on the cancelled version of that page and does not appear on its
#'       replacement (as in the 1957-1959 facsimile edition.
#' 
#' @source [Hunt Institute for Botanical Documentation](http://fmhibd.library.cmu.edu/HIBD-DB/Species/home.php)
#' @name species_plantarum_binomials
#' @references Linnaeus, C. 1753. Species Plantarum. 2 vols. Salvius, Stockholm.
#'   \[Facsimile edition, 1957-1959, Ray Society, London.\]
#' @author Carl Linnaeus
#' @docType data
#' @keywords data
NULL
