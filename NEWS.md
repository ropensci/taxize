taxize 0.6.2
===============

## MINOR IMPROVEMENTS

* `rankagg()` doesn't depend on `data.table` anymore (fixes issue with CRAN checks)
* Replaced `RCurl::base64Decode()` with `openssl::base64_decode()`, needed for
`ubio_*()` functions (#447)
* Importing only functions (via `importFrom`) used across all imports now (#446). 
In addition, `importFrom` for all non-base R pkgs, including `graphics`, `methods`,
`stats` and `utils` packages (#441)
* Fixes to prevent problems with httr v1, where you can't pass a zero length
list to the `query` parameter in `GET()`, but can pass `NULL` (#445)
* Fixes to all of the `gni_*()` functions, including code tidying, some 
DRYing out, and ability to pass in curl options (#444)

## BUG FIXES

* Fixed typo in `taxize_cite()`
* Fixed a bug in `classification()` where numeric IDs as input got 
converted to itis ids just because they were numeric. Fixed now. (#434)
* Catalogue of Life (COL) changed from using short numeric codes for taxa to 
long alphanumeric UUID type ids. This required fixing functions using COL 
web services (#435)


taxize 0.6.0
===============

## NEW FEATURES

* Added a method for Catalogue of Life for the `synonyms` function to get
name synonyms. (#430)
* Added datasets `apgFamilies` and `apgOrders`. (#418)
* `col_search()` gains parameters `response` to get a terse or full response, and
`...` to pass in curl options.
* `eol_dataobjects()` gains parameter `...` to pass in curl options, and parameter
`returntype` renamed to `asdf` (for "as data.frame").
* `ncb_get_taxon_summary()` gains parameter `...` to pass in curl options.
* The `children()` function gains the `rows` parameter passed on to `get_*()` functions,
supported for data sources ITIS and Catalogue of Life, but not for NCBI.
* The `upstream()` function gains the `rows` parameter passed on to `get_*()` functions,
supported for both data sources ITIS and Catalogue of Life.
* The `classification()` function gains the `rows` parameter passed on to `get_*()`
functions, for all sources used in the function.
* The `downstream()` function gains the `rows` parameter passed on to `get_*()`
functions, for all sources used in the function.
* Nearly all taxonomic ID retrieveal functions (i.e., `get_*()`) gain new parameters to
help filter results (e.g., `division`, `phylum`, `class`, `family`, `parent`, `rank`, etc.).
These parameters allow direct matching or regex filters (e.g., `.a` to match any character
followed by an `a`). (#410) (#385)
* Nearly all taxonomic ID retrieveal functions (i.e., `get_*()`) now give back more
information (mostly higher taxonomic data) to help in the interactive decision
process. (#327)
* New data source added to `synonyms()` function: Catalogue of Life. (#430)

## MINOR IMPROVEMENTS

* `vegan` package, used in `class2tree()` function, moved from Imports to Suggests. (#392)
* Improved `taxize_cite()` a lot - get URLs and sometimes citation information
for data sources available in taxize. (#270)
* Fixed typo in `apg_lookup()` function. (#422)
* Fixed documentation in `apg_families()` function. (#418)
* Across many functions, fixed support for passing in curl options, and added
examples of curl option use.
* `callopts` parameter in `eol_pages()`, `eol_search()`, `gnr_resolve()`,
`tp_accnames()`, `tp_dist()`, `tp_search()`, `tp_summary()`, `tp_synonyms()`,
`ubio_search()` changed to `...`
* `accepted` parameter in `get_tsn()` changed to `FALSE` by default. (#425)
* Default value of `db` parameter in `resolve()` changed to `gnr` as `tnrs` is
often quite slow.
* General code tidying across the package to make code easier to read.

## BUG FIXES

* Fixed encoding issues in `tpl_families()` and `tpl_get()`. (#424)

## DEPRECATED AND DEFUNCT

* The following functions that were deprecated are now defunct (no longer available):
`ncbi_getbyname()`, `ncbi_getbyid()`, `ncbi_search()`, `eol_invasive()`,
`gisd_isinvasive()`. These functions are available in the `traits` package. (#382)
* `phylomatic_tree()` is deprecated, but will be defunct in a upcoming version.

taxize 0.5.2
===============

## NEW FEATURES

* New set of functions to ping each of the APIs used in `taxize`. E.g., `itis_ping()` pings ITIS and returns a logical, indicating if the ITIS API is working or not. You can also do a very basic test to see whether content returned matches what's expected. (#394)
* New function `status_codes()` to get vector of HTTP status codes. (#394)

## MINOR IMPROVEMENTS

* Removed startup message.
* Now can pass in curl options to `itis_ping()`, and all `*_ping()` functions.

## BUG FIXES

* Moved examples that were in `\donttest` into `\dontrun`.

taxize 0.5.0
===============

## NEW FEATURES

* New function `genbank2uid()` to get a NCBI taxonomic id (i.e., a _uid_) from a either a GenBank accession number of GI number. (#375)
* New function `get_nbnid()` to get a UK National Biodiversity Network taxonomic id (i.e., a _nbnid_). (#332)
* New function `nbn_classification()` to get a taxonomic classification for a UK National Biodiversity Network taxonomic id. Using this new function, generic method `classification()` gains method for `nbnid`. (#332)
* New function `nbn_synonyms()` to get taxonomic synonyms for a UK National Biodiversity Network taxonomic id. Using this new function, generic method `synonyms()` gains method for `nbnid`. (#332)
* New function `nbn_search()` to search for taxa in the UK National Biodiversity Network. (#332)
* New function `ncbi_children()` to get direct taxonomic children for a NCBI taxonomic id. Using this new function, generic method `children()` gains method for `ncbi`. (#348) (#351) (#354)
* New function `upstream()` to get taxa upstream of a taxon. E.g., getting families upstream from a genus gets all families within the one level higher up taxonomic class than family. (#343)
* New suite of functions `as.*()` to coerce numeric/alphanumeric codes to taxonomic identifiers for various databases. There are methods on this function for each of itis, ncbi, tropicos, gbif, nbn, bold, col, eol, and ubio. By default `as.*()` funtions make a quick check that the identifier is a real one by making a GET request against the identifier URI - this can be toggle off by setting `check=FALSE`. There are methods for returning itself, character, numeric, list, and data.frame. In addition, if the `as.*.data.frame()` function is used, a generic method exists to coerce the `data.frame` back to a identifier object. (#362)
* New suite of functions named, for example, `get_tsn_()` (the underscore is the only different from the previous function name). These functions don't do the normal interactive process of prompts that e.g., `get_tsn()` do, but instead returned a list of all ids, or a subset via the `rows` parameter. (#237)
* New function `ncbi_get_taxon_summary()` to get taxonomic name and rank for 1 or more NCBI uid's. (#348)

## MINOR IMPROVEMENTS

* `assertthat` removed from package imports, replaced with `stopifnot()`, to reduce dependency load. (#387)
* `eol_hierarchy()` now defunct (no longer available) (#228) (#381)
* `tp_classifcation()` now defunct (no longer available) (#228) (#381)
* `col_classification()` now defunct (no longer available) (#228) (#381)
* New manual page listing all the low level ITIS functions for which their manual pages are not shown in the package index, but are available if you to `?fxn-name`.
* All `get_*()` functions gain a new parameter `rows` to allow selection of particular rows. For example, `rows=1` to select the first row, or `rows=1:3` to select rows 1 through 3. (#347)
* `classification()` now by default returns taxonomic identifiers for each of the names. This can be toggled off by the `return_id=FALSE`. (#359) (#360)
* Simplification of many higher level functions to use `switch()` on the `db` parameter, which helps give better error message when a `db` value is not possible or spelled incorrectly. (#379)

## BUG FIXES

* Lots of reduction of redundancy in internal functions. (#378)

taxize 0.4.0
===============

## NEW FEATURES

* New data sources added to taxize: BOLD (Biodiversity of Life Database). Three more data sources were added (World Register of Marine Species (WoRMS), Pan-European Species directories Infrastructure (PESI), and Mycobank), but are not available on CRAN. Those three data sources provide data via SOAP web services protocol, which is hard to support in R. Thus, those sources are available on Github. See https://github.com/ropensci/taxize#version-with-soap-data-sources
* New function `children()`, which is a single interface to various data sources to get immediate children from a given taxonomic name. (#304)
* New functions added to search BOLD data" `bold_search()` that searches for taxa in the BOLD database of barcode data; `get_boldid()` to search for a BOLD taxon identifier. (#301)
* New function `get_ubioid()` to get a uBio taxon identifier. (#318)
* New function started (not complete yet) to get suggested citations for the various data sources available in `taxize`: `taxize_cite()`. (#270)

## MINOR IMPROVEMENTS

* Using `jsonlite` instead of `RJSONIO` throughout the `taxize`.
* `get_ids()` gains new option to search for a uBio ID, in addition to the others, itis, ncbi, eol, col, tropicos, and gbif.
* Fixed documentation for `stripauthority` parameter `gnr_resolve()`. (#325)
* `iplant_resolve()` now outputs data.frame structure instead of a list. (#306)
* Clarified parameter `seqrange` in `ncbi_getbyname()` and `ncbi_search()` (#328)
* `synonyms()` gains new data source, can now get synonyms from uBio data source (#319)
* `vascan_search()` giving back more useful results now.

## BUG FIXES

* Added error catching for when URI is too long, i.e., when too many names provided (#329) (#330)
* Various fixes to `tnrs()` function, including more meaningful error messages on failures (#323) (#331)
* Fixed bug in `getpublicationsfromtsn()` that caused function to fail on data.frame's with no data on name assignment (#297)
* Fixed bug in `sci2comm()` that caused fxn to fail when using `db=itis` sometimes (#293)
* Fixes to `scrapenames()`. Sending a text blob via the `text` parameter now works.
* Fixes to `resolve()` so that function now works for all 3 data sources. (#337)

taxize 0.3.0
===============

## NEW FEATURES

* New function `iplant_resolve()` to do name resolution using the iPlant name resolution service. Note, this is different from http://taxosaurus.org/ that is wrapped in the `tnrs()` function.
* New function `ipni_search()` to search for names in the International Plant Names Index (IPNI).
* New function `resolve()` that unifies name resolution services from iPlant's name resolution service (via `iplant_resolve()`), Taxosaurus' TNRS (via `tnrs()`), and GNR's name resolution service (via `gnr_resolve()`).
* All `get_*()` functions how returning a new _uri_ attribute that is a link to the taxon on on the web. If NA is given back (e.g. nothing found), the uri attribute is blank. You can go directly to the uri in your default browser by doing, for example: `browseURL(attr(result, "uri"))`.
* `get_eolid()` now returns an attribute _provider_ because EOL collates taxonomic data form a lot of sources, then gives back IDs that are internal EOL ids, not those matching the id of the source they pull from. This should help with provenance, and should help if there is confusion about why the id givenb back by this function does not match that from the original source.
* Within the `get_tsn()` function, now using the function `itis_terms()`, which gives back the accepted status of the taxa. This allows a new parameter in the function (`accepted`, logical) that allows user to say give back only accepted status names (`accepted=TRUE`), or to give back all names (`accepted=FALSE`).
* `gnr_resolve()` gains two new parameters `best_match_only` (logical, to return best match only) and `preferred_data_sources` (to return preferred data sources) and `callopts` to pass in curl options.
* `tnrs()`, `tp_accnames()`, `tp_refs()`, `tp_summary()`, and `tp_synonyms()` gain new parameter `callopts` to pass in curl options.

## MINOR IMPROVEMENTS

* `class2tree()` can now handle NA in classification objects.
* `classification.eolid()` and `classification.colid()` now return the submitted name along with the classification.
* Changed from CC0 to MIT license.
* Updated citation to have both the taxize paper in F1000 Research and the package citation.
* Sped up some functions by removing internal use of `plyr` functions, see #275.
* Removed dependency on rgbif - copied into this package a few functions needed internally. This avoids users having to install GDAL binary.
* Added in `verbose` parameter to many more functions to allow suppression of help messages.
* In most functions when using `httr`, now manually parsing JSON to a list then to another data format instead of allowing internal `httr` parsing - in addition added checks on content type and encoding in many functions.
* Added `match.arg` iternally to `get_ids()` for the `db` parameter so that a) unique short abbreviations of possible values are possible, and b) gives a meaningful warning if unsupported values are given.
* Most long-named ITIS functions (e.g., `getexpertsfromtsn`, `getgeographicdivisionsfromtsn`) gain parameter `curlopts` to pass in curl options.
* Added `stringsAsFactors=FALSE` to all `data.frame` creations to eliminate factor variables.

## BUG FIXES

* `classification.gbifid()` did not return the correct result when taxon not found.
* Fixed bugs in many functions, see #245, #248, #254, #277.
* `classification()` used to fail when it was passed a subset of a vector of ids, in which case the class information was stripped off. Now works (#284)

taxize 0.2.2
===============

## NEW FEATURES

* itis_downstream() and col_downstream() functions accessible now from a single function downstream() (https://github.com/ropensci/taxize/issues/238)

## MINOR IMPROVEMENTS

* Added a extension function classification() for the gbif id class, classification.gbifid() (https://github.com/ropensci/taxize/issues/241)

## BUG FIXES

* Added some error catching to class2tree function. (https://github.com/ropensci/taxize/issues/240)
* Fixed problems in cbind.classification() and rbind.classification() where the first column of the ouput was a useless column name, and all column names now lower case for consistency. (https://github.com/ropensci/taxize/issues/243)
* classification() was giving back IDS instead of taxon names on the list element names, fixed this so hopefully all are giving back names. (https://github.com/ropensci/taxize/issues/243)
* Fixed bugs in col_*() functions so they give back data.frame's now with character class columns instead of factors, damned stringsAsFactors!  (https://github.com/ropensci/taxize/issues/246)


taxize 0.2.0
===============

## MINOR IMPROVEMENTS

* New dataset: Lookup-table for family, genus, and species names for ThePlantList under dataset name "theplantlist".
* get_ids() now accepts "gbif" as an option via use of get_gbifid().
* Changed function itis_phymat_format() to phylomatic_format() - this function gets the typical Phylomatic format name string "family/genus/genus_epithet"

## BUG FIXES

* Updated gbif_parse() base url to the new one (http://api.gbif.org/v1/parser/name).
* Fixes to phylomatic_tree().

## NEW FEATURES

* New function class2tree() to convert list of classifications to a tree. For example, go from a list of classifications from the function classification() to this function to get a taxonomy tree in ape phylo format.
* New function get_gbfid() to get a Global Biodiversity Information Facility identifier. This is the ID GBIF uses in their backbone taxonomy.
* classification() outputs gain rbind() and cbind() generic methods that act on the various outputs of classification() to bind data width-wise, or column-wise, respectively.

taxize 0.1.9
===============

## MINOR IMPROVEMENTS

* Updated ncbi_search() to retrieve more than a max of 500, slightly changed column headers in output data files, and if didn't before, now accepts a vector/list of taxonomic names instead of just one name.

taxize 0.1.8
===============

## NEW FEATURES

* We attempted to make all ouput column names lowercase, and to increase consistency across column names in outputs from similar functions.
* New function scrapenames() uses the Global Names Recognition and Discovery service to extract taxonomic names from a web page, pdf, or other document.
* New function vascan_search() to search the CANADENSYS Vascan names database.

## BUG FIXES

* Fixed bugs in get_tpsid(), get_eolid() and eol_pages().
* phylomatic_tree() bugs fixed.

## MINOR IMPROVEMENTS

* classification() methods were simplified. Now classification() is the workhorse for every data-source. col_classification(), eol_hierarchy(), and tp_classification() are now deprecated and will be removed in the next taxize version.
* classification() gains four new arguments: start, checklist, key, and callopts.
* comm2sci() gains argument simplify to optionally simplify output to a vector of names (TRUE by default).
* get_eolid() and get_tpsid() both gain new arguments key to specify an API key, and ... to pass on arguments to eol_search().
* Added ncbi as a data source (db="ncbi") in sci2comm().
* tax_agg() now accepts a matrix in addition to a data.frame. Thanks to @tpoi
* tnrs() changes: Using httr instead of RCurl; now forcing splitting up name vector when long. Still issues when using POST requests (getpost="POST") wherein a request sent with 100 names only returns 30 for example. Investigating this now.

## NOTES

* Function name change: tp_acceptednames() now tp_accnames().
* Function name change: tp_namedistributions() now tp_dist().
* Function name change: tp_namereferences() now tp_refs().
* Internal ldfast() function changed name to taxize_ldfast() to avoid namespace conflicts with similar function in another package.
* Three functions now with ncbi_* prefix: get_seqs() is now ncbi_getbyname(); get_genes() is now ncbi_getbyid(); and get_genes_avail() is now ncbi_search().

taxize 0.1.5
===============

## NEW FEATURES

* classification() gains extension method classification.ids() to accept output from get_ids() - which attempts to get a taxonomic hierarchy from each of the taxon identifiers with the output from get_ids().
* synonyms() gains extension method synonyms.ids() to accept output from get_ids() - which attempts to get synonyms from each of the taxon identifiers with the output from get_ids().

taxize 0.1.4
===============

## NEW FEATURES

* Reworked functions that interact with the ITIS API so that lower level functions were grouped together into higher level functions. All the approximately 50 lower level functions are still exported but are not included in the index help file (due to @keywords internal for each fxn) - but can still be used normally, and man files are avaialable at ?functionName.
* New function itis_ping() to check if the ITIS API service is up, similar to eol_ping() for the EOL API.
* New function itis_getrecord() to get a partial or full record, using a TSN or lsid.
* New function itis_refs() to get references associated with a TSN.
* New function itis_kingdomnames() to get all kingdom names, or kingdom name for a TSN.
* New function itis_lsid() to get a TSN from an lsid, get a partial or full record from an lsid.
* New function itis_native() to get status as native, exotic, etc. in various geographic regions.
* New function itis_hierarchy() to get full hierarchy, or immediate up or downstream hierarchy.
* New function itis_terms() to get tsn's, authors, common names, and scientific names from a given query.
* New function sci2comm() to get common (vernacular) names from input scientific names from various data sources.
* New function comm2sci() to get scientific names from input common (vernacular) names from various data sources.
* New function get_ids() to get taxonomic identifiers across all sources.

## MINOR IMPROVEMENTS

* itis_taxrank() now outputs a character, not a factor; loses parameter verbose, and gains ..., which passes on further arguments to gettaxonomicranknamefromtsn.
* tp_synonyms(), tp_summary(), plantminer(), itis_downstream(), gisd_isinvasive(), get_genes_avail(), get_genes(), eol_invasive(), eol_dataobjects(), andn tnrs() gain parameter verbose to optionally suppress messages.
* phylomatic_tree() format changed so that names are passed in normall (e.g., Poa annua) instead of the slashpath format (family/genus/genus_species). Also, taxaformat parameter dropped.
* itis_acceptname() gains ... to pass in further arguments to getacceptednamesfromtsn()
* tp_namedistributions() loses parameter format.
* get_tsn() and get_uid() return infomation about match as attribute.
* clarified iucn-documentation

## BUG	FIXES

* Fixed bug in synonyms() so that further arguments can be passed on to get_tsn() to suppress messages.
* Removed test for ubio_classification_search(), a function that isn't operational yet.

taxize 0.1.1
===============

## NEW FEATURES

* New functions added just like get_uid()/get_tsn() but for EOL, Catalogue of Life, and Tropicos, see get_eolid(), get_colid(), and get_tpsid(), respectively.
* classification() methods added for EOL, Catalogue of Life, and Tropicos, see functions classification.eolid(), classification.colid(), and classification.tpsid() respectively.
* New function col_search() to search for names in the Catalogue of Life.
* User can turn off interactive mode in get_* functions. All get_* functions gain an ask argument, if TRUE (default) a user prompt is used for user to select which row they want, if FALSE, NA is returned when many results available; and added tests for the new argument. Affects downstream functions too.
* New function eol_invasive() to search EOL collections of invasive species lists.
* New function tp_search() to search for a taxonomic IDs from Tropicos.
* New function tp_classification() to get a taxonomic hierarchy from Tropicos.
* New function gbif_parse() to parse scientific names into their components, using the GBIF name parser API.
* New function itis_searchcommon() to search for common names across both searchbycommonnamebeginswith, and searchbycommonnameendswith.


## BUG FIXES

* tax_name() and other function broke, because get_tsn() and get_uid() returned wrong value when a taxon was not found. Fixed.


## MINOR IMPROVEMENTS

* Added tests for new classification() methods for EOL, COL, and Tropicos.
* Added tests for new functions tp_search() and tp_classification().

## NOTES

* Moved tests from inst/tests to tests/testthat according to new preferred location of tests.
* Updated CITATION in inst/ with our F1000Research paper info.
* Package repo name on Github changed from taxize_ to taxize - remember to use "taxize" in install_github() calls now instead of "taxize_"


taxize 0.1.0
===============

## NEW FEATURES

* New function tpl_families() to get data.frame of families from The Plantlist.org site.
* New function names_list() to get a random vector of species names using the
* Added two new data sets, plantGenusNames.RData and plantNames.RData, to be used in names_list().
* New function ldfast(), a replacement function for plyr::ldply that should be faster in all cases.
* Changed API key names to be more consistent, now tropicosApiKey, eolApiKey, ubioApiKey, and pmApiKey - do change these in your .Rprofile if you store them there.
* Added a startup message.

## MINOR IMPROVEMENTS

* Across most functions, removed dependencies on plyr, using ldfast() instead, for increased speed.
* Across most functions, changed from using RCurl to using httr.
* Across most functions, stop_for_status() now used directly after Curl call to check the http status code, stoping the function if appropriate code found.
* Many functions changed parameter ... to callopts, which passes on additional Curl options, with default an empty list (list()), which makes function testing easier.
* eol_search() gains parameters page, exact, filter_tid, filter_heid, filter_by_string, matching, cache_ttl, and callopts.
* eol_hierarchy() gains parameter callopts, and loses parameter usekey (always using API key now).
* eol_pages() gains parameters images, videos, sounds, maps, text, subject, licenses, details, common_names, synonyms, references, vetted, cache_ttl, and callopts.
* gni_search(): parameter url lost, is defined inside the function now, and .Rd file gains url references.
* phylomatic_tree() now checks to make sure family names were found for input taxa. If not, the function stops with message informing this.
* tpl_get() updated with fixes/improvements by John Baumgartner - now gets taxa from all groups, whereas only retrieved from Angiosperms before. In addition, csv files from The Plantlist.org are downloaded directly rather than read into R and written out again.
* tpl_search() now checks for missing data or errors, and stops function with error message.

## BUG FIXES

* capwords() fxn changed to taxize_capwords() to avoid namespace conflicts with other packages with a similar function.
* ubio_namebank() was giving back base64 encoded data, now decoded appropriately.

## NOTES

* Added John Baumgartner as an author.

taxize 0.0.6
===============

## NEW FEATURES
* tax_name() accepts multiple ranks to query.
* tax_name() accepts vectors as input.
* tax_name() has an option to query both, NCBI and ITIS, in one call and return the union of both.
* new extractor function for iucn_summary(): iucn_status(), to extract status from iucn-objects.
* tax_agg(): A function to aggregate species data to given taxonomic rank.
* tax_rank(): Get taxonomic rank for a given taxon name.
* classification() accepts taxon names as input and returns a named list.
* new function apg_lookup() looks up APGIII taxonomy and replaces family names
* new function gni_parse() parses scientific names using EOl's name parser API
* new function iucn_getname() is a utility to find IUCN names using the EOL API
* new function rank_agg() aggregates data by a given taxonomic rank
* new data table apg_families
* new data table apg_orders
* gnr_resolve() gains new arguments gnr_resolvee_once, with_context, stripauthority, highestscore, and http, and loses returndf (that is, a data.frame is returned by default)
* gni_search() gains parameter parse_names

## MINOR IMPROVEMENTS
* tnrs() parameter getpost changed from default of 'GET' to 'POST'
* Across all functions, the url parameter specifying an API endpoint was moved inside of functions (i.e., not available as a parameter in the function call)
* gnr_datasources() parameter todf=TRUE by default now, returning a data.frame
* col_classification() minor formatting improvements

## BUG FIXES
* iucn_summary() returns no information about population estimates.
* get_tsn() raised a warning in specific situations.
* tax_name() did not work for multiple ranks with ITIS.
* fixed errors in getfullhierarchyfromtsn()
* fixed errors in gethierarchydownfromtsn()
* fixed errors in getsynonymnamesfromtsn()
* fixed errors in searchforanymatch()
* fixed errors in searchforanymatchedpage()

## NOTES
* Removed dependency to NCBI2R
* Improvements of documentation
* Citation added

taxize 0.0.5
===============

## BUG FIXES
* removed tests for now until longer term fix is made so that web APIs that are temporarily down don't cause tests to fail.

taxize 0.0.4
===============

## BUG FIXES
* added R (>= 2.15.0) so that package tests don't fail on some systems due to paste0()
* remove test for ubio_namebank() function as it sometimes fails

taxize 0.0.3
===============

## BUG FIXES
* iucn_summary() does not break when API returns no information.
* tax_name() returns NA when taxon is not found on API.
* get_uid() asks for user input when more then one UID is found for a taxon.
* changed base URL for phylomatic_tree(), and associated parameter changes

## NEW FEATURES
* added check for invasive species status for a set of species from GISD database via gisd_isinvasive().
* Further development with the EOL-API: eol_dataobjects().
* added Catalogue of Life: col_classification(), col_children(), and col_downstream().
* new fxn get_genes(), retrieve gene sequences from NCBI by accession number.
* new functions to interact with the Phylotastic name resolution service: tnrs_sources() and tnrs()
* Added unit tests

## DEPRECATED AND DEFUNCT
* itis_name() fxn deprecated - use tax_name() instead


taxize 0.0.2
===============

## BUG FIXES

* changed paste0 to paste to avoid problems on certain platforms.
* removed all tests until the next version so that tests will not fail on any platforms.
* plyr was missing as import for iucn_summary fxn.

## NEW FEATURES

* added NEWS file.


taxize 0.0.1
===============

## NEW FEATURES

* released to CRAN
