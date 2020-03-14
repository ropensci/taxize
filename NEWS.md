taxize 0.9.92
=============

### DEFUNCT

COL introduced rate limiting recently in 2019 - which has made the API
essentially unusable - CoL+ is coming soon and we'll incorporate it here when it's stable. see https://github.com/ropensci/colpluz for the in development R client (#796)

### NEW FEATURES

* gains new function `gn_parse()` to access the Global Names scientific name parser. it's a super fast parser. see the section on name parsers (https://docs.ropensci.org/taxize/reference/index.html#section-name-parsers) for the 3 functions that do name parsing  (#794)
* dropped packages from imports: reshape2, stringr, plyr (#795)
* `get_wormsid()` gains two new parameters: `fuzzy` and `marine_only`; both are passed through to `worrms::wm_records_name()`/`worrms::wm_records_name()` (#790)

### MINOR IMPROVEMENTS

* no longer running taxon state examples on check (#791)
* vignettes have names now in the pkg docs site (#772)
* update docs for new roxygen2 version that suppoprts R6 (#793)
* gains dataset `worrms_ranks` to apply rank names in cases where WORMS fails to return rank names in their data
* remove a `get_tpsid()` example that passes in names as factors; `get_*` functions no longer accept factors

### BUG FIXES

* fix to `classification.tpsid()`: change to an internal fxn changed its output; fix for that (#797)
* fix `get_boldid()`: when filtering (e.g., w/ `rank`, `division`, `parent`) returned no match, `get_boldid` was failing on downstream parsing; return NA now
* fix `get_wormsid_()`: was missing `marine_only` and `fuzzy` parameters
* fix `pow_search()`: an if statement was leading to length > 1 booleans
* fix `synonyms()`: an if statement in internal fxn `process_syn_ids` was leading to length > 1 booleans
* fix `classification.gbifid`: select columns only if they exist instead of failing on plucking non-existtent columns


taxize 0.9.91
=============

### NEW FEATURES

* `get_ids()` gains a new parameter `suppress` (default:`FALSE`) to toggle pakage `cli` messages stating which database is being worked on (#719)

### MINOR IMPROVEMENTS

* the following datasets are now available when the package is not loaded, so functions that use these datasets can now be called with package namespace like `taxize::downstream()`: `rank_ref`, `theplantlist`, `apg_families`, `apg_orders`  (#777) (#781)
* add new documentation site url (https://docs.ropensci.org/taxize/) to DESCRITPION file (#774) (@jeroen)
* fix links in README to issues label of new potential data sources (#782) (@katrinleinweber)
* more or less all functions that take as input the output of `get_*` functions have S3 methods that dispatch on those `get_*` output classes. however, you can still pass in a `db` parameter, which is IGNORED when dispatching on the input class. the `db` parameter is used (not ignored) when passing in a taxon id as character/numeric/etc. now these functions (children, classification, comm2sci, sci2comm, downstream, id2name, synonyms, upstream) warn when the user passes a `db` value which will be ignored (#780)
* The NCBI Entrez API often throws errors for users of this and other packages related to HTTP version used by the client; we now hard code the http version to HTTP/1.1 via the curl option `http_version=2L` across all Entrez requests (#783)

### BUG FIXES

* fixes to `col_search()`: COL now does rate limiting (if you make too many requests within a time period they will stop allowing requests from your IP address/your computer); documented rate limiting, what I know at least; changed `checklist` parameter behavior: years 2014 and back dont provide JSON, so we return `xml_document` objects now for those years that the user can parse themselves (#786)
* `tax_rank` somehow (my bad) had two `.default` methods. previous behavior is the same as current behavior (this version) (#784)
* fix `ncbi_children()`: fixed regex that was supposed to flag ambiguous taxa only, it was supposed to flag `sp.` and `spp.`, but was including `subsp.`, which we didn't want included (#777) (#781)
* another fix to `ncbi_children()`: when ID is passed rather than a name, we need to then set `id=NULL` after switching to the equivalent taxononmic name internally to avoid getting duplicate data back (#777) (#781)


taxize 0.9.9
============

### NEW FEATURES

* update all EUBON functions to use their new API version; `eubon_search()` gains new params `limit` and `page`; other eubon functions have no pagination  (#766)
* change base url in `ipni_search()` from http to https, via (#773)

### MINOR IMPROVEMENTS

* change `synonyms()` to always return `NA` for name not found, and always return a zero row data.frame when name found BUT no synonyms found; updated docs to indicate better what's returned (#763) (#765)
* COL sometimes returns control characters in the XML payload; these can't be parsed by the `xml2` package, so we have to remove them using regex; we throw a message when we're doing this so the user knows (#768)
* docs typos fixes (#770)
* update `classification()` docs with a new `EOL` section discussing that EOL does not have good failure behavior, and what to expect from them (#775)
* the following datasets are now available when the package is not loaded, so functions that use these datasets can now be called with package namespace like `taxize::downstream()`: `rank_ref`, `theplantlist`, `apg_families`, `apg_orders`  (#777)
* `sci2comm()` and `comm2sci()` improvements: for `db="ncbi"` we no longer stop with error when when there's no results for a query; instead we return `character(0)`. In addition, now all data source options for both functions now return `character(0)` when there's no results for a query (#778)
* `id2name.uid()` now actually passes on `...` internally for curl options

### BUG FIXES

* fix `get_nbnid()`: was returning non-taxon entities, have ot add `idxtype:TAXON` to the `fq` query (#761)
* fixes for `as.eolid()` and `as.colid()` - don't run through helper function that was raising error on HTTP 404/etc., dont want to fail  (#762)
* fix to `class2tree()`: set root node name to NA if it does not exist, ITIS does not set a root node (#767) (#769) work by @gpli
* fix to `ipni_search()`: IPNI changed parameter names, fixes for that; and now returning tibble's instead of data.frame's (#773) thanks @joelnitta !
* fix `ncbi_children()`: fixed regex that was supposed to flag ambiguous taxa only, it was supposed to flag `sp.` and `spp.`, but was including `subsp.`, which we didn't want included (#777)
* another fix to `ncbi_children()`: when ID is passed rather than a name, we need to then set `id=NULL` after switching to the equivalent taxononmic name internally to avoid getting duplicate data back (#777)


taxize 0.9.8
============

### NEW FEATURES

* all `get_*` functions gain some new features (associated new fxns are `taxon_last` and `taxon_clear`): a) nicer messages printed to the console when iterating through taxa, and a summary at the end of what was done; and b) state is now saved when running `get_*` functions. That is, in an object external to the `get_*` function call we keep track of what happened, so that if an error is encountered, you can easily restart where you left off; this is especially useful when dealing with a large number of inputs to a `get_*` function. To utilize, pass the output of `taxon_last()` to a `get_*` function call. Associated with these changes are new package imports: R6, crayon and cli (#736) (#757)
* gains a new function `taxize_options()` to set options when using taxize. the first reason for the function is to set two options for the above item for `get_*` functions: `taxon_state_messages` to allow taxon state tracking messages in `get_*` functions or not, and `quiet=TRUE` quiets output from the `taxize_options()` function itself

### MINOR IMPROVEMENTS

* in `id2name()` and `worms_downstream()` use `worrms::wm_record` instead of `worrms::wm_record_` for newest version of `worrms` (#760)
* many `get_*` functions and `col_downstream()` parameter `verbose` changed to `messages` to not conflict with a `verbose` curl options parameter passed in to `crul`

### BUG FIXES

* fix to http request processing for COL - sometimes errors, and gives a message in the response body, but DOES NOT give the appropriate error HTTP status code - need to always do a check for COL responses (#755) (#756) thanks @dougwyu
* fix to `gbif_downstream()` - GBIF in some cases returns a rank of "unranked", which we hadn't accounted for in internal rank processing code (#758) thanks @ocstringham


taxize 0.9.7
============

### MINOR IMPROVEMENTS

* `class2tree()` gains node labels when present (#644) (#748) thanks @gpli
* change documentation to use markdown (#658) (#746) thanks @Rekyt


taxize 0.9.6
============

### NEW FEATURES

* gains new functions for Kew's Plants of the World: `get_pow()`, `get_pow_()`, `as.pow()`, `classification.pow()`, `pow_search()`, and `pow_lookup()` (#598) (#739)
* we now pass a user agent string in all HTTP requests to the various data sources so they know its coming from `taxize`. the string will look something like `r-curl/3.3 crul/0.7.0 rOpenSci(taxize/0.9.6)`, including the versions of the `curl` R pkg, the `crul` package, and the `taxize` package (#662)
* change to `get_colid` functionality: we weren't paginating for the user when there were more than 50 results for a query; we now paginate for the user using async HTTP requests; this means that some requests will take longer than they did before if they have more than 50 results; this is a good change given that you get all the results for your query now (#743)
* change across most `get_*` functions: in some of the `get_*` functions we tried for a direct match (e.g., `"Poa" == "Poa"`) and if one was found, then we were done and returned that record. however, we didn't deploy the same logic across all `get_*` functions. Now all `get_*` functions check for a direct match. Of course if there is a direct match with more than 1 result, you still get the prompt asking you which name you want. (#631) (#734)

### MINOR IMPROVEMENTS

* Make separate `taxize-authentication` manual file covering authentication information across the package (#681)
* new case study vignette added (#544) (#721) thanks @fozy81
* add note to `gnr_resolve()` docs about age of datasets used in the Global Names Resolver, and how to access age of datasets (#737)
* `get_eolid()` fixes: gains new attribute `pageid`; `uri`'s given are updated to EOL's new URL format; `rank` and `datasource` parameters were not documented, now are; we no longer use short names for data sources within EOL, but instead use their full names  (#702) (#742)
* `col_search()` now returns attributes on the output data.frame's with number of results found and returned, and other metadata about the search 
* `gnr_datasources()` loses the `todf` parameter; now always returns a data.frame and the data.frame has all the columns, whereas the default call returned a limited set of columns in previous versions

### BUG FIXES

* fix bug in `get_wormsid()`, was failing when there was a direct match found with more than 1 result (#740)
* fix across all `get_*` functions: linting of the input to the `rows` parmeter was failing with a vector of values in some cases (#741)
* fix to `iucn_summary()`; we weren't passing on the API key internally correctly (#735) thanks @PrincessPi314 for the report

taxize 0.9.5
============

### DEFUNCT

* `iucn_summary_id()` is defunct, use `iucn_summary()` instead

### NEW FEATURES

* `col_downstream()` gains parameter `extant_only` (logical) to optionally keep extant taxa only (#714) thanks @ArielGreiner for the inquiry
* `downstream()` gains another `db` options: Worms. You can now set `db="worms"` to use Worms to get taxa downstream from a target taxon. In addition, `taxize` gains new function `worms_downstream()`, which is used under the hood in `downstream(..., db="worms")` (#713) (#715)
* gains new function `id2name()` with `db` options for tol, itis, ncbi, worms, gbif, col, and bold. the function converts taxonomid IDs to names. It's sort of the inverse of the `get_*()` family of functions. (#712) (#716)
* `tax_rank()` gains new parameter `rows` so that one can pass `rows` down to `get_*()` functions

### MINOR IMPROVEMENTS

* `synonyms()` warning from an internal `cbind()` call now fixed (#704) (#705) thanks @vijaybarve
* namespace `taxize` function calls thrown when notifying users about API keys (e.g., `taxize::use_tropicos()`) to make it very clear where the functions live (to avoid confusion with `usethis`) (#724) (#725) thanks @maelle
* changed `iucn_summary()` to output the same structure when no match is found as when a match is found so that when output is passed to `iucn_status()` behavior is the same (#708) thanks @Rekyt
* skip `tax_name()` tests on CRAN (#728)
* `httr` replaced by `crul` throughout (#590)
* most unit tests that make HTTP requests now cached with `vcr`, making tests much faster and not prone to errors to remote services being down (#729)
* EOL: The EOL API underwent major changes, and we've attempted to get things in working order. `eol_dataobjects()` gains new parameter `language`. `eol_pages()` loses `iucn`, `images`, `videos`, `sounds`, `maps`, and `text` parameters, and gains `images_per_page`, `videos_per_page`, `sounds_per_page`, `maps_per_page`, `texts_per_page`, and `texts_page`. Please do let us know if you find any problems with any EOL functions (#717) (#718)
* As part of EOL changes, the default `db` value for `comm2sci()` and `sci2comm()` is now `ncbi` instead of `eol`
* EUBON base URL now https instead of http
* A number of `get_*()` functions changed parameter `verbose` to `messages` to not conflict with `verbose` passed down to `crul::HttpClient`
* ping functions: `ncbi_ping()` reworked to allow use of your api key as a parameter or pulled from your environemnt; `eol_ping()` using https instead of http, and parsing JSON instead of XML.

### BUG FIXES

* `get_eolid()` was erroring when no results found for a query due to not assigning an internal variable (#701) (#709) thanks for the fix @taddallas
* `get_tolid()` was erroring when values were `NULL` - now replacing all `NULLL` with `NA_character_` to make `data.table::rbindlist()` happy (#710) (#711) thanks @gpli for the fix
* add additional rows to the `rank_ref` data.frame of taxonomic ranks: species subgroup, forma, varietas, clade, megacohort, supercohort, cohort, subcohort, infracohort. when there's no matched rank errors can result in many of the downstream functions. The data.frame now has 43 rows. (#720) (#727)
* fix to `downstream()` and `ncbi_get_taxon_summary()`: change in `ncbi_get_taxon_summary` to break up queries into smaller chunks to avoid HTTP 414 errors ("URI too long") (#727) (#730) thanks for reporting @fischhoff and  @benjaminschwetz 
* a number of fixes internally (not user facing) to comply with upcoming R-devel changes for checking length greater than 1 in logical statements (#731)


taxize 0.9.4
============

### NEW FEATURES

* new contributor: [Gaopeng Li](https://github.com/gpli)
* gains new functions for helping the user get authentication keys/tokens: `use_entrez()`, `use_eol()`, `use_iucn()` (which uses internally `rredlist::rl_use_iucn()`), and `use_tropicos()` (#682) (#691) (#693) By @maelle
 
### MINOR IMPROVEMENTS

* remove commented out code

### BUG FIXES

* fix `tropicos_ping()`
* fixed `downstream()` and `gbif_downstream()`: some of the results don't have a `canonicalName`, so now safely try to get that field  (#673)
* fixed `as.uid()`, was erroring when passing in a taxon ID (#674) (#675) by @zachary-foster
* fix in `get_boldid()` (and by extension `classification(..., db = "bold")`): was failing when no parent taxon found, just fill in with NA now (#680)
* fix to `synonyms()`: was failing for some TSNs for `db="itis"` (#685)
* fix to `tax_name()`: `rows` arg wasn't being passed on internally (#686)
* fix to `gnr_resolve()` and `gnr_datasources()`: problems were caused by http scheme, switched to use https instead of http (#687)
* fix to `class2tree()`: organisms with unique rank lower than non-unique ranks will give extra wrong rows (#689) (#690) thanks @gpli
* fix in `ncbi_get_taxon_summary()`: changes in the NCBI API most likely lead to HTTP 414 (URI Too Long) errors. we now loop internally for the user. By extension this helps problems upsteam in `downstream()`/`ncbi_downstream()`/`ncbi_children()` (#698)
* fix in `class2tree()`: was erroring when name strings contained pound signs (e.g., `#`) (#699) (#700) thanks @gpli


taxize 0.9.3
============

### MINOR IMPROVEMENTS

* package gains three new authors: Bastian Greshake Tzovaras, Philippe Marchand, and Vinh Tran
* Don't enforce rate limiting via `Sys.sleep` for NCBI requests if the user has an API key (#667)
* Fix to all functions that do NCBI requests to work whether or not a user has an NCBI API key (#668)
* Increased documentation on authentication, see `?taxize-authentication`
* Further conversion of `verbose` to `messages` across the package so that supressing calls to `message()` do not conflict with curl options passed in
* Converted `genbank2uid()` and `ncbi_get_taxon_summary()` to use `crul` instead of `httr` for HTTP requests

### BUG FIXES

* Fix to `get_tolid()`: it was missing assignment of the `att` attribute internally, causing failures in some cases (#663) (#672)
* Fix to `ncbi_children()` (and thus `children()` when requesting NCBI data) to not fail when there is an empty result from the internal call to `classification()` (#664) thanks @arendsee



taxize 0.9.2
===================

### NEW FEATURES

* `class2tree()` gets a major overhaul thanks to @gedankenstuecke and @trvinh (!!). The function now takes unnamed ranks into account when clustering, which fixes problem where trees were unresolved for many splits as the named taxonomy levels were shared between them. Now it makes full use of the NCBI Taxonomy string, including the unnamed ranks, leading to higher resolution trees that have less multifurcations (#611) (#634)
* Added support throughout package for use of NCBI Entrez API keys - NCBI now strongly encourages their use and you get a higher rate limit when you use one. See `?taxize-authentication` for help. Importantly, note that API key names (both R options and environment variables) have changed. They are now the same for R options and env vars: TROPICOS_KEY, EOL_KEY, PLANTMINER_KEY, ENTREZ_KEY. You no longer need an API key for Plantminer. (#640) (#646)
* New author Zebulun Arendsee (@arendsee)
* New package dependencies: `crul` and `zoo`

### MINOR IMPROVEMENTS

* In `downstream()` we now pass on `limit` and `start` parameters to `gbif_downstream()`; we weren't doing that before; the two parameters control pagination (#638)
*  `genbank2uid()` now returns the correct ID when there are multiple possibilities and invalid IDs no longer make whole batches fail (#642) thanks @zachary-foster
* `children()` outputs made more consistent for certain cases when no results found for searches (#648) (#649) thanks @arendsee
* Improve `downstream()` by passing `...` (additional parameters) down to `ncbi_children()` used internally. allows e.g., use of `ambiguous` parameter in `ncbi_children()` allows you to remove ambiguousl named nodes (#653) (#654) thanks @arendsee
* swapped out use of `httr` for `crul` in EOL and Tropics functions - note that this won't affect you unless you're passing curl options. see package `crul` for help on curl options. Along with this change, the parameter `verbose` has changed to `messages` (for toggling printing of information messages)

### DOCUMENTATION

* Added additional text to the `CONTRIBUTING.md` file for how to contribute to the test suite (#635)

### BUG FIXES

* `genbank2uid` now returns the correct ID when there are multiple possibilities and invalid IDs no longer make whole batches fail.
* Fix to `downstream()`: passing numeric taxon ids to the function while using `db="ncbi"` wasn't working (#641) thanks @arendsee
* Fix to `children()`: passing numeric taxon ids to the function while using `db="worms"` wasn't working (#650) (#651) thanks @arendsee
* `synonyms_df()` - that attemps to combine many outputs from the `synonyms()` function -  now removes NA/NULL/empy outputs before attempting the combination (#636)
* Fix to `gnr_resolve()`: before if `preferred_data_sources` was used, you would get the preferred data but only a few columns of the response. We now return all fields; however, we only return the preferred data part when that parameter is used  (#656)
* Fixes to `children()`. It was returning unexpected results for amgiguous taxonomic names (e.g., there's some insects that are returned when searching within Bacteria). It was also failing when one tried to get the children of a root taxon (e.g., the children of the NCBI id 131567). (#639) (#647) fixed via PR (#659) thanks @arendsee and @zachary-foster


taxize 0.9.0
============

### Changes to `get_*()` functions

* Added separate documentation file for all get* functions 
describing attributes and various exception behaviors
* Some `get*()` functions had `NaN` as default `rows` parameter
value. Those all changed to `NA`
* Better failure behavior now when non-acceptable `rows` 
parameter value given
* Added in all type checks for parameters across `get_*()` functions
* Changed behavior across all `get_*()` functions to behave the 
same when `ask = FALSE, rows = 1` and `ask = TRUE, rows = 1` as these
should result in the same outcome. (#627) thanks @zachary-foster !
* Fixed direct match behavior so that when there's multiple results 
from the data provider, but no direct match, that the functions don't 
give back just `NA` with no inication that there were multiple matches.
* Please let me know if any of these changes cause problems for your
code or package.

### NEW FEATURES

* Change `comm2sci()` to S3 setup with methods for `character`, `uid`, 
and `tsn` (#621)
* `iucn_status()` now has S3 setup with a single method that only handles
output from the `iucn_summary()` function.

### MINOR IMPROVEMENTS

* Add required `key` parameter to fxn `iucn_id()` (#633)
* imrove docs for `sci2comm()`: to indicate how to get non-simplified
output (which includes what language the common name is from) vs. 
getting simplified output (#623) thanks @glaroc !
* Fix to `sci2comm()` to not be case sensitive when looking for matches 
(#625) thanks @glaroc !
* Two additional columns now returned with `eol_search()`: `link` and `content`
* Improve docs in `eol_search()` to describe returned `data.frame`
* Fix `bold_bing()` to use new base URL for their API
* Improved description of the dataset `rank_ref`, see `?rank_ref`

### BUG FIXES

* Fix to `downstream()` via fix to `rank_ref` dataset to include
"infraspecies" and make "unspecified" and "no rank" requivalent.
Fix to `col_downstream()` to remove properly ranks lower than 
allowed. (#620) thanks @cdeterman !
* `iucn_summary`: changed to using `rredlist` package internally.
`sciname` param changed to `x`. `iucn_summary_id()` now is 
deprecated in favor of `iucn_summary()`. `iucn_summary()` now has a
S3 setup, with methods for `character` and `iucn` (#622)
* Added "cohort" to `rank_ref` dataset as that rank sometimes used 
at NCBI (from bug reported in `ncbi_downstream()`) (#626)
* Fix to `sci2comm()`, add `tryCatch()` to internals to catch 
failed requests for specific pageid's (#624) thanks @glaroc !
* Fix URL for taxa for NBN taxonomic ids retrieved via 
`get_nbnid()` (#632)


taxize 0.8.9
============

### BUG FIXES

* Remove `ape::neworder_phylo` object, which is not used anymore in `taxize`  
(#618) (#619) thanks @ashiklom


taxize 0.8.8
============

### NEW FEATURES

* New function `ncbi_downstream()` and now NCBI is an option in 
the function `downstream()` (#583) thanks for the push @andzandz11
* New data source: Wiki*, which includes Wikipedia, Wikispecies, and 
Wikidata - you can choose which you'd like to search. Uses new package
`wikitaxa`, with contributions from @ezwelty (#317)
* `scrapenames()` gains a parameter `return_content`, a boolean, to 
optionally return the OCR content as a text string with the results. (#614)
thanks @fgabriel1891
* New function `get_iucn()` - to get IUCN Red List ids for taxa. In addition,
new S3 methods `synonyms.iucn` and `sci2comm.iucn` - no other methods could 
be made to work with IUCN Red List ids as they do no share their taxonomic
classification data (#578) thanks @diogoprov 

### MINOR IMPROVEMENTS

* `bold` now an option in `classification()` function (#588)
* fix to NBN to use new base URL (#582) ($597)
* `genbank2uid()` can give back more than 1 taxon matched to a given
Genbank accession number. Now the function can return more than one 
match for each query, e.g., try `genbank2uid(id = "AM420293")` (#602)
 thanks @sariya
* had to modify `cbind()` usage to incclude `...` for method 
consistency (#612)
* `tax_rank()` used to be able to do only ncbi and itis. Can now do a 
lot more data sources: ncbi, itis, eol, col, tropicos, gbif, nbn,
worms, natserv, bold  (#587)
* Added to `classification()` docs in a section `Lots of results` a 
note about how to deal with results when there are A LOT of them. (#596)
thanks @ahhurlbert for raising the issue
* `tnrs()` now returns the resulting data.frame in the oder of the 
names passed in by the user (#613) thanks @wpetry
* Changes to `gnr_resolve()` to now strip out taxonomic names submitted 
by user that are NA, or zero length strings, or are not of class 
character (#606)
* Added description of the columns of the data.frame output in 
`gnr_resolve()` (#610) thanks @kamapu 
* Added noted in `tnrs()` docs that the service doesn't provide any
information about homonyms. (#610) thanks @kamapu 
* Added `parvorder` to the `taxize` `rank_ref` dataset - used by NCBI - 
if tax returned with that rank, some functions in `taxize` were failing 
due to that rank missing in our reference dataset `rank_ref` (#615)

### BUG FIXES

* Fix to `get_colid()` via problem in parsing within `col_search()` (#585)
* Fix to `gbif_downstream` (and thus fix in `downstream()`): there 
was two rows with form in our `rank_ref` reference dataset of rank names, 
causing > 1 result in some cases, then causing `vapply` to fail as it's 
expecting length 1 result (#599) thanks @andzandz11
* Fix `genbank2uid()`: was failing when getting more than 1 result back, 
works now (#603) and fails better now, giving back warnings/error messages
that are more informative (see also #602) thanks @sariya
* Fix to `synonyms.tsn()`: in some cases a TSN has > 1 accepted name. We 
get accepted names first from the TSN, then look for synonyms, and hadn't 
accounted for > 1 accepted name. Fixed now (#607) thanks @tdjames
* Fixed bug in `sci2comm()` - was not dealing internally with passing 
the `simplify` parameter (#616)


taxize 0.8.4
============

### NEW FEATURES

* Added WoRMS integration via the new `worrms` package on CRAN.
Adds functions `as.wormsid()`, `get_wormsid()`, `get_wormsid_()`,
`children.wormsid()`, `classification.wormsid()`, `sci2comm.wormsid()`,
`comm2sci.wormsid()`, and `synonyms.wormsid()` (#574) (#579)
* New functions for NatureServe data, including `as.natservid`,
`get_natservid`, `get_natservid_`, and `classification.natservid`
(#126)

### BUG FIXES

* EOL API keys were not passed on to internal functions. fixed now.
thanks @dschlaep ! (#576)
* Fix in `rankagg()` with respect to `vegan` package to work with
older and new version of `vegan` - thank @jarioksa (#580) (#581)

taxize 0.8.0
============

### NEW FEATURES

* New data source added: Open Tree of Life. New functions for the data source
added: `get_tolid()`, `get_tolid_()`, and `as.tolid()` (#517)
* related to above `classification()` gains new method for TOL data
* related to above `lowest_common()` gains new method for TOL data
* Now using `ritis` package, an external dependency for ITIS taxonomy
data. Note that a large number of ITIS functions were removed, and are
now available via the package `ritis`. However, there are still many
high level functions for working with ITIS data (see functions prefixed
with `itis_`), and `get_tsn()`, `classification.tsn()`, and similar
high level functions remain unchanged. (#525)
* EUBON has a new API (v1.2). We now interact with that new API version.
In addition, `eubon()` fxn is now `eubon_search()`, although either still
work - though `eubon()` will be made defunct in the next version of
this package. Additional new functions were added: `eubon_capabilities()`,
`eubon_children()`, and `eubon_hierarchy()` (#567)
* `lowest_common()` function gains two new data source options: COL (Catalogue
of Life) and TOL (Tree of Life) (#505)
* Addded new function `synonyms_df()` as a slim wrapper around
`data.table::rbindlist()` to make it easy to combine many outputs
from `synonyms()` for a single data source - there is a lot of heterogeneity
among data sources in how they report synonyms data, so we don't attempt
to combine data across sources (#533)

### MINOR IMPROVEMENTS

* Change NCBI URLs to `https` from `http` (#571)

### BUG FIXES

* Fixed bug in `tax_name()` in which when an invalid taxon was searched
for then `classification()` returned no data and caused an error.
Fixed now. (#560) thanks @ljvillanueva for reporting it!
* Fixed bug in `gnr_resolve()` in which order of input names to the function
was not retained. fixed now. (#561) thanks @bomeara for reporting it!
* Fixed bug in `gbif_parse()` - data format changed coming back from
GBIF - needed to replace `NULL` with `NA` (#568)  thanks @ChrKoenig for
reporting it!


taxize 0.7.9
============

### NEW FEATURES

* New vignette: "Strategies for programmatic name cleaning" (#549)

### MINOR IMPROVEMENTS

* `get_*()` functions now have new attributes to further help the user:
`multiple_matches` (logical) indicating whether there were multiple
matches or not, and `pattern_match` (logical) indicating whether a
pattern match was made, or not. (#550) from (#547) discussion,
thanks @ahhurlbert ! see also (#551)
* Change all `xml2::xml_find_one()` to `xml2::xml_find_first()`
for new `xml2` version (#546)
* `gnr_resolve()` now retains user supplied taxa that had no matches -
this could affect your code, make sure to check your existing code (#558)
* `gnr_resolve()` - stop sorting output data.frame, so order of rows
in output data.frame now same as user input vector/list (#559)

### BUG FIXES

* Fixed internal fxn `sub_rows()` inside of most `get_*()` functions
to not fail when the data.frame rows were less than that requested by
the user in `rows` parameter (#556)
* Fixed `get_gbifid()`, as sometimes calls failed because we now
return numberic IDs but used to return character IDs (#555)
* Fix to all `get_()` functions to call the internal `sub_rows()`
function later in the function flow so as not to interfere with
taxonomic based filtering (e.g., user filtering by a taxonomic rank)
(#555)
* Fix to `gnr_resolve()`, to not fail on parsing when no data
returned when a preferred data source specified (#557)

taxize 0.7.8
============

### MINOR IMPROVEMENTS

* Fix to `iucn_summary()` (#543) thanks @mcsiple
* Added message for when too many Ids passed in to `ncbi_get_taxon_summary()`
suggesting to break up the ids into chunks (#541) thanks @daattali
* Fix to `itis_acceptname()` to accept multiple names (#534) and now
gives back same output regardless of whether match found or not (#531)

### BUG FIXES

* Fix to `tax_name()` for some queries that return no classification data
via internal call to `classification()` (#542) thanks @daattali
* Another fix for `tax_name()` (#530) thanks @ibartomeus
* Fixed docs for `rankagg()` function, use `requireNamespace()` in examples
to make sure user has `vegan` installed (#529)

taxize 0.7.6
============

### MINOR IMPROVEMENTS

* Changed defunct messages in `eol_invasive()` and `gisd_invasive()`
to point to new location in the [originr](https://github.com/ropenscilabs/originr)
package. Also, cleaned out code in those functions as not avail.
anymore (#494)
* Access to IUCN taxonomy information is now provided through the newish
[rredlist](https://github.com/ropenscilabs/rredlist) package. (Two issues
dealing with IUCN problems (#475) (#492))

### BUG FIXES

* Fix to `get_gbifid()` to use new internal code to provide two
ways to search GBIF taxonomy API, either via `/species/match` or via
`/species/search`, instead of `/species/suggest`, which we used previously.
The suggest route was too coarse. `get_gbifid()` also gains a parameter
`method` to toggle whether you search for names using `/species/match` or
`/species/search`.  (#528)
* Fix for `col_search()` to handle when COL can return a value of
`missapplied name`, which a `switch()` statement didn't handle yet (#511)
thanks @JoStaerk !
* Fixes for `get_colid()` and `col_search()` (#523) thanks @zachary-foster !

taxize 0.7.5
============

### BUG FIXES

* Fixed bug in the package dependency `bold`, which fixes
`taxize::bold_search()`, so no actual changes in `taxize` for
this, but take note (#521)
* Fixed problem in `gnr_resolve()` where we indexed to data
incorrectly. And added tests to account for this problem.
Thanks @raredd ! (#519) (#520)
* Fixed bug in `iucn_summary()` introduced in last version.
`iucn_summary()` now uses the package `rredlist`, which requires
an API key, and I didn't document how to use the key. Function
now allows user to pass the key in as a parameter, and documents
how to get a key and save it in either `.Renviron` or in
`.Rprofile` (#522)


taxize 0.7.4
===============

### NEW FEATURES

* New function `lowest_common()` for obtaining the lowest common taxon and
rank for a given taxon name or ID. Methods so far for ITIS, NCBI, and GBIF (#505)
* New contributor James O'Donnell (@jimmyodonnell) (via #505)
* Now importing `rredlist` [rredlist](https://github.com/ropenscilabs/rredlist)
* New function `iucn_summary_id()` - same as `iucn_summary()`, except takes
IUCN IDs as input instead of taxonomic names (#493)
* All taxonomic rank columns in data.frame's now given back as lower case.
This provides consistency, which is important, and many functions use ranks
to determine what to do next, so using a consistent case is good.

### MINOR IMPROVEMENTS

* `iucn_summary()` fixes, long story short: a number of bug fixes, and uses
the new IUCN API via the newish package `rredlist` when IDs are given as input,
but uses the old IUCN API when taxonomic names given. Also: gains new parameter `distr_details`
(#174) (#472) (#487) (#488)
* Replaced `XML` with `xml2` for XML parsing (#499)
* Fixes to internal use of `httr::content` to explicitly state `encoding="UTF-8"` (#498)
* `gnr_resolve()` now outputs a column (`user_supplied_name`) for the exact input taxon
name - facilitates merging data back to original data inputs (#486) thanks @Alectoria
* `eol_dataobjects()` gains new parameter `taxonomy` to toggle whether to return
any taxonomy details from different data providers (#497)
* Catalogue of Life URLs changed - updated all appropriate COL functions to use
the new URLs (#501)
* `classification()` was giving back rank values in mixed case from different data
providers (e.g., `class` vs. `Class`). All rank values are now all lowercase (#504)
* Changed number of results returned from internal GBIF search in `get_gbfid` to
50 from 20. Gives back more results, so more likely to get the thing searched for (#513)
* Fix to `gni_search()` to make all output columns `character` class
* `iucn_id()`, `tpl_families()`, and `tpl_get()` all gain a new parameter `...` to
pass on curl options to `httr::GET()`

### BUG FIXES

* Fixes to `get_eolid()`: URI returned now always has the pageid, and goes to the
right place; API key if passed in now actually used, woopsy (#484)
* Fixes to `get_uid()`: when a taxon not found, the "match" attribute was saying
found sometimes anyway - that is now fixed; additionally, fixed docs to correctly
state that we give back `'NA due to ask=FALSE'` when `ask = FALSE` (#489) Additionally,
made this doc fix in other `get_*()` function docs
* Fix to `apgOrders()` function (#490)
* Fixes to `tp_search()` which fixes `get_tpsid()`: Tropicos doesn't allow periods (`.`) in
query strings, so those are URL encoded now; Tropicos doesn't like sub-specific rank names
in name query strings, so we warn when those are found, but don't alter user inputs; and
improved docs to be more clear about how the function fails (#491) thanks @scelmendorf !
* Fix to `classification(db = "itis")` to fail better when no taxa found (#495) thanks @ashenkin !
* `eol_pages()` fixes: the EOL API route for this method gained a new parameter `taxonomy`,
this function gains that parameter. That change caused this fxn to fail. Now fixed. Also,
parameter `subject` changed to `subjects` (#500)
* Fix to `col_search()` due to when `misapplied name` come back as a data slot. There
was previously no parser for that type. Now there is, and it works (#512)

taxize 0.7.0
===============

### NEW FEATURES

* Now requires `R >= 3.2.1`. Good idea to update your R installation anyway (#476)
* New function `ion()` for obtaining data from Index of Organism Names (#345)
* New function `eubon()` for obtaining data from EU (European Union) BON
taxonomy (#466) Note that you may onloy get partial results for some requests
as paging isn't implemented yet in the EU BON API (#481)
* New suite of functions, with prefix `fg_*()` for obtaining data from Index
Fungorum. More work has to be done yet on this data source, but these initial
functions allow some Index Fungorum data access (#471)
* New function `gbif_downstream()` for obtaining downstream names from
GBIF's backbone taxonomy. Also available in `downstream()`, where you can
request downstream names from GBIF, along with other data sources (#414)

### MINOR IMPROVEMENTS

* Note added in docs for all `db` parameters to warn users that if they
provide the wrong `db` value for the given taxon ID, they can get data
back, but it would be wrong. That is, all taxonomic data sources available
in `taxize` use their own unique IDs, so a single ID value can be in multiple
data sources, even though the ID refers to different taxa in each data source.
There is no way we can think of to prevent this from happening, so be cautious.
(#465)
* A note added to all IUCN functions to warn users that sometimes incorrect
data is returned. This is beyond our control, as sometimes IUCN itself gives
back incorrect data, and sometimes EOL/Global Names (which we use in some of
the IUCN functions) give back incorrect data. (#468) (#473) (#174) (472) (#475)

### BUG FIXES

* Fix to `gnr_resolve()` to by default capitalize first name of a name string
passed to the function. GNR is case sensitive, so case matters (#469)

### DEFUNCT

* `phylomatic_tree()` and `phylomatic_format()` are defunct. They were deprecated
in recent versions, but are now gone. See the new package `brranching` for
Phylomatic data (#479)

taxize 0.6.6
===============

### MINOR IMPROVEMENTS

* `stripauthority` argument in `gnr_resolve()` has been renamed to `canonical`
to better match what it actually does (#451)
* `gnr_resolve()` now returns a single data.frame in output, or `NULL`
when no data found. The input taxa that have no match at all are returned in
an attribute with name `not_known` (#448)
* updated some functions to work with to R >3.2.x
* In `vascan_search()` changed `callopts` parameter to `...` to pass in curl
options to the request.
* In `ipni_search()` changed `callopts` parameter to `...` to pass in curl
options to the request. In addition, better http error handling, and
added a test suite for this function. (#458)
* `stringsAsFactors=FALSE` now used for `gibf_parse()` (https://github.com/ropensci/taxize/commit/c0c4175d3a0b24d403f18c057258b67d3fbf17f0)
* Made nearly all column headers and list names lowercase to simplify
indexing to elements, as well as combining outputs. (#462)
* Plantminer API updated to use a new API. Option to search ThePlantList or
the Brazilian Flora Checklist (#464)
* Added more details to the documentation for `get_uid()` to make more clear
how to use the varoious parameters to get the desired result, and how to
avoid certain pitfalls (#436)
* Removed the parameter `asdf` from the function `eol_dataobjects()` - now
returning data.frame's only.
* Added some error catching to `get_eolid()` via `tryCatch()` to fail better
when names not found.
* Dropped `openssl` as a package dependency. Not needed anymore because uBio
dropped.

### BUG FIXES

* `gnr_resolve()` failed when no canonical form was found.
* Fixed `gnr_resolve()` when no results found when `best_match_only=TRUE` (#432)
* Fixed bug in internal function `itisdf()` to give back an empty data.frame
when no results found, often with subspecific taxa. Helps solve errors reported
in use of `downstream()`, `itis_downstream()`, and `gethierarchydownfromtsn()` (#459)

### NEW FEATURES

* `gnr_resolve()` gains new parameter `with_canonical_ranks` (logical) to choose
whether infraspecific ranks are returned or not.
* New function `iucn_id()` to get the IUCN ID for a taxon from it's name. (#431)

### DEFUNCT

* All functions that interacted with the taxonomy service uBio are now
defunct. Of course we would deprecate first, then make defunct later, to
make transition easier, but that is out of our hands. The functions
that are defunct are: `ubio_classification()`, `ubio_classification_search()`,
`ubio_id()`, `ubio_search()`, `ubio_synonyms()`, `get_ubioid()`, `ubio_ping()`.
In addition, ubio has been removed as an option in the `synonyms()` function,
and references for uBio have been removed from the `taxize_cite()` utility
function. (#449)

taxize 0.6.2
===============

### MINOR IMPROVEMENTS

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

### BUG FIXES

* Fixed typo in `taxize_cite()`
* Fixed a bug in `classification()` where numeric IDs as input got
converted to itis ids just because they were numeric. Fixed now. (#434)
* Catalogue of Life (COL) changed from using short numeric codes for taxa to
long alphanumeric UUID type ids. This required fixing functions using COL
web services (#435)


taxize 0.6.0
===============

### NEW FEATURES

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

### MINOR IMPROVEMENTS

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

### BUG FIXES

* Fixed encoding issues in `tpl_families()` and `tpl_get()`. (#424)

### DEPRECATED AND DEFUNCT

* The following functions that were deprecated are now defunct (no longer available):
`ncbi_getbyname()`, `ncbi_getbyid()`, `ncbi_search()`, `eol_invasive()`,
`gisd_isinvasive()`. These functions are available in the `traits` package. (#382)
* `phylomatic_tree()` is deprecated, but will be defunct in a upcoming version.

taxize 0.5.2
===============

### NEW FEATURES

* New set of functions to ping each of the APIs used in `taxize`. E.g., `itis_ping()` pings ITIS and returns a logical, indicating if the ITIS API is working or not. You can also do a very basic test to see whether content returned matches what's expected. (#394)
* New function `status_codes()` to get vector of HTTP status codes. (#394)

### MINOR IMPROVEMENTS

* Removed startup message.
* Now can pass in curl options to `itis_ping()`, and all `*_ping()` functions.

### BUG FIXES

* Moved examples that were in `\donttest` into `\dontrun`.

taxize 0.5.0
===============

### NEW FEATURES

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

### MINOR IMPROVEMENTS

* `assertthat` removed from package imports, replaced with `stopifnot()`, to reduce dependency load. (#387)
* `eol_hierarchy()` now defunct (no longer available) (#228) (#381)
* `tp_classifcation()` now defunct (no longer available) (#228) (#381)
* `col_classification()` now defunct (no longer available) (#228) (#381)
* New manual page listing all the low level ITIS functions for which their manual pages are not shown in the package index, but are available if you to `?fxn-name`.
* All `get_*()` functions gain a new parameter `rows` to allow selection of particular rows. For example, `rows=1` to select the first row, or `rows=1:3` to select rows 1 through 3. (#347)
* `classification()` now by default returns taxonomic identifiers for each of the names. This can be toggled off by the `return_id=FALSE`. (#359) (#360)
* Simplification of many higher level functions to use `switch()` on the `db` parameter, which helps give better error message when a `db` value is not possible or spelled incorrectly. (#379)

### BUG FIXES

* Lots of reduction of redundancy in internal functions. (#378)

taxize 0.4.0
===============

### NEW FEATURES

* New data sources added to taxize: BOLD (Biodiversity of Life Database). Three more data sources were added (World Register of Marine Species (WoRMS), Pan-European Species directories Infrastructure (PESI), and Mycobank), but are not available on CRAN. Those three data sources provide data via SOAP web services protocol, which is hard to support in R. Thus, those sources are available on Github. See https://github.com/ropensci/taxize#version-with-soap-data-sources
* New function `children()`, which is a single interface to various data sources to get immediate children from a given taxonomic name. (#304)
* New functions added to search BOLD data" `bold_search()` that searches for taxa in the BOLD database of barcode data; `get_boldid()` to search for a BOLD taxon identifier. (#301)
* New function `get_ubioid()` to get a uBio taxon identifier. (#318)
* New function started (not complete yet) to get suggested citations for the various data sources available in `taxize`: `taxize_cite()`. (#270)

### MINOR IMPROVEMENTS

* Using `jsonlite` instead of `RJSONIO` throughout the `taxize`.
* `get_ids()` gains new option to search for a uBio ID, in addition to the others, itis, ncbi, eol, col, tropicos, and gbif.
* Fixed documentation for `stripauthority` parameter `gnr_resolve()`. (#325)
* `iplant_resolve()` now outputs data.frame structure instead of a list. (#306)
* Clarified parameter `seqrange` in `ncbi_getbyname()` and `ncbi_search()` (#328)
* `synonyms()` gains new data source, can now get synonyms from uBio data source (#319)
* `vascan_search()` giving back more useful results now.

### BUG FIXES

* Added error catching for when URI is too long, i.e., when too many names provided (#329) (#330)
* Various fixes to `tnrs()` function, including more meaningful error messages on failures (#323) (#331)
* Fixed bug in `getpublicationsfromtsn()` that caused function to fail on data.frame's with no data on name assignment (#297)
* Fixed bug in `sci2comm()` that caused fxn to fail when using `db=itis` sometimes (#293)
* Fixes to `scrapenames()`. Sending a text blob via the `text` parameter now works.
* Fixes to `resolve()` so that function now works for all 3 data sources. (#337)

taxize 0.3.0
===============

### NEW FEATURES

* New function `iplant_resolve()` to do name resolution using the iPlant name resolution service. Note, this is different from http://taxosaurus.org/ that is wrapped in the `tnrs()` function.
* New function `ipni_search()` to search for names in the International Plant Names Index (IPNI).
* New function `resolve()` that unifies name resolution services from iPlant's name resolution service (via `iplant_resolve()`), Taxosaurus' TNRS (via `tnrs()`), and GNR's name resolution service (via `gnr_resolve()`).
* All `get_*()` functions how returning a new _uri_ attribute that is a link to the taxon on on the web. If NA is given back (e.g. nothing found), the uri attribute is blank. You can go directly to the uri in your default browser by doing, for example: `browseURL(attr(result, "uri"))`.
* `get_eolid()` now returns an attribute _provider_ because EOL collates taxonomic data form a lot of sources, then gives back IDs that are internal EOL ids, not those matching the id of the source they pull from. This should help with provenance, and should help if there is confusion about why the id givenb back by this function does not match that from the original source.
* Within the `get_tsn()` function, now using the function `itis_terms()`, which gives back the accepted status of the taxa. This allows a new parameter in the function (`accepted`, logical) that allows user to say give back only accepted status names (`accepted=TRUE`), or to give back all names (`accepted=FALSE`).
* `gnr_resolve()` gains two new parameters `best_match_only` (logical, to return best match only) and `preferred_data_sources` (to return preferred data sources) and `callopts` to pass in curl options.
* `tnrs()`, `tp_accnames()`, `tp_refs()`, `tp_summary()`, and `tp_synonyms()` gain new parameter `callopts` to pass in curl options.

### MINOR IMPROVEMENTS

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

### BUG FIXES

* `classification.gbifid()` did not return the correct result when taxon not found.
* Fixed bugs in many functions, see #245, #248, #254, #277.
* `classification()` used to fail when it was passed a subset of a vector of ids, in which case the class information was stripped off. Now works (#284)

taxize 0.2.2
===============

### NEW FEATURES

* itis_downstream() and col_downstream() functions accessible now from a single function downstream() (https://github.com/ropensci/taxize/issues/238)

### MINOR IMPROVEMENTS

* Added a extension function classification() for the gbif id class, classification.gbifid() (https://github.com/ropensci/taxize/issues/241)

### BUG FIXES

* Added some error catching to class2tree function. (https://github.com/ropensci/taxize/issues/240)
* Fixed problems in cbind.classification() and rbind.classification() where the first column of the ouput was a useless column name, and all column names now lower case for consistency. (https://github.com/ropensci/taxize/issues/243)
* classification() was giving back IDS instead of taxon names on the list element names, fixed this so hopefully all are giving back names. (https://github.com/ropensci/taxize/issues/243)
* Fixed bugs in col_*() functions so they give back data.frame's now with character class columns instead of factors, damned stringsAsFactors!  (https://github.com/ropensci/taxize/issues/246)


taxize 0.2.0
===============

### MINOR IMPROVEMENTS

* New dataset: Lookup-table for family, genus, and species names for ThePlantList under dataset name "theplantlist".
* get_ids() now accepts "gbif" as an option via use of get_gbifid().
* Changed function itis_phymat_format() to phylomatic_format() - this function gets the typical Phylomatic format name string "family/genus/genus_epithet"

### BUG FIXES

* Updated gbif_parse() base url to the new one (http://api.gbif.org/v1/parser/name).
* Fixes to phylomatic_tree().

### NEW FEATURES

* New function class2tree() to convert list of classifications to a tree. For example, go from a list of classifications from the function classification() to this function to get a taxonomy tree in ape phylo format.
* New function get_gbfid() to get a Global Biodiversity Information Facility identifier. This is the ID GBIF uses in their backbone taxonomy.
* classification() outputs gain rbind() and cbind() generic methods that act on the various outputs of classification() to bind data width-wise, or column-wise, respectively.

taxize 0.1.9
===============

### MINOR IMPROVEMENTS

* Updated ncbi_search() to retrieve more than a max of 500, slightly changed column headers in output data files, and if didn't before, now accepts a vector/list of taxonomic names instead of just one name.

taxize 0.1.8
===============

### NEW FEATURES

* We attempted to make all ouput column names lowercase, and to increase consistency across column names in outputs from similar functions.
* New function scrapenames() uses the Global Names Recognition and Discovery service to extract taxonomic names from a web page, pdf, or other document.
* New function vascan_search() to search the CANADENSYS Vascan names database.

### BUG FIXES

* Fixed bugs in get_tpsid(), get_eolid() and eol_pages().
* phylomatic_tree() bugs fixed.

### MINOR IMPROVEMENTS

* classification() methods were simplified. Now classification() is the workhorse for every data-source. col_classification(), eol_hierarchy(), and tp_classification() are now deprecated and will be removed in the next taxize version.
* classification() gains four new arguments: start, checklist, key, and callopts.
* comm2sci() gains argument simplify to optionally simplify output to a vector of names (TRUE by default).
* get_eolid() and get_tpsid() both gain new arguments key to specify an API key, and ... to pass on arguments to eol_search().
* Added ncbi as a data source (db="ncbi") in sci2comm().
* tax_agg() now accepts a matrix in addition to a data.frame. Thanks to @tpoi
* tnrs() changes: Using httr instead of RCurl; now forcing splitting up name vector when long. Still issues when using POST requests (getpost="POST") wherein a request sent with 100 names only returns 30 for example. Investigating this now.

### NOTES

* Function name change: tp_acceptednames() now tp_accnames().
* Function name change: tp_namedistributions() now tp_dist().
* Function name change: tp_namereferences() now tp_refs().
* Internal ldfast() function changed name to taxize_ldfast() to avoid namespace conflicts with similar function in another package.
* Three functions now with ncbi_* prefix: get_seqs() is now ncbi_getbyname(); get_genes() is now ncbi_getbyid(); and get_genes_avail() is now ncbi_search().

taxize 0.1.5
===============

### NEW FEATURES

* classification() gains extension method classification.ids() to accept output from get_ids() - which attempts to get a taxonomic hierarchy from each of the taxon identifiers with the output from get_ids().
* synonyms() gains extension method synonyms.ids() to accept output from get_ids() - which attempts to get synonyms from each of the taxon identifiers with the output from get_ids().

taxize 0.1.4
===============

### NEW FEATURES

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

### MINOR IMPROVEMENTS

* itis_taxrank() now outputs a character, not a factor; loses parameter verbose, and gains ..., which passes on further arguments to gettaxonomicranknamefromtsn.
* tp_synonyms(), tp_summary(), plantminer(), itis_downstream(), gisd_isinvasive(), get_genes_avail(), get_genes(), eol_invasive(), eol_dataobjects(), andn tnrs() gain parameter verbose to optionally suppress messages.
* phylomatic_tree() format changed so that names are passed in normall (e.g., Poa annua) instead of the slashpath format (family/genus/genus_species). Also, taxaformat parameter dropped.
* itis_acceptname() gains ... to pass in further arguments to getacceptednamesfromtsn()
* tp_namedistributions() loses parameter format.
* get_tsn() and get_uid() return infomation about match as attribute.
* clarified iucn-documentation

### BUG FIXES

* Fixed bug in synonyms() so that further arguments can be passed on to get_tsn() to suppress messages.
* Removed test for ubio_classification_search(), a function that isn't operational yet.

taxize 0.1.1
===============

### NEW FEATURES

* New functions added just like get_uid()/get_tsn() but for EOL, Catalogue of Life, and Tropicos, see get_eolid(), get_colid(), and get_tpsid(), respectively.
* classification() methods added for EOL, Catalogue of Life, and Tropicos, see functions classification.eolid(), classification.colid(), and classification.tpsid() respectively.
* New function col_search() to search for names in the Catalogue of Life.
* User can turn off interactive mode in get_* functions. All get_* functions gain an ask argument, if TRUE (default) a user prompt is used for user to select which row they want, if FALSE, NA is returned when many results available; and added tests for the new argument. Affects downstream functions too.
* New function eol_invasive() to search EOL collections of invasive species lists.
* New function tp_search() to search for a taxonomic IDs from Tropicos.
* New function tp_classification() to get a taxonomic hierarchy from Tropicos.
* New function gbif_parse() to parse scientific names into their components, using the GBIF name parser API.
* New function itis_searchcommon() to search for common names across both searchbycommonnamebeginswith, and searchbycommonnameendswith.


### BUG FIXES

* tax_name() and other function broke, because get_tsn() and get_uid() returned wrong value when a taxon was not found. Fixed.


### MINOR IMPROVEMENTS

* Added tests for new classification() methods for EOL, COL, and Tropicos.
* Added tests for new functions tp_search() and tp_classification().

### NOTES

* Moved tests from inst/tests to tests/testthat according to new preferred location of tests.
* Updated CITATION in inst/ with our F1000Research paper info.
* Package repo name on Github changed from taxize_ to taxize - remember to use "taxize" in install_github() calls now instead of "taxize_"


taxize 0.1.0
===============

### NEW FEATURES

* New function tpl_families() to get data.frame of families from The Plantlist.org site.
* New function names_list() to get a random vector of species names using the
* Added two new data sets, plantGenusNames.RData and plantNames.RData, to be used in names_list().
* New function ldfast(), a replacement function for plyr::ldply that should be faster in all cases.
* Changed API key names to be more consistent, now tropicosApiKey, eolApiKey, ubioApiKey, and pmApiKey - do change these in your .Rprofile if you store them there.
* Added a startup message.

### MINOR IMPROVEMENTS

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

### BUG FIXES

* capwords() fxn changed to taxize_capwords() to avoid namespace conflicts with other packages with a similar function.
* ubio_namebank() was giving back base64 encoded data, now decoded appropriately.

### NOTES

* Added John Baumgartner as an author.

taxize 0.0.6
===============

### NEW FEATURES
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

### MINOR IMPROVEMENTS
* tnrs() parameter getpost changed from default of 'GET' to 'POST'
* Across all functions, the url parameter specifying an API endpoint was moved inside of functions (i.e., not available as a parameter in the function call)
* gnr_datasources() parameter todf=TRUE by default now, returning a data.frame
* col_classification() minor formatting improvements

### BUG FIXES
* iucn_summary() returns no information about population estimates.
* get_tsn() raised a warning in specific situations.
* tax_name() did not work for multiple ranks with ITIS.
* fixed errors in getfullhierarchyfromtsn()
* fixed errors in gethierarchydownfromtsn()
* fixed errors in getsynonymnamesfromtsn()
* fixed errors in searchforanymatch()
* fixed errors in searchforanymatchedpage()

### NOTES
* Removed dependency to NCBI2R
* Improvements of documentation
* Citation added

taxize 0.0.5
===============

### BUG FIXES
* removed tests for now until longer term fix is made so that web APIs that are temporarily down don't cause tests to fail.

taxize 0.0.4
===============

### BUG FIXES
* added R (>= 2.15.0) so that package tests don't fail on some systems due to paste0()
* remove test for ubio_namebank() function as it sometimes fails

taxize 0.0.3
===============

### BUG FIXES
* iucn_summary() does not break when API returns no information.
* tax_name() returns NA when taxon is not found on API.
* get_uid() asks for user input when more then one UID is found for a taxon.
* changed base URL for phylomatic_tree(), and associated parameter changes

### NEW FEATURES
* added check for invasive species status for a set of species from GISD database via gisd_isinvasive().
* Further development with the EOL-API: eol_dataobjects().
* added Catalogue of Life: col_classification(), col_children(), and col_downstream().
* new fxn get_genes(), retrieve gene sequences from NCBI by accession number.
* new functions to interact with the Phylotastic name resolution service: tnrs_sources() and tnrs()
* Added unit tests

### DEPRECATED AND DEFUNCT
* itis_name() fxn deprecated - use tax_name() instead


taxize 0.0.2
===============

### BUG FIXES

* changed paste0 to paste to avoid problems on certain platforms.
* removed all tests until the next version so that tests will not fail on any platforms.
* plyr was missing as import for iucn_summary fxn.

### NEW FEATURES

* added NEWS file.


taxize 0.0.1
===============

### NEW FEATURES

* released to CRAN
