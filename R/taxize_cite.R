#' Get citations and licenses for data sources used in taxize
#'
#' @export
#' @param fxn Function to search on. A special case is the package name 'taxize' that will give the
#' citations for the package.
#' @param what One of citation (default), license, or both.
#' @examples
#' taxize_cite(fxn='eol_search')
#' taxize_cite(fxn='itis_hierarchy')
#' taxize_cite(fxn='tp_classification')
#' taxize_cite(fxn='gbif_ping')
#' taxize_cite(fxn='plantminer')
#'
#' # Functions that use many data sources
#' taxize_cite(fxn='synonyms')
#' taxize_cite(fxn='classification')
#'
#' # Get the taxize citation
#' taxize_cite(fxn='taxize')
#'
#' # Get license information
#' taxize_cite(fxn='taxize', "license")

taxize_cite <- function(fxn = "itis", what = 'citation'){
  what <- match.arg(what, c("citation","license","both"))
  if (what == "citation") {
    out <- data_citations(fxn)
    if (is.null(out)) {
      cat("Nothing found, try different inputs", "\n\n")
      cat("Can this citation be improved? https://github.com/ropensci/taxize/issues", "\n")
      cat("Please cite taxize in your paper: citation(package = 'taxize')")
    } else {
      if (fxn == 'taxize') {
        cat("The paper: ","\n")
        print(out[[1]])
        cat("\n")
        cat("The software: ","\n")
        print(out[[2]])
      } else {
        for (i in seq_along(out)) {
          cat(sprintf("Source: %s", names(out)[i]), "\n")
          cat(sprintf("  Home page: %s", out[[i]]$url_home), "\n")
          cat(sprintf("  API help: %s", out[[i]]$apidocs), "\n")
          cat(sprintf("  Citation: %s", out[[i]]$citation), "\n\n")
        }
        cat("Can any of these citations be improved? https://github.com/ropensci/taxize/issues")
      }
    }
  } else if (what == "license") {
    out <- data_licenses(fxn)
    if (is.null(out)) {
      cat("Unknown...")
    } else{
      cat(sprintf("License: %s", out$license), "\n")
      cat(sprintf("URL:     %s", out$url))
    }
  }
}

data_citations <- function(x){
  switch(x,
    itis_acceptname = list(itis = c_itis),
    itis_downstream = list(itis = c_itis),
    itis_getrecord = list(itis = c_itis),
    itis_hierarchy = list(itis = c_itis),
    itis_kingdomnames = list(itis = c_itis),
    itis_lsid = list(itis = c_itis),
    itis_name = list(itis = c_itis),
    itis_native = list(itis = c_itis),
    itis_ping = list(itis = c_itis),
    itis_refs = list(itis = c_itis),
    itis_taxrank = list(itis = c_itis),
    itis_terms = list(itis = c_itis),
    getacceptednamesfromtsn = list(itis = c_itis),
    getanymatchcount = list(itis = c_itis),
    getcommentdetailfromtsn = list(itis = c_itis),
    getcommonnamesfromtsn = list(itis = c_itis),
    getcoremetadatafromtsn = list(itis = c_itis),
    getcoveragefromtsn = list(itis = c_itis),
    getcredibilityratingfromtsn = list(itis = c_itis),
    getcredibilityratings = list(itis = c_itis),
    getcurrencyfromtsn = list(itis = c_itis),
    getdatedatafromtsn = list(itis = c_itis),
    getdescription = list(itis = c_itis),
    getexpertsfromtsn = list(itis = c_itis),
    getfullhierarchyfromtsn = list(itis = c_itis),
    getfullrecordfromlsid = list(itis = c_itis),
    getfullrecordfromtsn = list(itis = c_itis),
    getgeographicdivisionsfromtsn = list(itis = c_itis),
    getgeographicvalues = list(itis = c_itis),
    getglobalspeciescompletenessfromtsn = list(itis = c_itis),
    gethierarchydownfromtsn = list(itis = c_itis),
    gethierarchyupfromtsn = list(itis = c_itis),
    getitisterms = list(itis = c_itis),
    getitistermsfromcommonname = list(itis = c_itis),
    getitistermsfromscientificname = list(itis = c_itis),
    getjurisdictionaloriginfromtsn = list(itis = c_itis),
    getjurisdictionoriginvalues = list(itis = c_itis),
    getjurisdictionvalues = list(itis = c_itis),
    getkingdomnamefromtsn = list(itis = c_itis),
    getkingdomnames = list(itis = c_itis),
    getlastchangedate = list(itis = c_itis),
    getlsidfromtsn = list(itis = c_itis),
    getothersourcesfromtsn = list(itis = c_itis),
    getparenttsnfromtsn = list(itis = c_itis),
    getpublicationsfromtsn = list(itis = c_itis),
    getranknames = list(itis = c_itis),
    getrecordfromlsid = list(itis = c_itis),
    getreviewyearfromtsn = list(itis = c_itis),
    getscientificnamefromtsn = list(itis = c_itis),
    getsynonymnamesfromtsn = list(itis = c_itis),
    gettaxonauthorshipfromtsn = list(itis = c_itis),
    gettaxonomicranknamefromtsn = list(itis = c_itis),
    gettaxonomicusagefromtsn = list(itis = c_itis),
    gettsnbyvernacularlanguage = list(itis = c_itis),
    gettsnfromlsid = list(itis = c_itis),
    getunacceptabilityreasonfromtsn = list(itis = c_itis),
    getvernacularlanguages = list(itis = c_itis),
    searchbycommonname = list(itis = c_itis),
    searchbycommonnamebeginswith = list(itis = c_itis),
    searchbycommonnameendswith = list(itis = c_itis),
    searchbyscientificname = list(itis = c_itis),
    searchforanymatch = list(itis = c_itis),
    searchforanymatchpaged = list(itis = c_itis),
    apg_lookup = list(apg = c_apg),
    as.boldid = list(bold = c_bold),
    as.colid = list(col = c_col),
    as.eolid = list(eol = c_eol),
    as.gbifid = list(gbif = c_gbif),
    as.nbnid = list(nbn = c_nbn),
    as.tpsid = list(tropicos = c_tropicos),
    as.tsn = list(itis = c_itis),
    as.uid = list(ncbi = c_ncbi),
    bold_ping = list(bold = c_bold),
    bold_search = list(bold = c_bold),
    cbind.classification = list(none = c_none),
    cbind.classification_ids = list(none = c_none),
    children = list(itis = c_itis, col = c_col, ncbi = c_ncbi),
    class2tree = list(none = c_none),
    classification = list(itis = c_itis, col = c_col, ncbi = c_ncbi, gbif = c_gbif, eol = c_eol, troicos = c_tropicos, nbn = c_nbn),
    col_children = list(col = c_col),
    col_classification = list(col = c_col),
    col_downstream = list(col = c_col),
    col_ping = list(col = c_col),
    col_search = list(col = c_col),
    comm2sci = list(c_itis, c_ncbi, c_eol, c_tropicos),
    downstream = list(itis = c_itis, col = c_col),
    eol_dataobjects = list(eol = c_eol),
    eol_hierarchy = list(eol = c_eol),
    eol_invasive = list(eol = c_eol),
    eol_pages = list(eol = c_eol),
    eol_ping = list(eol = c_eol),
    eol_search = list(eol = c_eol),
    gbif_name_usage = list(gbif = c_gbif),
    gbif_parse = list(gbif = c_gbif),
    gbif_ping = list(gbif = c_gbif),
    genbank2uid = list(genbank = c_genbank),
    get_boldid = list(bold = c_bold),
    get_boldid_ = list(bold = c_bold),
    get_colid = list(col = c_col),
    get_colid_ = list(col = c_col),
    get_eolid = list(eol = c_eol),
    get_eolid_ = list(eol = c_eol),
    get_gbifid = list(gbif = c_gbif),
    get_gbifid_ = list(gbif = c_gbif),
    get_ids = list(itis = c_itis, col = c_col, ncbi = c_ncbi, gbif = c_gbif, eol = c_eol,
                   tropicos = c_tropicos, nbn = c_nbn),
    get_ids_ = list(itis = c_itis, col = c_col, ncbi = c_ncbi, gbif = c_gbif, eol = c_eol,
                    tropicos = c_tropicos, nbn = c_nbn),
    get_nbnid = list(nbn = c_nbn),
    get_nbnid_ = list(nbn = c_nbn),
    get_seqs = list(genbank = c_genbank),
    get_tpsid = list(tropicos = c_tropicos),
    get_tpsid_ = list(tropicos = c_tropicos),
    get_tsn = list(itis = c_itis),
    get_tsn_ = list(itis = c_itis),
    get_uid = list(ncbi = c_ncbi),
    get_uid_ = list(ncbi = c_ncbi),
    gisd_isinvasive = list(gisd = c_gisd),
    gni_details = list(gni = c_gni),
    gni_parse = list(gni = c_gni),
    gni_search = list(gni = c_gni),
    gnr_datasources = list(global_names = c_gnames),
    gnr_resolve = list(global_names = c_gnames),
    iplant_resolve = list(iplant = c_iplant),
    ipni_ping = list(ipni = c_ipni),
    ipni_search = list(ipni = c_ipni),
    iucn_getname = list(iucn = c_iucn),
    iucn_status = list(iucn = c_iucn),
    iucn_summary = list(iucn = c_iucn),
    names_list = list(theplantlist = c_plist),
    nbn_classification = list(nbn = c_nbn),
    nbn_ping = list(nbn = c_nbn),
    nbn_search = list(nbn = c_nbn),
    nbn_synonyms = list(nbn = c_nbn),
    ncbi_children = list(ncbi = c_ncbi),
    ncbi_get_taxon_summary = list(ncbi = c_ncbi),
    ncbi_getbyid = list(genbank = c_genbank),
    ncbi_getbyname = list(genbank = c_genbank),
    ncbi_ping = list(ncbi = c_ncbi),
    ncbi_search = list(genbank = c_genbank),
    phylomatic_format = list(phylomatic = c_phylomatic),
    phylomatic_tree = list(phylomatic = c_phylomatic),
    plantminer = list(plantminer = c_plantminer),
    rankagg = list(none = c_none),
    rbind.classification = list(none = c_none),
    rbind.classification_ids = list(none = c_none),
    resolve = list(global_names = c_gnames, iplant = c_iplant),
    sci2comm = list(itis = c_itis, ncbi = c_ncbi, eol = c_eol),
    scrapenames = list(global_names = c_gnames),
    synonyms = list(itis = c_itis, col = c_col, tropicos = c_tropicos, nbn = c_nbn),
    tax_agg = list(itis = c_itis, ncbi = c_ncbi),
    tax_name = list(itis = c_itis, ncbi = c_ncbi),
    tax_rank = list(itis = c_itis, ncbi = c_ncbi),
    tp_acceptednames = list(tropicos = c_tropicos),
    tp_accnames = list(tropicos = c_tropicos),
    tp_classification = list(tropicos = c_tropicos),
    tp_dist = list(tropicos = c_tropicos),
    tp_namedistributions = list(tropicos = c_tropicos),
    tp_namereferences = list(tropicos = c_tropicos),
    tp_refs = list(tropicos = c_tropicos),
    tp_search = list(tropicos = c_tropicos),
    tp_summary = list(tropicos = c_tropicos),
    tp_synonyms = list(tropicos = c_tropicos),
    tpl_families = list(theplantlist = c_plist),
    tpl_get = list(theplantlist = c_plist),
    tpl_search = list(theplantlist = c_plist),
    tropicos_ping = list(theplantlist = c_plist),
    upstream = list(c_itis, c_col),
    vascan_ping = list(vascan = c_vascan),
    vascan_search = list(vascan = c_vascan),
    taxize = citation("taxize")
  )
}

data_licenses <- function(x){
  switch(x,
    taxize = list(license = "MIT", url = "http://opensource.org/licenses/MIT")
  )
}

c_itis_url <- "http://www.itis.gov/"
c_itis_api <- "http://www.itis.gov/ws_description.html"
c_itis_citation <- "Retrieved [month, day, year], from the Integrated Taxonomic Information System on-line database, http://www.itis.gov."
c_itis <- list(url_home = c_itis_url, apidocs = c_itis_api, citation = c_itis_citation)
c_eol <- list(url_home = 'http://eol.org/', apidocs = 'http://eol.org/api/')
c_gbif = list(url_home = 'http://www.gbif.org', apidocs = 'http://www.gbif.org/developer/summary',
              citation = 'GBIF (2013). GBIF (Ed.), Global Biodiversity Information Facility Data Portal (2013)',
              dataset_citation = 'http://www.gbif.org/resources/2381')
c_tropicos <- list(url_home = "http://tropicos.org/", apidocs = "http://services.tropicos.org/")
c_plist <- list(url_home = "http://theplantlist.org/", apidocs = NULL,
                citation = "The Plant List (2013). Version 1.1. Published on the Internet; http://www.theplantlist.org/ ([accessed <date>).")
c_vascan <- list(url_home = "http://data.canadensys.net/explorer/en/search", apidocs = "http://data.canadensys.net/vascan/api",
                 citation = "Brouillet, L., F. Coursol, S.J. Meades, M. Favreau, M. Anions, P. Belisle & P. Desmet. 2010+. VASCAN, the Database of Vascular Plants of Canada. http://data.canadensys.net/vascan/ (consulted on <date>)")
c_plantminer <- list(url_home = "http://www.plantminer.com/", apidocs = "http://www.plantminer.com/help",
                     citation = 'See The Plant List or Tropicos citations')
c_phylomatic <- list(url_home = "http://phylodiversity.net/phylomatic/", apidocs = NULL,
                     citation = "Webb CO, Donoghue MJ (2005) Phylomatic: Tree assembly for applied phylogenetics. Mol. Ecol. Notes 5:181-183.")
c_nbn <- list(url_home = "http://www.nbn.org.uk/", apidocs = "https://data.nbn.org.uk/Documentation/Web_Services/",
              citation = NULL)
c_ncbi <- list(url_home = "http://www.ncbi.nlm.nih.gov/taxonomy", apidocs = "http://www.ncbi.nlm.nih.gov/books/NBK25501/",
               citation = "Federhen S: The NCBI Taxonomy database. Nucleic Acids Res 2012, 40 (Database issue): D136-D143.")
c_genbank <- list(url_home = "http://www.ncbi.nlm.nih.gov/genbank/", apidocs = "http://www.ncbi.nlm.nih.gov/books/NBK25501/",
                  citation = 'Bilofsky, Howard S., and Burks Christian. "The GenBank genetic sequence data bank." Nucleic acids research 16.5 (1988): 1861-1863.')
c_col <- list(url_home = "http://www.catalogueoflife.org/", apidocs = "http://www.catalogueoflife.org/content/web-services",
              citation = "See > http://www.catalogueoflife.org/col/info/cite")
c_iplant <- list(url_home = "http://tnrs.iplantcollaborative.org/", apidocs = "http://tnrs.iplantcollaborative.org/api.html",
              citation = 'Boyle, B. et al. 2013. The taxonomic name resolution service: an online tool for automated standardization of plant names. BMC bioinformatics 14:16. doi:10.1186/1471-2105-14-16')
c_gnames <- list(url_home = "http://gnrd.globalnames.org/", apidocs = "http://gnrd.globalnames.org/api", citation = NULL)
c_gni <- list(url_home = "http://gni.globalnames.org/", apidocs = "https://github.com/dimus/gni/wiki/api", citation = NULL)
c_bold <- list(url_home = "http://www.boldsystems.org/", apidocs = "http://www.boldsystems.org/index.php/resources/api",
               citation = "Ratnasingham, S., & Hebert, P. D. (2007). BOLD: The Barcode of Life Data System (http://www. barcodinglife. org). Molecular ecology notes, 7(3), 355-364.")
c_iucn <- list(url_home = "http://www.iucn.org/", apidocs = NULL,
               citation = "IUCN 2015. The IUCN Red List of Threatened Species. Version 2015.1. <http://www.iucnredlist.org>. Downloaded on <date>.")
c_ipni <- list(url_home = "http://www.ipni.org/", apidocs = "http://www.ipni.org/ipni/advPlantNameSearch.do",
               citation = "The International Plant Names Index (<year>). Published on the Internet http://www.ipni.org [accessed <date>]")
c_apg <- list(url_home = "http://www.mobot.org/MOBOT/research/APweb/", apidocs = NULL,
              citation = "Stevens, P. F. (<year>). Angiosperm Phylogeny Website. Version 13, July 2012.")
c_gisd <- list(url_home = "http://www.issg.org/database/welcome/", apidocs = NULL, citation = NULL)
c_none <- list(url_home = "no data source", apidocs = "no data source", citation = "no data source")
