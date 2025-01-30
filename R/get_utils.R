make_generic <- function(x, uu, clz, check = TRUE) {
  if (check) {
    if ( evalfxn(clz)(x) ) {
      toid(x, uu, clz)
    } else {
      structure(
        NA, class = clz, match = "not found",
        multiple_matches = FALSE, pattern_match = FALSE, uri = NA
      )
    }
  } else {
    toid(x, uu, clz)
  }
}

make_wiki_generic <- function(x, uu, clz, check = TRUE) {
  if (check) {
    if ( evalfxn(clz)(sprintf(uu, x)) ) {
      toid(x, uu, clz)
    } else {
      structure(
        NA, class = clz, match = "not found",
        multiple_matches = FALSE, pattern_match = FALSE, uri = NA
      )
    }
  } else {
    toid(x, uu, clz)
  }
}

# messages
tx_msg_not_found <-
  "Not found. Consider checking the spelling or alternate classification"

# url templates for uri attributes of get_* functions
get_url_templates <- list(
  gbif = "https://www.gbif.org/species/%s",
  pow = "https://powo.science.kew.org/taxon/%s",
  tol = "https://tree.opentreeoflife.org/opentree/argus/ottol@%s",
  tropicos = "http://tropicos.org/Name/%s",
  worms = "http://www.marinespecies.org/aphia.php?p=taxdetails&id=%s",
  ncbi = "https://www.ncbi.nlm.nih.gov/taxonomy/%s",
  eol = "https://eol.org/pages/%s/",
  bold = "http://boldsystems.org/index.php/Taxbrowser_Taxonpage?taxid=%s",
  itis = "https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=%s",
  nbn = "https://species.nbnatlas.org/species/%s",
  natserv = "https://explorer.natureserve.org/Taxon/ELEMENT_GLOBAL.2.%s",
  iucn = iucn_base_url
)
