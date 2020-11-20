test_that("taxa_taxon", {
  # empty
  a <- taxa_taxon()
  expect_is(a, c("txid", "taxa_taxon", "vctrs_rcrd", "vctrs_vctr"))
  expect_length(a, 0)
  expect_named(a, NULL) # not named

  # extract parts of ids
  ## fxns from taxa2
  rank <- taxa2::tax_rank(a)
  expect_is(rank, "taxa_taxon_rank")
  expect_length(as.character(rank), 0)

  id <- taxa2::tax_id(a)
  expect_is(id, "taxa_taxon_id")
  expect_length(as.character(id), 0)

  name <- taxa2::tax_name(a)
  expect_is(name, "character")
  expect_length(name, 0)

  db <- taxa2::tax_db(a)
  expect_is(db, "taxa_taxon_db")
  expect_length(as.character(db), 0)

  ## fxns from taxize
  expect_length(txz_uri(a), 0)
  expect_length(txz_match(a), 0)
  expect_length(txz_mm(a), 0)
  expect_length(txz_pm(a), 0)

  # coercion to data.frame
  adf <- as.data.frame(a)
  expect_is(adf, "data.frame")
  expect_equal(dim(adf), c(0, 8))


  # all parts filled
  b <- taxa_taxon("Puma concolor", rank = "species",
    id = taxa2::taxon_id("34567", db = "gbif"),
    uri = sprintf(get_url_templates$gbif, 34567),
    match = "found", multiple_matches = FALSE, pattern_match = TRUE,
    class = "gbif")
  expect_is(b, c("txid", "taxa_taxon", "vctrs_rcrd", "vctrs_vctr"))
  expect_length(b, 1)
  expect_named(b, NULL) # not named
  expect_error(as.character(b)) # can't coerce to character
  
  # extract parts of ids
  ## fxns from taxa2
  rank <- taxa2::tax_rank(b)
  expect_is(rank, "taxa_taxon_rank")
  expect_equal(as.character(rank), "species")

  id <- taxa2::tax_id(b)
  expect_is(id, "taxa_taxon_id")
  expect_equal(as.character(id), "34567")

  name <- taxa2::tax_name(b)
  expect_is(name, "character")
  expect_equal(name, "Puma concolor")

  db <- taxa2::tax_db(b)
  expect_is(db, "taxa_taxon_db")
  expect_equal(as.character(db), "gbif")

  ## fxns from taxize
  uri <- txz_uri(b)
  expect_is(uri, "character")
  expect_match(uri, "https")

  match <- txz_match(b)
  expect_is(uri, "character")
  expect_equal(match, "found")

  multiple_matches <- txz_mm(b)
  expect_is(multiple_matches, "logical")
  expect_false(multiple_matches)

  pattern_match <- txz_pm(b)
  expect_is(pattern_match, "logical")
  expect_true(pattern_match)

  # coercion to data.frame
  bdf <- as.data.frame(b)
  expect_is(bdf, "data.frame")
  expect_equal(dim(bdf), c(1, 8))
})


test_that("taxa_taxon - we can combine get_ and as. outputs", {
  # two "get_" outputs
  d <- get_gbif(sci='Puma concolor', messages=FALSE)
  e <- get_gbif(sci='Homo sapiens', messages=FALSE)
  expect_is(d, c("txid", "gbif"))
  expect_is(e, c("txid", "gbif"))
  expect_length(d, 1)
  expect_length(e, 1)
  de <- c(d, e)
  expect_is(de, c("txid", "gbif"))
  expect_length(de, 2)

  # a "get_" output and an "as." output
  f <- as.gbif(2436436)
  expect_is(f, c("txid", "gbif"))
  df <- c(d, f)
  expect_is(df, c("txid", "gbif"))
  expect_length(df, 2)
  df_ids <- as.character(taxa2::tax_id(df))
  expect_length(df_ids, 2)
  expect_is(df_ids, "character")

  # two "get_" outputs of different data sources
  g <- get_ncbi("Abies magnifica", messages=FALSE)
  expect_is(g, c("txid", "ncbi"))
  dg <- c(d, g)
  dg_dbs <- as.character(taxa2::tax_db(dg))
  expect_is(dg_dbs, "character")
  expect_equal(dg_dbs, c("gbif", "ncbi"))
})
