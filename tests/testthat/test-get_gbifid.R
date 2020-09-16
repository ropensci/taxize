context("get_gbif")

test_that("get_gbif returns the correct value", {
  skip_on_cran()
  vcr::use_cassette("get_gbif", {
    z <- get_gbif(c("Chironomus riparius", "aaasdfadsfasdf"), 
      messages = FALSE, rows = 1)
    w <- get_gbif(c("Chironomus riparius", "Chaetopteryx"), 
      messages = FALSE, rows = 1)
  }, preserve_exact_body_bytes = TRUE)

  expect_is(z, "txid")
  expect_is(z, "gbif")
  expect_false(is.na(z[[1]]))
  expect_true(is.na(z[[2]]))
  
  expect_is(w, "txid")
  expect_is(w, "gbif")
  expect_false(is.na(w[[1]]))
  expect_false(is.na(w[[2]]))
})

test_that("get_gbif accepts ask-argument", {
  skip_on_cran()
  vcr::use_cassette("get_gbif_ask_arg", {
    a <- sw(get_gbif('Dugesia', ask = FALSE, messages = FALSE))
  })
  expect_true(is.na(a))
})

test_that("get_gbif method parameter works", {
  skip_on_cran()
  vcr::use_cassette("get_gbif_method_param", {
    mod1 <- get_gbif_(sci = "Cad*", method = "backbone", messages = FALSE, 
      rows = 1:100)
    mod2 <- get_gbif_("Cad*", method = "lookup", messages = FALSE, 
      rows = 1:100)
  }, preserve_exact_body_bytes = TRUE)

  expect_is(mod1, "list")
  expect_is(mod2, "list")
  expect_gt(NROW(mod2[[1]]), NROW(mod1[[1]]))
})

test_that("get_gbif phylum/class/order/family parameters work", {
  skip_on_cran()
  vcr::use_cassette("get_gbif_phylum_param", {
    aa <- get_gbif("Satyrium", phylum = "Tracheophyta", rows = 1, 
      messages = FALSE)
    bb <- get_gbif("Satyrium", phylum = "Arthropoda", rows = 1, 
      messages = FALSE)
  })

  expect_is(aa, "gbif")
  expect_is(aa, "txid")
  expect_is(bb, "gbif")
  expect_is(bb, "txid")
  expect_equal(txidac(aa), "5307264")
  expect_equal(txidac(bb), "1927718")
})

# test_that("get_gbif rank parameter works", {
#   skip_on_cran()

#   ## Rank example
#   rf1 <- get_gbif(sci = "Bison", rank = "genus", rows = 1, messages = FALSE)
#   rf2 <- get_gbif(sci = "Bison bison", rank = "species", rows = 1, messages = FALSE)

#   expect_is(rf1, "gbifid")
#   expect_is(rf2, "gbifid")
#   expect_false(is.na(rf1[[1]]))
#   expect_equal(rf2[[1]], "2441176")
# })

test_that("works regardless of character or numeric GGBIF ID given back", {
  skip_on_cran()
  vcr::use_cassette("get_gbif_class_doesnt_matter", {
    aa <- get_gbif("Chironomus riparius", messages = FALSE, rows = 1)
    bb <- get_gbif("Pinus contorta", messages = FALSE, rows = 1)
  })
  
  expect_is(aa, "gbif")
  expect_is(txidac(aa), "character")

  expect_is(bb, "gbif")
  expect_is(txidac(bb), "character")
})

test_that("get_gbif fails as expected", {
  skip_on_cran()

  expect_error(get_gbif(), "argument \"sci\" is missing")
  expect_error(get_gbif('Poa annua', ask = 4, messages = FALSE),
               "ask must be of class logical")
  expect_error(
    get_gbif("Satyrium", phylum = TRUE, messages = FALSE),
    "phylum must be of class character")
  expect_error(
    get_gbif("Satyrium", class = TRUE, messages = FALSE),
    "class must be of class character")
  expect_error(
    get_gbif("Satyrium", order = TRUE, messages = FALSE),
    "order must be of class character")
  expect_error(
    get_gbif("Satyrium", family = TRUE, messages = FALSE),
    "family must be of class character")
  expect_error(
    get_gbif("Satyrium", rank = TRUE, messages = FALSE),
    "rank must be of class character")
  expect_error(
    get_gbif("Satyrium", method = 55, messages = FALSE),
    "method must be of class character")

  # rows param
  expect_error(get_gbif("Satyrium", rows = "foobar", messages = FALSE),
               "rows must be of class numeric, integer")
  expect_error(get_gbif("Satyrium", rows = 0, messages = FALSE),
               "all\\(rows > 0\\) is not TRUE")
})


test_that("get_gbif works with state input", {
  skip_on_cran()
  
  taxon_clear()
  
  # species list
  # spp <- names_list("species", size = 3)
  spp <- c("Ilex zygophylla", "Astronia shungolensis", "Uromorus anthopophagorum")
  xx <- taxon_last()
  # make sure last taxon is null
  expect_null(xx)

  vcr::use_cassette("get_gbif_state", {
    res <- get_gbif(spp, messages = FALSE, rows = 1)
  }, preserve_exact_body_bytes = TRUE)

  # now it's not NULL
  expect_is(taxon_last(), "taxon_state")

  # & get_gbif output is all good
  expect_is(res, "gbif")

  # passing taxon_last to get_gbif: already done, so should be identical
  expect_identical(res, get_gbif(taxon_last()))
})
