context("get_gbifid")

test_that("get_gbifid returns the correct value", {
  vcr::use_cassette("get_gbifid", {
    z <- get_gbifid(c("Chironomus riparius", "aaasdfadsfasdf"), 
      messages = FALSE)[2]
    w <- get_gbifid(c("Chironomus riparius", "Chaetopteryx"), 
      messages = FALSE)
  }, preserve_exact_body_bytes = TRUE)

  expect_true(is.na(z))
  expect_is(w, "gbifid")
})

test_that("get_gbifid accepts ask-argument", {
  vcr::use_cassette("get_gbifid_ask_arg", {
    a <- sw(get_gbifid('Dugesia', ask = FALSE, messages = FALSE))
  })
  expect_true(is.na(a))
})

test_that("get_gbifid method parameter works", {
  vcr::use_cassette("get_gbifid_method_param", {
    mod1 <- get_gbifid_(sciname = "Z*", method = "backbone", messages = FALSE, 
      rows = 1:100)
    mod2 <- get_gbifid_("Z*", method = "lookup", messages = FALSE, 
      rows = 1:100)
  }, preserve_exact_body_bytes = TRUE)

  expect_is(mod1, "list")
  expect_is(mod2, "list")
  expect_gt(NROW(mod2[[1]]), NROW(mod1[[1]]))
})

test_that("get_gbifid phylum/class/order/family parameters work", {
  vcr::use_cassette("get_gbifid_phylum_param", {
    aa <- get_gbifid("Satyrium", phylum = "Tracheophyta", rows = 1, 
      messages = FALSE)
    bb <- get_gbifid("Satyrium", phylum = "Arthropoda", rows = 1, 
      messages = FALSE)
  })

  expect_is(aa, "gbifid")
  expect_is(bb, "gbifid")
  expect_equal(aa[[1]], "5307264")
  expect_equal(bb[[1]], "1927718")
})

# test_that("get_gbifid rank parameter works", {
#   skip_on_cran()

#   ## Rank example
#   rf1 <- get_gbifid(sciname = "Bison", rank = "genus", rows = 1, messages = FALSE)
#   rf2 <- get_gbifid(sciname = "Bison bison", rank = "species", rows = 1, messages = FALSE)

#   expect_is(rf1, "gbifid")
#   expect_is(rf2, "gbifid")
#   expect_false(is.na(rf1[[1]]))
#   expect_equal(rf2[[1]], "2441176")
# })

test_that("works regardless of character or numeric GGBIF ID given back", {
  vcr::use_cassette("get_gbifid_class_doesnt_matter", {
    aa <- get_gbifid("Chironomus riparius", messages = FALSE)
    bb <- get_gbifid("Pinus contorta", messages = FALSE, rows = 1)
  })
  
  expect_is(aa, "gbifid")
  expect_is(aa[[1]], "character")

  expect_is(bb, "gbifid")
  expect_is(bb[[1]], "character")
})

test_that("get_gbifid fails as expected", {
  skip_on_cran()

  expect_error(get_gbifid(), "argument \"sciname\" is missing")
  expect_error(get_gbifid('Poa annua', ask = 4, messages = FALSE),
               "ask must be of class logical")
  expect_error(
    get_gbifid("Satyrium", phylum = TRUE, messages = FALSE),
    "phylum must be of class character")
  expect_error(
    get_gbifid("Satyrium", class = TRUE, messages = FALSE),
    "class must be of class character")
  expect_error(
    get_gbifid("Satyrium", order = TRUE, messages = FALSE),
    "order must be of class character")
  expect_error(
    get_gbifid("Satyrium", family = TRUE, messages = FALSE),
    "family must be of class character")
  expect_error(
    get_gbifid("Satyrium", rank = TRUE, messages = FALSE),
    "rank must be of class character")
  expect_error(
    get_gbifid("Satyrium", method = 55, messages = FALSE),
    "method must be of class character")

  # rows param
  expect_error(get_gbifid("Satyrium", rows = "foobar", messages = FALSE),
               "rows must be of class numeric, integer")
  expect_error(get_gbifid("Satyrium", rows = 0, messages = FALSE),
               "all\\(rows > 0\\) is not TRUE")
})


test_that("get_gbifid works with state input", {
  taxon_clear()
  
  # species list
  # spp <- names_list("species", size = 3)
  spp <- c("Ilex zygophylla", "Astronia shungolensis", "Uromorus anthopophagorum")
  xx <- taxon_last()
  # make sure last taxon is null
  expect_null(xx)

  vcr::use_cassette("get_gbifid_state", {
    res <- get_gbifid(spp, messages = FALSE)
  }, preserve_exact_body_bytes = TRUE)

  # now it's not NULL
  expect_is(taxon_last(), "taxon_state")

  # & get_gbifid output is all good
  expect_is(res, "gbifid")

  # passing taxon_last to get_gbifid: already done, so should be identical
  expect_identical(res, get_gbifid(taxon_last()))
})
