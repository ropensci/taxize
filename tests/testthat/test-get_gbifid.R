context("get_gbifid")

test_that("get_gbifid returns the correct value", {
  skip_on_cran()

  expect_true(is.na(get_gbifid(c("Chironomus riparius", "aaa"), verbose = FALSE)[2]))
})

test_that("get_gbifid returns the correct class", {
  skip_on_cran()

  expect_is(get_gbifid(c("Chironomus riparius", "Chaetopteryx"), verbose = FALSE), "gbifid")
})

test_that("get_gbifid accepts ask-argument", {
  skip_on_cran()

  expect_true(is.na(get_gbifid('Dugesia', ask = FALSE, verbose = FALSE)))
})

test_that("get_gbifid method parameter works", {
  skip_on_cran()

  ### w/ method = backbone
  mod1 <- get_gbifid_(sciname = "Z*", method = "backbone", verbose = FALSE, rows = 1:100)
  mod2 <- get_gbifid_("Z*", method = "lookup", verbose = FALSE, rows = 1:100)

  expect_is(mod1, "list")
  expect_is(mod2, "list")
  expect_gt(NROW(mod2[[1]]), NROW(mod1[[1]]))
})

test_that("get_gbifid phylum/class/order/family parameters work", {
  aa <- get_gbifid("Satyrium", phylum = "Tracheophyta", rows = 1, verbose = FALSE)
  bb <- get_gbifid("Satyrium", phylum = "Arthropoda", rows = 1, verbose = FALSE)

  expect_is(aa, "gbifid")
  expect_is(bb, "gbifid")
  expect_equal(aa[[1]], "5307264")
  expect_equal(bb[[1]], "1927718")
})

test_that("get_gbifid rank parameter works", {
  skip_on_cran()

  ## Rank example
  rf1 <- get_gbifid(sciname = "bison bison", rank = "genus", rows = 1, verbose = FALSE)
  rf2 <- get_gbifid(sciname = "bison bison", rank = "species", rows = 1, verbose = FALSE)

  expect_is(rf1, "gbifid")
  expect_is(rf2, "gbifid")
  expect_false(is.na(rf1[[1]]))
  expect_equal(rf2[[1]], "2441176")
})

test_that("works regardless of character or numeric GGBIF ID given back", {
  skip_on_cran()

  aa <- get_gbifid("Chironomus riparius", verbose = FALSE)
  expect_is(aa, "gbifid")
  expect_is(aa[[1]], "character")

  bb <- get_gbifid("Pinus contorta", verbose = FALSE, rows = 1)
  expect_is(bb, "gbifid")
  expect_is(bb[[1]], "character")
})
