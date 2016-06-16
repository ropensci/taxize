# tests for downstream fxn in taxize
context("downstream")

test_that("downstream basic usage works", {
  skip_on_cran()

  aa <- downstream("015be25f6b061ba517f495394b80f108", db = "col", downto = "Species")
  cc <- downstream("Ursus", db = 'gbif', downto = 'Species', verbose = FALSE)

  expect_is(aa, "downstream")
  expect_is(cc, "downstream")

  expect_named(aa, "015be25f6b061ba517f495394b80f108")
  expect_named(cc, "Ursus")

  expect_is(aa$`015be25f6b061ba517f495394b80f108`$childtaxa_id, "character")
  expect_is(cc$Ursus$rank, "character")
})

test_that("downstream - many names input", {
  skip_on_cran()

  aa <- downstream(c("015be25f6b061ba517f495394b80f108", "6df38b73c53ce9e2982f3e1883305fc4"),
                   db = "col", downto = 'Species', verbose = FALSE)

  expect_is(aa, "downstream")
  expect_named(aa, c("015be25f6b061ba517f495394b80f108", "6df38b73c53ce9e2982f3e1883305fc4"))
})

test_that("downstream - taxonomic id input", {
  skip_on_cran()

  aa <- downstream(get_gbifid("Ursus", verbose = FALSE), db = 'gbif', downto = 'Species')

  expect_is(aa, "downstream")
  expect_is(aa[[1]], "data.frame")
  expect_is(aa[[1]]$canonicalname, "character")
})

test_that("downstream - multiple data sources", {
  skip_on_cran()

  ids <- get_ids("Ursus", db = c('gbif', 'itis'), verbose = FALSE)
  aa <- downstream(ids, downto = 'Species')

  expect_is(aa, "downstream_ids")
  expect_is(aa[[1]], "downstream")
  expect_is(aa[[1]]$Ursus, "data.frame")
})

test_that("downstream - Use the rows parameter", {
  skip_on_cran()

  aa <- downstream("Carya", db = 'col', downto = "Species", rows = 1, verbose = FALSE)

  expect_is(aa, "downstream")
  expect_is(aa[[1]], "data.frame")
  expect_is(aa[[1]]$childtaxa_id, "character")
})

test_that("downstream fails well", {
  skip_on_cran()

  expect_error(downstream("adfaf"), "Must specify downto")
  expect_error(downstream("Ursus", downto = "adfasdf"), "Must specify db")
  expect_error(downstream("Ursus", downto = "Species", db = "asdfdsf"),
               "the provided db value was not recognised")
})
