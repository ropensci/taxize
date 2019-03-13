context("downstream")

test_that("downstream basic usage works", {
  vcr::use_cassette("downstream", {
    aa <- downstream("015be25f6b061ba517f495394b80f108", 
      db = "col", downto = "Species")
    cc <- downstream("Ursus", db = "gbif", downto = "Species", 
      messages = FALSE)
  })

  expect_is(aa, "downstream")
  expect_is(cc, "downstream")

  expect_named(aa, "015be25f6b061ba517f495394b80f108")
  expect_named(cc, "Ursus")

  expect_is(aa$`015be25f6b061ba517f495394b80f108`$childtaxa_id, "character")
  expect_is(cc$Ursus$rank, "character")
})

test_that("downstream - many names input", {
  ids <- c("015be25f6b061ba517f495394b80f108", 
    "6df38b73c53ce9e2982f3e1883305fc4")

  vcr::use_cassette("downstream_names", {
    aa <- downstream(ids, db = "col", downto = "Species", messages = FALSE)
  })

  expect_is(aa, "downstream")
  expect_named(aa, ids)
})

test_that("downstream - taxonomic id input", {
  vcr::use_cassette("downstream_id_input", {
    aa <- downstream(get_gbifid("Ursus", messages = FALSE), 
      db = "gbif", downto = "Species")
  })

  expect_is(aa, "downstream")
  expect_is(aa[[1]], "data.frame")
  expect_is(aa[[1]]$name_type, "character")
})

test_that("downstream - multiple data sources", {
  vcr::use_cassette("downstream_multiple_data_sources", {
    ids <- sw(get_ids("Ursus", db = c("gbif", 'itis'), rows = 1, 
      messages = FALSE))
    aa <- downstream(ids, downto = "Species")
  })

  expect_is(aa, "downstream_ids")
  expect_is(aa[[1]], "downstream")
  expect_is(aa[[1]]$Ursus, "data.frame")
})

test_that("downstream - Use the rows parameter", {
  vcr::use_cassette("downstream_rows_param", {
    aa <- downstream("Hereroa", db = 'col', downto = "species", 
      rows = 1, messages = FALSE)
  })

  expect_is(aa, "downstream")
  expect_is(aa[[1]], "data.frame")
  expect_is(aa[[1]]$childtaxa_id, "character")
})

test_that("downstream - Works with COL which previously failed
          due to lack of infraspecies value in rank_ref dataset", {
  vcr::use_cassette("downstream_col_infraspecies_problem", {
    x <- as.colid("d324f3777e98688584cf8b68d0f06e5f", FALSE)
    aa <- downstream(x, db = 'col', downto = "suborder", messages = FALSE)
  })

  expect_is(aa, "downstream")
  expect_is(aa[[1]], "data.frame")
  expect_equal(NROW(aa[[1]]), 0)
})

test_that("downstream fails well", {
  skip_on_cran()

  expect_error(downstream("adfaf"), "Must specify downto")
  expect_error(downstream("Ursus", downto = "adfasdf"), "Must specify db")
  expect_error(downstream("Ursus", downto = "Species", db = "asdfdsf"),
               "the provided db value was not recognised")
})
