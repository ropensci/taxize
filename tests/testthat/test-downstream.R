context("downstream")

test_that("downstream basic usage works", {
  skip_on_cran()
  vcr::use_cassette("downstream", {
    cc <- downstream("Ursus", db = "gbif", downto = "Species",
      messages = FALSE)
  }, preserve_exact_body_bytes = TRUE)

  expect_is(cc, "downstream")
  expect_named(cc, "Ursus")
  expect_is(cc$Ursus$rank, "character")
})

test_that("downstream - taxonomic id input", {
  skip_on_cran()
  vcr::use_cassette("downstream_id_input", {
    aa <- downstream(get_gbifid("Ursus", messages = FALSE),
      db = "gbif", downto = "Species")
  }, preserve_exact_body_bytes = TRUE)

  expect_is(aa, "downstream")
  expect_is(aa[[1]], "data.frame")
  expect_is(aa[[1]]$name_type, "character")
})

test_that("downstream - multiple data sources", {
  skip_on_cran()
  vcr::use_cassette("downstream_multiple_data_sources", {
    ids <- sw(get_ids("Ursus", db = c("gbif", 'itis'), rows = 1,
      messages = FALSE, suppress = TRUE))
    aa <- downstream(ids, downto = "Species")
  }, preserve_exact_body_bytes = TRUE)

  expect_is(aa, "downstream_ids")
  expect_is(aa[[1]], "downstream")
  expect_is(aa[[1]]$Ursus, "data.frame")
})

test_that("downstream - Use the rows parameter", {
  skip_on_cran()
  vcr::use_cassette("downstream_rows_param", {
    aa <- downstream("Hereroa", db = 'gbif', downto = "species",
      rows = 1, messages = FALSE)
  })

  expect_is(aa, "downstream")
  expect_is(aa[[1]], "data.frame")
  expect_is(aa[[1]]$name, "character")
})

test_that("downstream fails well", {
  skip_on_cran()

  expect_error(downstream("adfaf"), "Must specify downto")
  expect_error(downstream("Ursus", downto = "adfasdf"), "Must specify db")
  expect_error(downstream("Ursus", downto = "Species", db = "asdfdsf"),
               "the provided db value was not recognised")
})

test_that("warn on mismatch 'db'", {
  skip_on_cran()
  vcr::use_cassette("downstream_warn_on_db_mismatch", {
    expect_warning(
      downstream(
        get_gbifid('Apis', messages = FALSE, rows = 1), downto = "species",
        db = "itis"))
  })
})

test_that("downstream basic usage works", {
  skip_on_cran()
  vcr::use_cassette("downstream_bold", {
    cc <- downstream("Ursus", db = "bold", downto = "species",
      messages = FALSE)
  }, preserve_exact_body_bytes = TRUE)

  expect_is(cc, "downstream")
  expect_named(cc, "Ursus")
  expect_is(cc$Ursus$rank, "character")
})
