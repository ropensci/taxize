context("id2name")

test_that("id2name: itis", {
  skip_on_cran()

  aa <- id2name(19322, db = "itis")

  expect_is(aa, "id2name")
  expect_equal(attr(aa, "db"), "tsn")
  expect_is(unclass(aa), "list")
  expect_is(aa[[1]], "data.frame")
  expect_named(aa[[1]], c('id', 'name', 'rank', 'status', 'parent_tsn'))
  expect_is(aa[[1]]$name, "character")
})

test_that("id2name: tol", {
  skip_on_cran()

  aa <- id2name(515698, db = "tol")

  expect_is(aa, "id2name")
  expect_equal(attr(aa, "db"), "tol")
  expect_is(unclass(aa), "list")
  expect_is(aa[[1]], "data.frame")
  expect_is(aa[[1]]$name, "character")
})

test_that("missing/wrong data given returns result", {
  skip_on_cran()

  expect_error(id2name(), "Must specify db")
  expect_error(id2name(db = "tol"), "argument \"id\" is missing")
  expect_error(id2name(5, db = "stuff"), "'db' must be one of")
  # requires HTTP request
  expect_warning(id2name(55555555, db = "tol"), "Bad Request")
  expect_equal(NROW(suppressWarnings(id2name(55555555, db = "tol"))[[1]]), 0)
})

test_that("warn on mismatch 'db'", {
  skip_on_cran()
  vcr::use_cassette("id2name_warn_on_db_mismatch", {
    expect_warning(
      id2name(
        get_uid('Apis', messages = FALSE), db = "gbif"))
  })
})
