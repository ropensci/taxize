context("comm2sci")

test_that("comm2sci returns the correct values and classes", {
  skip_on_cran()

  vcr::use_cassette("comm2sci", {
    tt <- suppressMessages(comm2sci(com='american black bear'))
    uu <- suppressMessages(comm2sci('annual blue grass', 
      db='tropicos'))
    temp2 <- suppressMessages(comm2sci(com='black bear', db='tropicos'))
    # FIXME: skip EOL for now until they're bad cert issue sorted out
    # temp3 <- suppressMessages(comm2sci(com=c('black bear','roe deer'),
    #   db='eol'))
  }, preserve_exact_body_bytes = TRUE)

  expect_equal(names(tt), 'american black bear')
  expect_equal(names(uu), 'annual blue grass')

  expect_identical(
    suppressMessages(
      comm2sci(com='bear', db='itis', itisby = "asfasdf"))[[1]], 
    character(0))
  expect_named(temp2)
  # expect_named(temp3)

  expect_that(tt, is_a("list"))
  expect_that(tt[[1]], is_a("character"))
  expect_that(uu, is_a("list"))
  expect_that(uu[[1]], is_a("character"))
  expect_is(
    suppressMessages(
      comm2sci(com='black bear', db='itis', simplify = FALSE))[[1]],
    "data.frame")
})

test_that("comm2sci fails well", {
  skip_on_cran()

  expect_error(comm2sci(5), "com must be of class character")
  expect_error(comm2sci(list()), "com must be of class character")
  expect_error(comm2sci(mtcars), "com must be of class character")

  expect_error(comm2sci("bear", db = "adsf"),
               "'db' must be one of 'ncbi', 'itis', 'tropicos', 'eol', 'worms'")

  expect_error(comm2sci("bear", simplify = "Asdf"),
               "simplify must be of class logical")

  # no results if itisby is not a valid value, doesn't error though
  expect_equal(length(comm2sci('bear', db='itis', itisby = "asdff")[[1]]), 0)
})

test_that("warn on mismatch 'db'", {
  skip_on_cran()
  vcr::use_cassette("comm2sci_warn_on_db_mismatch", {
    expect_warning(
      comm2sci(
        get_uid("Chironomus riparius", messages = FALSE), db = "itis"))
  })
})
