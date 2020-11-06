context("sci2comm")

test_that("sci2comm returns the correct value", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("sci2comm", {
    tt <- sci2comm(sci = 'Helianthus annuus', db = 'ncbi', messages = FALSE)
    uu <- sw(sci2comm(sci = 'Helianthus annuus', db = 'itis', rows = 1,
                      messages = FALSE))
    zz <- sci2comm(sci = 'Pomatomus saltatrix', db = 'worms',
                   messages = FALSE)
  })

  expect_named(tt, 'Helianthus annuus')
  expect_named(uu, 'Helianthus annuus')
  expect_named(zz, 'Pomatomus saltatrix')

  expect_is(tt, "list")
  expect_is(tt[[1]], "character")

  expect_is(uu, "list")
  expect_is(uu[[1]], "character")

  expect_is(zz, "list")
  expect_is(zz[[1]], "character")
})

test_that("warn on mismatch 'db'", {
  skip_on_cran()
  # vcr::use_cassette("sci2comm_warn_on_db_mismatch", {
  #   expect_warning(
  #     sci2comm(
  #       get_tsn('Helianthus annuus', messages = FALSE), db = "ncbi"))
  # })
})
