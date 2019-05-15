context("gbif_downstream")

test_that("gbif_downstream works", {
  vcr::use_cassette("gbif_downstream", {
    aa <- gbif_downstream(key = 198, downto = "Genus")
    bb <- gbif_downstream(key = 1227, "Family")
  }, preserve_exact_body_bytes = TRUE)

  expect_is(aa, "data.frame")
  expect_is(aa$name, "character")
  expect_is(aa$rank, "character")
  expect_is(aa$key, "integer")
  expect_equal(unique(aa$rank), "genus")

  expect_is(bb, "data.frame")
  expect_is(bb$name, "character")
  expect_is(bb$rank, "character")
  expect_is(bb$key, "integer")
  expect_equal(unique(bb$rank), "family")
})

test_that("gbif_downstream intermediate param works", {
  vcr::use_cassette("gbif_downstream_intermediate", {
    cc <- gbif_downstream(key = 198, downto = "genus", intermediate = TRUE)
  }, preserve_exact_body_bytes = TRUE)

  expect_is(cc, "list")
  expect_is(cc$target, "data.frame")
  expect_is(cc$intermediate, "list")

  expect_is(cc$target$name, "character")
  expect_is(cc$target$rank, "character")
  expect_is(cc$target$key, "integer")

  expect_equal(unique(cc$target$rank), "genus")
})

test_that("gbif_downstream - limit and start params", {
  vcr::use_cassette("gbif_downstream_pagination", {
    aa <- gbif_downstream(2978223, "species", limit = 3)
    bb <- gbif_downstream(2978223, "species", limit = 3, start = 3)
  })

  expect_is(aa, "data.frame")
  expect_is(bb, "data.frame")
  expect_true(!all(aa$key %in% bb$key))
})

test_that("gbif_downstream fails well", {
  skip_on_cran()

  expect_error(gbif_downstream(198, "adfadf"), "'arg' should be one of")
  expect_error(gbif_downstream(198, "Genus", intermediate = "adf"),
               "'intermediate' should be of class 'logical'")
})
