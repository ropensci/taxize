context("worms_downstream")

test_that("worms_downstream works", {
  vcr::use_cassette("worms_downstream", {
    aa <- suppressWarnings(worms_downstream(id = 125732, downto = "species"))
  })

  expect_is(aa, "data.frame")
  expect_type(aa$id, "integer")
  expect_is(aa$name, "character")
  expect_is(aa$rank, "character")

  expect_equal(unique(aa$rank), "species")
})

test_that("worms_downstream intermediate param works", {
  vcr::use_cassette("worms_downstream_intermediate_param", {
    cc <- suppressWarnings(worms_downstream(id = 125732, downto = "species", 
      intermediate = TRUE))
  })

  expect_is(cc, "list")
  expect_is(cc$target, "data.frame")
  expect_is(cc$intermediate, "list")

  expect_is(cc$target$name, "character")
  expect_is(cc$target$rank, "character")
  expect_is(cc$target$id, "integer")

  expect_equal(unique(cc$target$rank), "species")
})

test_that("worms_downstream fails well", {
  skip_on_cran()

  expect_error(suppressWarnings(worms_downstream(198, "adfadf")),
    "'arg' should be one of")
  expect_error(
    suppressWarnings(worms_downstream(198, "Genus", intermediate = "adf")),
               "'intermediate' should be of class 'logical'")
})

test_that("worms_downstream - start param", {
  vcr::use_cassette("worms_downstream_start_param", {
    aa <- suppressWarnings(worms_downstream(125732, "species", start = 1))
    bb <- suppressWarnings(worms_downstream(125732, "species", start = 3))
  })

  expect_is(aa, "data.frame")
  expect_is(bb, "data.frame")
  expect_is(aa$name, "character")
  expect_match(bb$name[1], "Gadus")
})
