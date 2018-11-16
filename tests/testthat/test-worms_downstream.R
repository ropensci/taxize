context("worms_downstream")

test_that("worms_downstream works", {
  skip_on_cran()

    aa <- worms_downstream(id = 125732, downto = "species")

    expect_is(aa, "data.frame")
    expect_type(aa$id, "integer")
    expect_is(aa$name, "character")
    expect_is(aa$rank, "character")

    expect_equal(unique(aa$rank), "species")
})

test_that("worms_downstream intermediate param works", {
  skip_on_cran()

  cc <- worms_downstream(id = 125732, downto = "species", intermediate = TRUE)

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

  expect_error(worms_downstream(198, "adfadf"), "'arg' should be one of")
  expect_error(worms_downstream(198, "Genus", intermediate = "adf"),
               "'intermediate' should be of class 'logical'")
})

test_that("worms_downstream - start param", {
  skip_on_cran()

  aa <- worms_downstream(125732, "species", start = 1)
  bb <- worms_downstream(125732, "species", start = 3)

  expect_is(aa, "data.frame")
  expect_is(bb, "data.frame")
  expect_true(aa$name[2] == "Gadus aeglefinus")
  expect_true(bb$name[1] == "Gadus aeglefinus")
})
