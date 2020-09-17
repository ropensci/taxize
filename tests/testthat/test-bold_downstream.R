test_that("bold_downstream", {
  skip_on_cran()
  vcr::use_cassette("bold_downstream", {
    aa <- bold_downstream(id = 3451, downto="species")
  }, preserve_exact_body_bytes = TRUE)

  expect_is(aa, "data.frame")
  expect_is(aa$name, "character")
  expect_is(aa$rank, "character")
  expect_is(aa$id, "character")
  expect_equal(unique(aa$rank), "species")
})

test_that("bold_downstream intermediate param works", {
  skip_on_cran()
  vcr::use_cassette("bold_downstream_intermediate", {
    cc <- bold_downstream(id = 443, downto="genus", intermediate=TRUE)
  }, preserve_exact_body_bytes = TRUE)

  expect_is(cc, "list")
  expect_is(cc$target, "data.frame")
  expect_is(cc$intermediate, "list")

  expect_is(cc$target$name, "character")
  expect_is(cc$target$rank, "character")
  expect_is(cc$target$id, "character")

  expect_equal(unique(cc$target$rank), "genus")
})

test_that("bold_downstream fails well", {
  skip_on_cran()

  expect_error(bold_downstream(198, "adfadf"), "'arg' should be one of")
  expect_error(bold_downstream(198, "Genus", intermediate = "adf"),
               "'intermediate' should be of class 'logical'")
})
