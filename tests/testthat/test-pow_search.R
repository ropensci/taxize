context("pow_search")
test_that("pow_search returns the correct class", {
  vcr::use_cassette("pow_search", {
    one <- pow_search(q = "Quercus")
  }, preserve_exact_body_bytes = TRUE)

  expect_is(one, "list")
  expect_named(one, c("meta", "data"))
  expect_is(one$meta, "list")
  expect_is(one$meta$cursor, "character")
  expect_is(one$data, "data.frame")
})

test_that("pow_search fails well", {
  skip_on_cran()

  expect_error(pow_search(), "\"q\" is missing")
  expect_error(pow_search(4), "q must be of class")
  expect_error(pow_search("foo", "bar"), "limit must be of class")
  expect_error(pow_search("foo", 1, 5), "cursor must be of class")
  expect_error(pow_search("foo", 1, "bar", 5), "sort must be of class")
})


context("pow_lookup")
test_that("pow_lookup works", {
  vcr::use_cassette("pow_lookup", {
    one <- pow_lookup(id = "urn:lsid:ipni.org:names:320035-2")
  }, preserve_exact_body_bytes = TRUE)

  expect_is(one, "list")
  expect_named(one, c("meta", "data"))
  expect_is(one$meta, "list")
  expect_is(one$meta$synonyms, "data.frame")
  expect_null(one$meta$distribution)
  expect_null(one$data)
})

test_that("pow_lookup works", {
  vcr::use_cassette("pow_lookup_include_param", {
    one <- pow_lookup(id = "urn:lsid:ipni.org:names:320035-2",
      include = "distribution")
  }, preserve_exact_body_bytes = TRUE)

  expect_is(one, "list")
  expect_named(one, c("meta", "data"))
  expect_is(one$meta, "list")
  expect_is(one$meta$distribution, "list")
  expect_is(one$meta$distribution$natives, "data.frame")
  expect_is(one$meta$distribution$introduced, "data.frame")
  expect_null(one$data)
})

test_that("pow_lookup returns the correct class", {
  skip_on_cran()

  expect_error(pow_lookup(), "\"id\" is missing")
  expect_error(pow_lookup("asdfds", 5), "include must be of class")
  expect_error(
    pow_lookup(id = "urn:lsid:ipni.org:names:320035-2", include = "foo"),
    "'include' must be one of"
  )
})
