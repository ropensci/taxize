context("itis_ping")

test_that("itis_ping returns the correct class", {
  vcr::use_cassette("itis_ping", {
    one <- itis_ping()
  })

  expect_is(one, "logical")
})

test_that("itis_ping returns correct things", {
  vcr::use_cassette("itis_ping_http_code_503", {
    expect_false(itis_ping(503))
  })

  vcr::use_cassette("itis_ping_http_code_200", {
    expect_true(itis_ping(200))
  })
})
