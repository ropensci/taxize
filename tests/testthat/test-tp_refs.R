context("tp_refs")

test_that("tp_refs returns the correct class", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("tp_refs", {
    ttt <- suppressMessages(tp_refs(id = 25509881))
  }, preserve_exact_body_bytes = TRUE)

	expect_that(ttt, is_a("data.frame"))
})
