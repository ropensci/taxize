context("tp_refs")

test_that("tp_refs returns the correct class", {
  vcr::use_cassette("tp_refs", {
    ttt <- suppressMessages(tp_refs(id = 25509881))
  })

	expect_that(ttt, is_a("data.frame"))
})
