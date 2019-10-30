context("ncbi_children")

test_that("ncbi_children returns correct class and result", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("ncbi_children", {
    tt <- ncbi_children(id = 4751)
    tt2 <- ncbi_children(id = 4751, out_type = 'uid')
  })

  expect_is(tt, "list")
  expect_is(tt[[1]], "data.frame")
  expect_equal(ncol(tt[[1]]), 3)
  expect_is(tt2, "list")
  expect_is(tt2[[1]], "character")
  expect_error(ncbi_children(name = 'Ilex', id = 4751))
  expect_equal(ncbi_children(name = NA)[[1]], NA)

  expect_equal(
    unname(sapply(ncbi_children('dragon', db='ncbi')[[1]], class)),
    c('character', 'character', 'character')
  )
})

test_that("ncbi_children does remove some ambiguous taxa", {
  skip_on_cran()
  vcr::use_cassette("ncbi_children_ambiguous", {
    # 28901 = "Salmonella enterica" - DOES NOT remove "subsp."
    subsp <- ncbi_children(id = 28901)
    # 2508041 = "unclassified Helianthus" - DOES remove "sp."
    sp <- ncbi_children(id = 2508041)
  })

  expect_is(subsp, "list")
  expect_is(subsp[[1]], "data.frame")
  expect_is(sp, "list")
  expect_is(sp[[1]], "data.frame")

  expect_gt(NROW(subsp[[1]]), 3)
  expect_equal(NROW(sp[[1]]), 0)
})
