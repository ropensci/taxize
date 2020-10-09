context("ncbi_downstream")

test_that("ncbi_downstream returns correct structure", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("ncbi_downstream", {
    tt <- ncbi_downstream(id = 7459, downto="species")
  })

  expect_is(tt, "data.frame")
  expect_equal(NCOL(tt), 3)
  for (i in seq_along(tt)) expect_is(tt[[i]], "character")
})

test_that("ncbi_downstream does remove some ambiguous taxa", {
  skip_on_cran()
  
  # 590 = "Salmonella"
  ## DOES remove "subsp."
  vcr::use_cassette("ncbi_downstream_ambiguous_false", {
    amb_no <- ncbi_downstream(id = '590', downto = "species", ambiguous = FALSE)
  })

  ## DOES NOT remove "subsp."
  vcr::use_cassette("ncbi_downstream_ambiguous_true", {
    amb_yes <- ncbi_downstream(id = '590', downto = "species", ambiguous = TRUE)
  })

  expect_is(amb_no, "data.frame")
  expect_is(amb_yes, "data.frame")
  expect_gt(NROW(amb_yes), NROW(amb_no))
})

test_that("ncbi_downstream handles when taxa searches return themselves", {
  skip_on_cran() # uses secrets
  # eg.., with `Euchloe` ncbi_downstream was fetching 2 subgenus rank children
  # which return data that had the ids from those subgenera within it
  # fix for https://github.com/ropensci/taxize/issues/807 to remove "self ids"
  # and remove any duplicate records resulting
  vcr::use_cassette("ncbi_downstream_handles_self_ids", {
    tt <- downstream("Euchloe", downto = "species", db = "ncbi",
      rank_filter="genus", messages = FALSE)
  })

  expect_named(tt, "Euchloe")
  expect_is(tt, "downstream")
  expect_is(tt[[1]], "data.frame")
  expect_equal(attr(tt, "db"), "ncbi")
})

test_that("ncbi_downstream doesn't fail on no intermediate data", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("ncbi_downstream_intermediate", {
    tt <- ncbi_downstream(1398485, downto = "no rank", intermediate = TRUE)
  })

  expect_is(tt, "list")
  expect_is(tt$target, "data.frame")
  expect_equal(NROW(tt$target), 0)
  expect_is(tt$intermediate, "list")
  expect_length(tt$intermediate, 0)
})
