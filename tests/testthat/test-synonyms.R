context("synonyms")

test_that("synonyms returns the correct value", {
  vcr::use_cassette("synonyms_itis", {
    tt <- sw(synonyms("Poa annua", db = "itis", rows = 1, verbose = FALSE))
  })

	expect_match(names(tt), "Poa annua")
	expect_match(tt[[1]][1, "syn_name"], "Poa annua var. aquatica")

	expect_is(tt, "synonyms")
	expect_equal(attr(tt, "db"), "itis")
	expect_is(tt[[1]], "data.frame")

	expect_gt(NROW(tt[[1]]), 1)
})

test_that("synonyms works with worms data", {
  vcr::use_cassette("synonyms_worms", {
    tt <- synonyms('Pomatomus saltatrix', db = "worms", verbose = FALSE)
  })

  expect_match(names(tt), 'Pomatomus saltatrix')
  expect_match(tt$`Pomatomus saltatrix`$valid_name[1], 'Pomatomus saltatrix')

  expect_is(tt, "synonyms")
  expect_equal(attr(tt, "db"), "worms")
  expect_is(tt[[1]], "data.frame")
  expect_is(tt[[1]], "tbl_df")
})
