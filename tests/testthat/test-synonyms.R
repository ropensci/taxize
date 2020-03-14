test_that("synonyms returns the correct value", {
  skip_on_cran()
  vcr::use_cassette("synonyms_itis", {
    tt <- sw(synonyms("Poa annua", db = "itis", rows = 1,
      messages = FALSE))
  })

	expect_match(names(tt), "Poa annua")
	expect_match(tt[[1]][1, "syn_name"], "Poa annua var. aquatica")

	expect_is(tt, "synonyms")
	expect_equal(attr(tt, "db"), "itis")
	expect_is(tt[[1]], "data.frame")

	expect_gt(NROW(tt[[1]]), 1)
})

test_that("synonyms works with worms data", {
  skip_on_cran()
  vcr::use_cassette("synonyms_worms", {
    tt <- synonyms('Pomatomus saltatrix', db = "worms",
      messages = FALSE)
  })

  expect_match(names(tt), 'Pomatomus saltatrix')
  expect_match(tt$`Pomatomus saltatrix`$valid_name[1], 'Pomatomus saltatrix')

  expect_is(tt, "synonyms")
  expect_equal(attr(tt, "db"), "worms")
  expect_is(tt[[1]], "data.frame")
  expect_is(tt[[1]], "tbl_df")
})

test_that("synonyms: data sources return consistent outputs", {
  skip_on_cran()
  if (Sys.getenv('IUCN_REDLIST_KEY') == "") {
    skip("No IUCN api key so test not run.")
  }
  if (Sys.getenv('TROPICOS_KEY') == "") {
    skip("No Tropicos api key so test not run.")
  }
  
  # when name not found, returns NA
  vcr::use_cassette("synonyms_name_not_found", {
    aa <- synonyms("Foo bar", db="itis", messages = FALSE)
    bb <- synonyms("Foo bar", db="tropicos", messages = FALSE)
    cc <- synonyms("Foo barasdfasdf", db="nbn", messages = FALSE)
    ee <- synonyms("Foo bar", db="worms", messages = FALSE)
    ff <- synonyms("Foo bar", db="iucn", messages = FALSE)
  })

  for (i in list(aa, bb, cc, ee, ff)) expect_true(is.na(i[[1]]))
  

  # when name found, but no synonyms found, returns empty data.frame
  vcr::use_cassette("synonyms_name_found_but_no_synonyms", {
    gg <- synonyms("Epigonus thai", db="worms", messages = FALSE)
    hh <- synonyms("Ursus arctos", db="nbn", messages = FALSE)
    ii <- synonyms(get_iucn('Ursus americanus'), db="iucn", messages = FALSE)
    kk <- synonyms("Pinus balfouriana", db="itis", messages = FALSE)
    ll <- synonyms("Pinus contorta", db="tropicos", messages = FALSE)
  })
  
  # type is data.frame
  for (i in list(gg, hh, ii, kk, ll)) expect_is(i[[1]], "data.frame")
  # no rows
  for (i in list(gg, hh, ii, kk, ll)) expect_equal(NROW(i[[1]]), 0)
  # no names
  for (i in list(gg, hh, ii, kk, ll)) expect_equal(length(names(i[[1]])), 0)
})

test_that("warn on mismatch 'db'", {
  skip_on_cran()
  vcr::use_cassette("synonyms_warn_on_db_mismatch", {
    expect_warning(
      synonyms(
        get_tsn("Poa annua"), db = "tropicos"))
  })
})
