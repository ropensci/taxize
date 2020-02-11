context("children")

test_that("children returns the correct values and classes", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("children_with_name", {
    ch_ncbi <- children("Salmo", db = "ncbi")
  })

  expect_is(ch_ncbi, "children")
  expect_equal(attr(ch_ncbi, "db"), "ncbi")
  expect_named(ch_ncbi, "Salmo")
  expect_is(ch_ncbi$Salmo, "data.frame")
  expect_named(ch_ncbi$Salmo, c('childtaxa_id', 'childtaxa_name', 'childtaxa_rank'))
})

test_that("passing in an id works", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("children_with_id", {
    ch_ncbi <- children(8028, db = "ncbi")
    ch_worms <- sw(children(125732, db='worms'))
  })

  expect_is(ch_worms, "children")
  expect_equal(attr(ch_worms, "db"), "worms")
  expect_named(ch_worms, '125732')
  expect_is(ch_worms$`125732`, "data.frame")
  expect_named(ch_worms$`125732`, c('childtaxa_id', 'childtaxa_name', 'childtaxa_rank'))

  expect_is(ch_ncbi, "children")
  expect_equal(attr(ch_ncbi, "db"), "ncbi")
  expect_named(ch_ncbi, '8028')
  expect_is(ch_ncbi$`8028`, "data.frame")
  expect_named(ch_ncbi$`8028`, c('childtaxa_id', 'childtaxa_name', 'childtaxa_rank'))
})

test_that("queries with no results fail well", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("children_no_results", {
    aa <- children(x = "Saurauia", db = "itis", verbose = FALSE)
  })
  expect_equal(NROW(aa[[1]]), 0)
})

test_that("itis types are correct", {
  skip_on_cran()
  itis_expected_col_types <- c(
    parentname = 'character',
    parenttsn  = 'character',
    rankname   = 'character',
    taxonname  = 'character',
    tsn        = 'character'
  )
  
  vcr::use_cassette("children_itis_types", {
    x <- children(1234123434, "itis")
    z <- children(161994, "itis")
  })

  # for no result
  expect_equal(
    sapply(x[['1234123434']], class), itis_expected_col_types 
  )
  # for good result
  expect_equal(
    sapply(z[['161994']], class), itis_expected_col_types 
  )
})

test_that("rows parameter, when used, works", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("children_rows_param", {
    x <- children("Asdfafsfd", db = "ncbi", rows = 1, messages = FALSE)
  })

  expect_is(x, "children")
})

test_that("expected results for no query match when using get_* fxns", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("children_no_results_structure_ncbi_x", {
    ncbi_x <- children(get_uid("23424234234", messages = FALSE))
  })

  vcr::use_cassette("children_no_results_structure_ncbi_y", {
    ncbi_y <- children('dragon', db = "ncbi", messages = FALSE)
  })

  expect_named(ncbi_x, NA_character_)
  expect_named(ncbi_y, "dragon")
})

test_that("expected results for no query match when using get_* fxns", {
  skip_on_cran()
  vcr::use_cassette("children_no_results_structure_x", {
    itis_x <- children(get_tsn("23424234234", messages = FALSE))
    worms_x <- children(get_wormsid("23424234234", messages = FALSE))
  })

  vcr::use_cassette("children_no_results_structure_y", {
    itis_y <- children("23424234234", db = "itis", messages = FALSE)
    worms_y <- children(get_wormsid("23424234234", messages = FALSE))
  })
  
  expect_named(itis_x, NA_character_)
  expect_named(itis_y, '23424234234')

  expect_named(worms_x, NA_character_)
  expect_named(worms_y, NA_character_)
})


test_that("children doesn't remove ambiguous taxa", {
  skip_on_cran()
  vcr::use_cassette("children_ambiguous_ncbi", {
    # 28901 = "Salmonella enterica" - DOES NOT remove "subsp."
    subsp <- children(28901, db = "ncbi")
    # 2508041 = "unclassified Helianthus" - DOES NOT remove "sp."
    sp <- children(2508041, db = "ncbi")
  })

  expect_is(subsp, "children")
  expect_is(subsp[[1]], "data.frame")
  expect_is(sp, "children")
  expect_is(sp[[1]], "data.frame")

  expect_gt(NROW(subsp[[1]]), 3)
  expect_gt(NROW(sp[[1]]), 3)
})

test_that("warn on mismatch 'db'", {
  skip_on_cran()
  vcr::use_cassette("children_warn_on_db_mismatch", {
    expect_warning(
      children(
        get_uid("Chironomus riparius", messages = FALSE), db = "itis"))
  })
})
