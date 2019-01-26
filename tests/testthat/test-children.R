context("children")

test_that("children returns the correct values and classes", {
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
  vcr::use_cassette("children_with_id", {
    ch_ncbi <- children(8028, db = "ncbi")
    ch_worms <- sw(children(254966, db='worms'))
  })

  expect_is(ch_worms, "children")
  expect_equal(attr(ch_worms, "db"), "worms")
  expect_named(ch_worms, '254966')
  expect_is(ch_worms$`254966`, "data.frame")
  expect_named(ch_worms$`254966`, c('childtaxa_id', 'childtaxa_name', 'childtaxa_rank'))

  expect_is(ch_ncbi, "children")
  expect_equal(attr(ch_ncbi, "db"), "ncbi")
  expect_named(ch_ncbi, '8028')
  expect_is(ch_ncbi$`8028`, "data.frame")
  expect_named(ch_ncbi$`8028`, c('childtaxa_id', 'childtaxa_name', 'childtaxa_rank'))
})

test_that("queries with no results fail well", {
  vcr::use_cassette("children_no_results", {
    aa <- children(x = "Saurauia", db = "itis", verbose = FALSE)
  })
  expect_equal(NROW(aa[[1]]), 0)
})

test_that("itis types are correct", {
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
  vcr::use_cassette("children_rows_param", {
    x <- children("Asdfafsfd", db = "ncbi", rows = 1, messages = FALSE)
  })

  expect_is(x, "children")
})

test_that("expected results for no query match when using get_* fxns", {
  vcr::use_cassette("children_no_results_structure_x", {
    itis_x <- children(get_tsn(23424234234, messages = FALSE))
    col_x <- children(get_colid(23424234234, messages = FALSE))
    ncbi_x <- children(get_uid(23424234234, messages = FALSE))
    worms_x <- children(get_wormsid(23424234234, messages = FALSE))
  })

  vcr::use_cassette("children_no_results_structure_y", {
    itis_y <- children(23424234234, db = "itis", messages = FALSE)
    col_y <- children('23424234234', db = "col", messages = FALSE)
    ncbi_y <- children('dragon', db = "ncbi", messages = FALSE)
    worms_y <- children(get_wormsid(23424234234, messages = FALSE))
  })
  
  expect_named(itis_x, NA_character_)
  expect_named(itis_y, '23424234234')

  expect_named(col_x, NULL)
  expect_named(col_y, "23424234234")

  expect_named(ncbi_x, NA_character_)
  expect_named(ncbi_y, "dragon")

  expect_named(worms_x, NA_character_)
  expect_named(worms_y, NA_character_)
})
