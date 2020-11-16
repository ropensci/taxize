skip_on_cran()

fchk_match_any <- function(fun) {
  body <- deparse(functionBody(eval(parse(text = fun))))
  expect_true(any(grepl("fchk", body)))
}

test_that("get_* fxn deprecations", {
  old_funs = c('get_boldid_', 'get_eolid', 'get_eolid_', 'get_gbifid',
    'get_gbifid_', 'get_tsn', 'get_tsn_', 'get_natservid', 'get_natservid_',
    'get_nbnid', 'get_nbnid_', 'get_uid', 'get_uid_', 'get_tolid',
    'get_tolid_', 'get_tpsid', 'get_tpsid_', 'get_wormsid', 'get_wormsid_')
  for (i in old_funs) fchk_match_any(i)
})

test_that("get_* fxn throws warning when old function used", {
  vcr::use_cassette("deprecated_get_fxns", {
    expect_warning(get_uid("howdy", messages=FALSE), "deprecated")
    expect_warning(get_uid("howdy", messages=FALSE), NA)
    expect_warning(get_tsn("howdy", messages=FALSE), "deprecated")
    expect_warning(get_tsn("howdy", messages=FALSE), NA)
    expect_warning(get_boldid("howdy", messages=FALSE), "deprecated")
    expect_warning(get_boldid("howdy", messages=FALSE), NA)
    expect_warning(get_gbifid("howdy", messages=FALSE), "deprecated")
    expect_warning(get_gbifid("howdy", messages=FALSE), NA)
    expect_warning(get_eolid("howdy", messages=FALSE), "deprecated")
    expect_warning(get_eolid("howdy", messages=FALSE), NA)
    expect_warning(get_natservid("howdy", messages=FALSE), "deprecated")
    expect_warning(get_natservid("howdy", messages=FALSE), NA)
    expect_warning(get_nbnid("howdy", messages=FALSE), "deprecated")
    expect_warning(get_nbnid("howdy", messages=FALSE), NA)
    expect_warning(get_tolid("helianthus", messages=FALSE), "deprecated")
    expect_warning(get_tolid("helianthus", messages=FALSE), NA)
    expect_warning(get_tpsid("howdy", messages=FALSE), "deprecated")
    expect_warning(get_tpsid("howdy", messages=FALSE), NA)
    expect_warning(get_wormsid("howdy", messages=FALSE), "deprecated")
    expect_warning(get_wormsid("howdy", messages=FALSE), NA)
  })
})
