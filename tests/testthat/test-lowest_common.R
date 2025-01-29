context("lowest_common")

force_http1_1 <- list(http_version = 2L)

test_that("lowest_common works with ncbi, passing in classifications and doing internally", {
  skip_on_cran()
  skip_on_travis()

  id <- c("9031", "9823", "9606", "9470")
  vcr::use_cassette("classification_lowest_common_ncbi", {
    idc <- classification(id, db = 'ncbi', callopts = force_http1_1)
  })
  
  aa <- lowest_common(id[2:4], db = "ncbi")
  bb <- lowest_common(id[2:4], db = "ncbi", low_rank = 'class')
  cc <- lowest_common(id[2:4], db = "ncbi", class_list = idc, low_rank = 'class')

	expect_is(aa, "data.frame")
  expect_is(bb, "data.frame")
  expect_is(cc, "data.frame")
  expect_named(aa, c('name', 'rank', 'id'))
  expect_named(cc, c('name', 'rank', 'id'))

  expect_identical(aa, bb)
  expect_identical(bb, cc)

  expect_equal(NROW(aa), 1)

  # passing in classification list obs. takes less time
  expect_lt(
    system.time(lowest_common(id[2:4], db = "ncbi", class_list = idc, low_rank = 'class'))[3],
    system.time(lowest_common(id[2:4], db = "ncbi", low_rank = 'class'))[3]
  )
})

test_that("lowest_common works with itis", {
  skip_on_cran()

  #lowest_common(spp, db = "itis")
  #spp <- c("Sus scrofa", "Homo sapiens", "Nycticebus coucang")

  ids <- c("180722","180092","572890")
  vcr::use_cassette("classification_lowest_common_itis", {
    idc <- classification(ids, db = 'itis')
  }, preserve_exact_body_bytes = TRUE, match_requests_on = c("method", "query"))

  expect_identical(
    lowest_common(ids, db = "itis"),
    lowest_common(ids, db = "itis", class_list = idc)
  )

  bb <- lowest_common(ids, db = "itis", low_rank = 'class')
  cc <- lowest_common(ids, db = "itis", class_list = idc, low_rank = 'class')

  expect_is(bb, "data.frame")
  expect_is(cc, "data.frame")
  expect_named(cc, c('name', 'rank', 'id'))

  expect_identical(bb, cc)

  expect_equal(NROW(bb), 1)
})
